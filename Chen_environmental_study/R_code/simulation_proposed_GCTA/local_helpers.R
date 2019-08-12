library("sas7bdat")
library("MASS")


##################################################################################
## simulation function with simulated covariate
##################################################################################
simulation_var_est_fn <- function(kernel = GCTA_kernel,
                                  kernel_args = NULL,
                                  kernel_result_col_names,
                                  kernel_2 = NULL,
                                  kernel_args_2 = NULL,
                                  kernel_result_col_names_2 = NULL,
                                  p,
                                  combine = FALSE,
                                  gene_coeff_args = NULL,
                                  generate_data,
                                  gene_data_args,
                                  brep, 
                                  n_sub,
                                  pro,
                                  bs,
                                  rho_e,
                                  emp_n = 10^5,
                                  uncorr_method = NULL,
                                  uncorr_args = NULL,
                                  dim_red_method = NULL,
                                  dim_red_args = NULL,
                                  inter_std = FALSE,
                                  seed_loop = 0,
                                  seed_coeff = 0, 
                                  cores = 1,
                                  inter_result_path = NULL) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  # check some arguments
  if(bs == "leave-1" & n_sub != gene_data_args$n){
    warning("n_sub has to be same as the number of observation")
    n_sub <- gene_data_args$n
  }
  
  # generate coefficients
  if(is.null(gene_coeff_args)){
    stop("you need provide coefficient generating information")
  }
  
  set.seed(seed_coeff)
  # Generate main fixed betas
  betam_fixed <- generate_main(p, gene_coeff_args)
  # Generate interaction fixed gammas
  betai_fixed <- generate_inter(p, gene_coeff_args)

  # Calculate the empirical covariance
  gene_data_args_emp <- gene_data_args
  if(gene_data_args_emp$structure == "I"){
    sigma_main_emp <- diag(p)
    sigma_inter_emp <- 0
    sigma_cov_emp <- 0
    sigma_total_emp <- 0
  } else {
    gene_data_args_emp$n <- emp_n 
    x_emp_raw <- do.call(generate_data, gene_data_args_emp) 
    x_emp <- gene_model_data(x_emp_raw, p, combine = combine)
    sigma_main_emp <- var(x_emp$b_m)
    sigma_inter_emp <- var(x_emp$b_i)
    sigma_cov_emp <- cov(x_emp$b_m, x_emp$b_i)
    sigma_total_emp <- cbind(x_emp$b_m, x_emp$b_i) %>% var(.)
  }
  # check the difference of empirical sigma and true sigma
  # if (gene_data_args_emp$structure == "un"){
  #   if (attributes(x_emp_raw)$x_dist == "chi")
  #     sigma_main_approx <- mean(abs(sigma_main_emp - (gene_data_args_emp$pre_cor[[1]])^2))
  #   else
  #     sigma_main_approx <- mean(abs(sigma_main_emp - gene_data_args_emp$pre_cor[[1]]))
  # } else if (gene_data_args_emp$structure == "I"){
  #     sigma_main_approx <- mean(abs(sigma_main_emp - diag(p)))
  # }
  # Calcualte the effects
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed_loop) %dorng%   {
    # Initial output 
    col_names <- c("var_main_effect","var_inter_effect","cov_main_inter_effect", "var_total_effect")
    kernel_result_col_names <- c(kernel_result_col_names, paste0("sub_", kernel_result_col_names))
    if(!is.null(kernel_result_col_names_2)){
      kernel_result_col_names_2 <- c(kernel_result_col_names_2, paste0("sub_", kernel_result_col_names_2))
    }
    if(combine == TRUE){
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2) %>% gsub(pattern = "_main$", replacement = "_total",x = .)
    } else {
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2)
    }

    result_tmp <- matrix(0, nrow = n_sub, ncol = length(col_names))
    colnames(result_tmp) <- col_names
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_data_args)
    b_gene_model <- gene_model_data(b_raw, p, combine)
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)
    # Sparsity
    sparse_index <- sparsify_coeff(colnames(b_gene_model$b_m), colnames(b_gene_model$b_i))  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    
    # rescale the magnitude of beta's
    if(combine == FALSE){
      betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam # as.numieric is to transform from array to vector
    } else {
      betam <- as.numeric(sqrt(8)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam
      betai <- as.numeric(sqrt(2)/sqrt(t(betai)%*%sigma_inter_emp%*%betai))*betai
      betat <- c(betam,betai)
      betam <- betat[1:length(betam)]
      betai <- betat[-(1:length(betam))]
    }
    
    # Generate the signals
    signalm <- b_gene_model$b_m%*%betam
    if(combine == TRUE) {
      signali <- b_gene_model$b_i%*%betai      
    } else {
      signali <- 0
    }
    
    
    # record all the variance 
    if(combine == FALSE){
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
    } else {
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
      result_tmp[,2] <- t(betai)%*%sigma_inter_emp%*%betai
      result_tmp[,3] <- t(betam)%*%sigma_cov_emp%*%betai
      betat <- c(betam, betai)
      result_tmp[,4] <- t(betat)%*%sigma_total_emp%*%betat  
    }

    # Generate y given fixed random effects
    rho_e <- as.numeric(rho_e)
    sigma_e <- sqrt(10*((1-rho_e)/rho_e))
    y <- signalm+signali+rnorm(length(signalm),sd=sigma_e)
    ## estimating model
    # generate data for esitmating
    b_est_model <- est_model_data(b_raw,
                                  y,
                                  p, 
                                  inter_std, 
                                  combined, 
                                  uncorr_method, 
                                  uncorr_args, 
                                  dim_red_method, 
                                  dim_red_args,
                                  uncorre = kernel_args$decor)
    
    # Call the methods 
    args <- append(b_est_model, kernel_args) %>% append(.,list(b_raw = b_raw, betam = betam, betai = betai))
    result_kernel <- do.call(kernel, args)
    result_tmp[,(5:(4+length(kernel_result_col_names)/2))] <- do.call("rbind", replicate(n_sub, result_kernel, simplify = FALSE))
    
    if(!is.null(kernel_2)){
      args_2 <- append(b_est_model, kernel_args_2) %>% append(.,list(b_raw = b_raw, betam = betam, betai = betai))
      result_kernel <- do.call(kernel_2, args_2)
      result_tmp[,(5+length(kernel_result_col_names)):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2)] <- do.call("rbind", replicate(n_sub, result_kernel, simplify = FALSE))
    }
    
    # generate sub_sampling effects

    for(i in 1:n_sub){
      if(bs == "full"){
        break
      }
      sub_data <- generate_sub(data = list(y = y,b_raw = b_raw), 
                               pro = pro,
                               bs = bs,
                               n = length(y),
                               iteration = i)
      
      b_tmp <- est_model_data(sub_data$b_raw,
                              sub_data$y,
                              p, 
                              inter_std, 
                              combined, 
                              uncorr_method, 
                              uncorr_args, 
                              dim_red_method, 
                              dim_red_args,
                              uncorre = kernel_args$decor)
      args <- append(b_tmp, kernel_args) %>% append(.,list(b_raw = sub_data$b_raw, betam = betam, betai = betai))
      result_tmp[i,(5+length(kernel_result_col_names)/2):(4+length(kernel_result_col_names))] <- do.call(kernel, args)
      if(!is.null(kernel_2)){
        args_2 <- append(b_tmp, kernel_args_2) %>% append(.,list(b_raw = sub_data$b_raw, betam = betam, betai = betai))
        result_tmp[i,(5+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2))] <- do.call(kernel_2, args_2)
      }
    }
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_data_args), 
                                       as.list(uncorr_args), 
                                       gene_coeff_args, 
                                       dim_red_args, 
                                       kernel_args,
                                       kernel_args_2,
                                       list(combine = combine, 
                                            n = nrow(b_raw),
                                            pro = pro,
                                            bs = bs,
                                            n_sub = n_sub,
                                            rho_e = rho_e,
                                            emp_n = emp_n,
                                            inter_std = inter_std)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output

    # save the result
    if(!(is.null(inter_result_path))) data.frame(result_tmp, additional, i = ibrep, row.names = NULL, stringsAsFactors = FALSE) %>% write.csv(., 
                                                                                                  file = paste0(inter_result_path, "rho_e_", rho_e, "_n_", nrow(b_raw), "_pro_", pro,"_iteration_",ibrep,".csv"), 
                                                                                                  row.names = FALSE)
    paste0(ibrep, " is done at ", timestamp())
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  print(result_raw)
}


##################################################################################
## using true sigma to decorrelation
##################################################################################
simulation_var_est_fn_true_sigma <- function(kernel = GCTA_kernel,
                                             kernel_args = NULL,
                                             kernel_result_col_names,
                                             kernel_2 = NULL,
                                             kernel_args_2 = NULL,
                                             kernel_result_col_names_2 = NULL,
                                             p,
                                             combine = FALSE,
                                             gene_coeff_args = NULL,
                                             generate_data,
                                             gene_data_args,
                                             brep, 
                                             n_sub,
                                             pro,
                                             bs,
                                             rho_e,
                                             emp_n = 10^5,
                                             uncorr_method = NULL,
                                             uncorr_args = NULL,
                                             dim_red_method = NULL,
                                             dim_red_args = NULL,
                                             inter_std = FALSE,
                                             seed_loop = 0,
                                             seed_coeff = 0, 
                                             cores = 1,
                                             inter_result_path = NULL) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  # check some arguments
  if(bs == "leave-1" & n_sub != gene_data_args$n){
    warning("n_sub has to be same as the number of observation")
    n_sub <- gene_data_args$n
  }
  
  # generate coefficients
  if(is.null(gene_coeff_args)){
    stop("you need provide coefficient generating information")
  }
  
  set.seed(seed_coeff)
  # Generate main fixed betas
  betam_fixed <- generate_main(p, gene_coeff_args)
  # Generate interaction fixed gammas
  betai_fixed <- generate_inter(p, gene_coeff_args)
  
  # Calculate the empirical covariance
  gene_data_args_emp <- gene_data_args
  if(gene_data_args_emp$structure == "I"){
    sigma_main_emp <- diag(p)
    sigma_inter_emp <- 0
    sigma_cov_emp <- 0
    sigma_total_emp <- 0
  } else {
    gene_data_args_emp$n <- emp_n 
    x_emp_raw <- do.call(generate_data, gene_data_args_emp) 
    x_emp <- gene_model_data(x_emp_raw, p, combine = combine)
    sigma_main_emp <- var(x_emp$b_m)
    sigma_inter_emp <- var(x_emp$b_i)
    sigma_cov_emp <- cov(x_emp$b_m, x_emp$b_i)
    sigma_total_emp <- cbind(x_emp$b_m, x_emp$b_i) %>% var(.)
  }
  
  uncorr_args <- append(uncorr_args, list(Sigma = sigma_main_emp))
    # check the difference of empirical sigma and true sigma
  # if (gene_data_args_emp$structure == "un"){
  #   if (attributes(x_emp_raw)$x_dist == "chi")
  #     sigma_main_approx <- mean(abs(sigma_main_emp - (gene_data_args_emp$pre_cor[[1]])^2))
  #   else
  #     sigma_main_approx <- mean(abs(sigma_main_emp - gene_data_args_emp$pre_cor[[1]]))
  # } else if (gene_data_args_emp$structure == "I"){
  #     sigma_main_approx <- mean(abs(sigma_main_emp - diag(p)))
  # }
  
  # Calcualte the effects
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed_loop) %dorng%   {
    # Initial output 
    col_names <- c("var_main_effect","var_inter_effect","cov_main_inter_effect", "var_total_effect")
    kernel_result_col_names <- c(kernel_result_col_names, paste0("sub_", kernel_result_col_names))
    if(!is.null(kernel_result_col_names_2)){
      kernel_result_col_names_2 <- c(kernel_result_col_names_2, paste0("sub_", kernel_result_col_names_2))
    }
    if(combine == TRUE){
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2) %>% gsub(pattern = "_main$", replacement = "_total",x = .)
    } else {
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2)
    }
    
    result_tmp <- matrix(0, nrow = n_sub, ncol = length(col_names))
    colnames(result_tmp) <- col_names
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_data_args)
    b_gene_model <- gene_model_data(b_raw, p, combine)
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)
    # Sparsity
    sparse_index <- sparsify_coeff(colnames(b_gene_model$b_m), colnames(b_gene_model$b_i))  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    
    # rescale the magnitude of beta's
    if(combine == FALSE){
      betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam # as.numieric is to transform from array to vector
    } else {
      betam <- as.numeric(sqrt(8)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam
      betai <- as.numeric(sqrt(2)/sqrt(t(betai)%*%sigma_inter_emp%*%betai))*betai
      betat <- c(betam,betai)
      betam <- betat[1:length(betam)]
      betai <- betat[-(1:length(betam))]
    }
    
    # Generate the signals
    signalm <- b_gene_model$b_m%*%betam
    if(combine == TRUE) {
      signali <- b_gene_model$b_i%*%betai      
    } else {
      signali <- 0
    }
    
    
    # record all the variance 
    if(combine == FALSE){
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
    } else {
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
      result_tmp[,2] <- t(betai)%*%sigma_inter_emp%*%betai
      result_tmp[,3] <- t(betam)%*%sigma_cov_emp%*%betai
      betat <- c(betam, betai)
      result_tmp[,4] <- t(betat)%*%sigma_total_emp%*%betat  
    }
    
    # Generate y given fixed random effects
    rho_e <- as.numeric(rho_e)
    sigma_e <- sqrt(10*((1-rho_e)/rho_e))
    y <- signalm+signali+rnorm(length(signalm),sd=sigma_e)
    ## estimating model
    # generate data for esitmating
    b_est_model <- est_model_data(b_raw,
                                  y,
                                  p, 
                                  inter_std, 
                                  combined, 
                                  uncorr_method, 
                                  uncorr_args, 
                                  dim_red_method, 
                                  dim_red_args,
                                  uncorre = kernel_args$decor)
    
    # Call the methods 
    args <- append(b_est_model, kernel_args) %>% append(.,list(b_raw = b_raw, betam = betam, betai = betai))
    result_kernel <- do.call(kernel, args)
    result_tmp[,(5:(4+length(kernel_result_col_names)/2))] <- do.call("rbind", replicate(n_sub, result_kernel, simplify = FALSE))
    
    if(!is.null(kernel_2)){
      args_2 <- append(b_est_model, kernel_args_2) %>% append(.,list(b_raw = b_raw, betam = betam, betai = betai))
      result_kernel <- do.call(kernel_2, args_2)
      result_tmp[,(5+length(kernel_result_col_names)):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2)] <- do.call("rbind", replicate(n_sub, result_kernel, simplify = FALSE))
    }
    
    # generate sub_sampling effects
    
    for(i in 1:n_sub){
      if(bs == "full"){
        break
      }
      sub_data <- generate_sub(data = list(y = y,b_raw = b_raw), 
                               pro = pro,
                               bs = bs,
                               n = length(y),
                               iteration = i)
      
      b_tmp <- est_model_data(sub_data$b_raw,
                              sub_data$y,
                              p, 
                              inter_std, 
                              combined, 
                              uncorr_method, 
                              uncorr_args, 
                              dim_red_method, 
                              dim_red_args,
                              uncorre = kernel_args$decor)
      args <- append(b_tmp, kernel_args) %>% append(.,list(b_raw = sub_data$b_raw, betam = betam, betai = betai))
      result_tmp[i,(5+length(kernel_result_col_names)/2):(4+length(kernel_result_col_names))] <- do.call(kernel, args)
      if(!is.null(kernel_2)){
        args_2 <- append(b_tmp, kernel_args_2) %>% append(.,list(b_raw = sub_data$b_raw, betam = betam, betai = betai))
        result_tmp[i,(5+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2))] <- do.call(kernel_2, args_2)
      }
    }
    # combined the all the attributes to b so we could plot them by the attributes
    uncorr_args$Sigma <- NULL
    additional <- list(x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_data_args), 
                                       as.list(uncorr_args), 
                                       gene_coeff_args, 
                                       dim_red_args, 
                                       kernel_args,
                                       kernel_args_2,
                                       list(combine = combine, 
                                            n = nrow(b_raw),
                                            pro = pro,
                                            bs = bs,
                                            n_sub = n_sub,
                                            rho_e = rho_e,
                                            emp_n = emp_n,
                                            inter_std = inter_std)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output
    
    # save the result
    if(!(is.null(inter_result_path))) data.frame(result_tmp, additional, i = ibrep, row.names = NULL, stringsAsFactors = FALSE) %>% write.csv(., 
                                                                                                                                              file = paste0(inter_result_path, "rho_e_", rho_e, "_n_", nrow(b_raw), "_pro_", pro,"_iteration_",ibrep,".csv"), 
                                                                                                                                              row.names = FALSE)
    paste0(ibrep, " is done at ", timestamp())
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  print(result_raw)
}

##################################################################################
## simulation function with simulated covariate
##################################################################################
simulation_var_est_lm_fn <- function(kernel = GCTA_kernel,
                                  kernel_args = NULL,
                                  kernel_result_col_names,
                                  kernel_2 = NULL,
                                  kernel_args_2 = NULL,
                                  kernel_result_col_names_2 = NULL,
                                  p,
                                  combine = FALSE,
                                  gene_coeff_args = NULL,
                                  generate_data,
                                  gene_data_args,
                                  brep, 
                                  n_sub,
                                  pro,
                                  bs,
                                  rho_e,
                                  emp_n = 10^5,
                                  uncorr_method = NULL,
                                  uncorr_args = NULL,
                                  dim_red_method = NULL,
                                  dim_red_args = NULL,
                                  inter_std = FALSE,
                                  sparse_ratio,
                                  seed_loop = 0,
                                  seed_coeff = 0, 
                                  cores = 1,
                                  inter_result_path = NULL) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  # check some arguments
  if(bs == "leave-1" & n_sub != gene_data_args$n){
    warning("n_sub has to be same as the number of observation")
    n_sub <- gene_data_args$n
  }
  # generate coefficients
  if(is.null(gene_coeff_args)){
    stop("you need provide coefficient generating information")
  }
  
  set.seed(seed_coeff)
  # Generate main fixed betas
  betam_fixed <- generate_main(p, gene_coeff_args)
  # Generate interaction fixed gammas
  betai_fixed <- generate_inter(p, gene_coeff_args)
  
  # Calculate the empirical covariance
  gene_data_args_emp <- gene_data_args
  if(gene_data_args_emp$structure == "I"){
    sigma_main_emp <- diag(p)
    sigma_inter_emp <- 0
    sigma_cov_emp <- 0
    sigma_total_emp <- 0
  } else {
    gene_data_args_emp$n <- emp_n
    x_emp_raw <- do.call(generate_data, gene_data_args_emp) 
    x_emp <- gene_model_data(x_emp_raw, p, combine = combine)
    sigma_main_emp <- var(x_emp$b_m)
    sigma_inter_emp <- var(x_emp$b_i)
    sigma_cov_emp <- cov(x_emp$b_m, x_emp$b_i)
    sigma_total_emp <- cbind(x_emp$b_m, x_emp$b_i) %>% var(.)
  }
  # check the difference of empirical sigma and true sigma
  # if (gene_data_args_emp$structure == "un"){
  #   if (attributes(x_emp_raw)$x_dist == "chi")
  #     sigma_main_approx <- mean(abs(sigma_main_emp - (gene_data_args_emp$pre_cor[[1]])^2))
  #   else 
  #     sigma_main_approx <- mean(abs(sigma_main_emp - gene_data_args_emp$pre_cor[[1]]))  
  # } else if (gene_data_args_emp$structure == "I"){
  #     sigma_main_approx <- mean(abs(sigma_main_emp - diag(p)))
  # }
  
  # Calcualte the effects
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed_loop) %dorng%   {
    # Initial output 
    
    col_names <- c("var_main_effect","var_inter_effect","cov_main_inter_effect", "var_total_effect")
    kernel_result_col_names <- c(kernel_result_col_names, paste0("sub_", kernel_result_col_names))
    if(!is.null(kernel_result_col_names_2)){
      kernel_result_col_names_2 <- c(kernel_result_col_names_2, paste0("sub_", kernel_result_col_names_2))
    }
    if(combine == TRUE){
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2) %>% gsub(pattern = "_main$", replacement = "_total",x = .)
    } else {
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2)
    }
    result_tmp <- matrix(0, nrow = n_sub, ncol = length(col_names))
    colnames(result_tmp) <- col_names
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_data_args)
    b_gene_model <- gene_model_data(b_raw, p, combine)
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)
    # Sparsity
    sparse_index <- sparsify_coeff(colnames(b_gene_model$b_m), colnames(b_gene_model$b_i),sparse_ratio = sparse_ratio)  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    # rescale the magnitude of beta's
    if(combine == FALSE){
      betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam # as.numieric is to transform from array to vector
    } else {
      betam <- as.numeric(sqrt(8)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam
      betai <- as.numeric(sqrt(2)/sqrt(t(betai)%*%sigma_inter_emp%*%betai))*betai
      betat <- c(betam,betai)
      betam <- betat[1:length(betam)]
      betai <- betat[-(1:length(betam))]
    }
    
    # Generate the signals
    signalm <- b_gene_model$b_m%*%betam
    if(combine == TRUE) {
      signali <- b_gene_model$b_i%*%betai      
    } else {
      signali <- 0
    }
    
    
    # record all the variance 
    if(combine == FALSE){
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
    } else {
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
      result_tmp[,2] <- t(betai)%*%sigma_inter_emp%*%betai
      result_tmp[,3] <- t(betam)%*%sigma_cov_emp%*%betai
      betat <- c(betam, betai)
      result_tmp[,4] <- t(betat)%*%sigma_total_emp%*%betat  
    }
    
    # Generate y given fixed random effects
    rho_e <- as.numeric(rho_e)
    sigma_e <- sqrt(10*((1-rho_e)/rho_e))
    y <- signalm+signali+rnorm(length(signalm),sd=sigma_e)

    ## estimating model

    # generate sub_sampling effects
    b_tmp <- list(s_final = NA, y = NA)
    for(i in 1:n_sub){
      index <- sample(1:nrow(b_raw), as.numeric(round(nrow(b_raw)*pro,0)),replace = FALSE)
      x_r <- pvalue_dim_reduction(x = b_raw, y = y, index = index, method = "lm")
      b_tmp$s_final <- x_r %>% SVD_method(.) %>% "[["(.,1) %>% std_fn(.)
      b_tmp$y <- y[index, ,drop = FALSE]
      # make estimation 
      args <- append(b_tmp, kernel_args)
      result_tmp[i,(5+length(kernel_result_col_names)/2):(4+length(kernel_result_col_names))] <- do.call(kernel, args)
      if(!is.null(kernel_2)){
        args_2 <- append(b_tmp, kernel_args_2)
        result_tmp[i,(5+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2))] <- do.call(kernel_2, args_2)
      }
    }
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_data_args), 
                                       as.list(uncorr_args), 
                                       gene_coeff_args, 
                                       dim_red_args, 
                                       kernel_args,
                                       kernel_args_2,
                                       list(combine = combine, 
                                            n = nrow(b_raw),
                                            pro = pro,
                                            bs = bs,
                                            n_sub = n_sub,
                                            rho_e = rho_e,
                                            emp_n = emp_n,
                                            inter_std = inter_std)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output
    
    # save the result
    if(!(is.null(inter_result_path))) data.frame(result_tmp, additional, i = ibrep, row.names = NULL, stringsAsFactors = FALSE) %>% write.csv(., 
                                                                                                                                              file = paste0(inter_result_path, "rho_e_", rho_e, "_n_", nrow(b_raw), "_pro_", pro,"_iteration_",ibrep,".csv"), 
                                                                                                                                              row.names = FALSE)
    paste0(ibrep, " is done at ", timestamp())
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  print(result_raw)
}



simulation_var_est_fn_decor_before <- function(kernel = GCTA_kernel,
                                  kernel_args = NULL,
                                  kernel_result_col_names,
                                  kernel_2 = NULL,
                                  kernel_args_2 = NULL,
                                  kernel_result_col_names_2 = NULL,
                                  p,
                                  combine = FALSE,
                                  gene_coeff_args = NULL,
                                  generate_data,
                                  gene_data_args,
                                  brep, 
                                  n_sub,
                                  pro,
                                  bs,
                                  rho_e,
                                  emp_n = 10^5,
                                  uncorr_method = NULL,
                                  uncorr_args = NULL,
                                  dim_red_method = NULL,
                                  dim_red_args = NULL,
                                  inter_std = FALSE,
                                  seed_loop = 0,
                                  seed_coeff = 0, 
                                  cores = 1,
                                  inter_result_path = NULL) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  # check some arguments
  if(bs == "leave-1" & n_sub != gene_data_args$n){
    warning("n_sub has to be same as the number of observation")
    n_sub <- gene_data_args$n
  }
  # 
  # generate coefficients
  if(is.null(gene_coeff_args)){
    stop("you need provide coefficient generating information")
  }
  
  set.seed(seed_coeff)
  # Generate main fixed betas
  betam_fixed <- generate_main(p, gene_coeff_args)
  # Generate interaction fixed gammas
  betai_fixed <- generate_inter(p, gene_coeff_args)
  
  # Calculate the empirical covariance
  gene_data_args_emp <- gene_data_args
  if(gene_data_args_emp$structure == "I"){
    sigma_main_emp <- diag(p)
    sigma_inter_emp <- 0
    sigma_cov_emp <- 0
    sigma_total_emp <- 0
  } else {
    gene_data_args_emp$n <- emp_n
    x_emp_raw <- do.call(generate_data, gene_data_args_emp) 
    x_emp <- gene_model_data(x_emp_raw, p, combine = combine)
    sigma_main_emp <- var(x_emp$b_m)
    sigma_inter_emp <- var(x_emp$b_i)
    sigma_cov_emp <- cov(x_emp$b_m, x_emp$b_i)
    sigma_total_emp <- cbind(x_emp$b_m, x_emp$b_i) %>% var(.)
  }
  # check the difference of empirical sigma and true sigma
  # if (gene_data_args_emp$structure == "un"){
  #   if (attributes(x_emp_raw)$x_dist == "chi")
  #     sigma_main_approx <- mean(abs(sigma_main_emp - (gene_data_args_emp$pre_cor[[1]])^2))
  #   else
  #     sigma_main_approx <- mean(abs(sigma_main_emp - gene_data_args_emp$pre_cor[[1]]))
  # } else if (gene_data_args_emp$structure == "I"){
  #     sigma_main_approx <- mean(abs(sigma_main_emp - diag(p)))
  # }
  
  # Calcualte the effects
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed_loop) %dorng%   {
    # Initial output 
    col_names <- c("var_main_effect","var_inter_effect","cov_main_inter_effect", "var_total_effect")
    kernel_result_col_names <- c(kernel_result_col_names, paste0("sub_", kernel_result_col_names))
    if(!is.null(kernel_result_col_names_2)){
      kernel_result_col_names_2 <- c(kernel_result_col_names_2, paste0("sub_", kernel_result_col_names_2))
    }
    if(combine == TRUE){
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2) %>% gsub(pattern = "_main$", replacement = "_total",x = .)
    } else {
      col_names <- c(col_names, kernel_result_col_names,kernel_result_col_names_2)
    }
    
    result_tmp <- matrix(0, nrow = n_sub, ncol = length(col_names))
    colnames(result_tmp) <- col_names
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_data_args)
    b_gene_model <- gene_model_data(b_raw, p, combine)
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)
    # Sparsity
    sparse_index <- sparsify_coeff(colnames(b_gene_model$b_m), colnames(b_gene_model$b_i))  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    
    # rescale the magnitude of beta's
    if(combine == FALSE){
      betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam # as.numieric is to transform from array to vector
    } else {
      betam <- as.numeric(sqrt(8)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam
      betai <- as.numeric(sqrt(2)/sqrt(t(betai)%*%sigma_inter_emp%*%betai))*betai
      betat <- c(betam,betai)
      betam <- betat[1:length(betam)]
      betai <- betat[-(1:length(betam))]
    }
    
    # Generate the signals
    signalm <- b_gene_model$b_m%*%betam
    if(combine == TRUE) {
      signali <- b_gene_model$b_i%*%betai      
    } else {
      signali <- 0
    }
    
    
    # record all the variance 
    if(combine == FALSE){
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
    } else {
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
      result_tmp[,2] <- t(betai)%*%sigma_inter_emp%*%betai
      result_tmp[,3] <- t(betam)%*%sigma_cov_emp%*%betai
      betat <- c(betam, betai)
      result_tmp[,4] <- t(betat)%*%sigma_total_emp%*%betat  
    }
    
    # Generate y given fixed random effects
    rho_e <- as.numeric(rho_e)
    sigma_e <- sqrt(10*((1-rho_e)/rho_e))
    y <- signalm+signali+rnorm(length(signalm),sd=sigma_e)
    ## estimating model
    # generate data for esitmating
    b_est_model <- est_model_data(b_raw,
                                  y,
                                  p, 
                                  inter_std, 
                                  combined, 
                                  uncorr_method, 
                                  uncorr_args, 
                                  dim_red_method, 
                                  dim_red_args,
                                  uncorre = kernel_args$decor)
    
    # Call the methods 
    args <- append(b_est_model, kernel_args) %>% append(.,list(b_raw = b_raw, betam = betam, betai = betai))
    result_kernel <- do.call(kernel, args)
    result_tmp[,(5:(4+length(kernel_result_col_names)/2))] <- do.call("rbind", replicate(n_sub, result_kernel, simplify = FALSE))
    
    if(!is.null(kernel_2)){
      args_2 <- append(b_est_model, kernel_args_2) %>% append(.,list(b_raw = b_raw, betam = betam, betai = betai))
      result_kernel <- do.call(kernel_2, args_2)
      result_tmp[,(5+length(kernel_result_col_names)):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2)] <- do.call("rbind", replicate(n_sub, result_kernel, simplify = FALSE))
    }
    
    # generate sub_sampling effects
    for(i in 1:n_sub){
      b_tmp <- generate_sub(data = b_est_model, 
                               pro = pro,
                               bs = bs,
                               n = length(y),
                               iteration = i)
      args <- append(b_tmp, kernel_args)
      result_tmp[i,(5+length(kernel_result_col_names)/2):(4+length(kernel_result_col_names))] <- do.call(kernel, args)
      if(!is.null(kernel_2)){
        args_2 <- append(b_tmp, kernel_args_2)
        result_tmp[i,(5+length(kernel_result_col_names)+length(kernel_result_col_names_2)/2):(4+length(kernel_result_col_names)+length(kernel_result_col_names_2))] <- do.call(kernel_2, args_2)
      }
    }
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_data_args), 
                                       as.list(uncorr_args), 
                                       gene_coeff_args, 
                                       dim_red_args, 
                                       kernel_args,
                                       kernel_args_2,
                                       list(combine = combine, 
                                            n = nrow(b_raw),
                                            pro = pro,
                                            bs = bs,
                                            n_sub = n_sub,
                                            rho_e = rho_e,
                                            emp_n = emp_n,
                                            inter_std = inter_std)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output
    
    # save the result
    if(!(is.null(inter_result_path))) data.frame(result_tmp, additional, i = ibrep, row.names = NULL, stringsAsFactors = FALSE) %>% write.csv(., 
                                                                                                                                              file = paste0(inter_result_path, "rho_e_", rho_e, "_n_", nrow(b_raw), "_pro_", pro,"_iteration_",ibrep,".csv"), 
                                                                                                                                              row.names = FALSE)
    paste0(ibrep, " is done at ", timestamp())
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  print(result_raw)
}

