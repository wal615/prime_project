library("sas7bdat")
library("MASS")

##################################################################################
## simulation function with simulated covariate
##################################################################################
simulation_fn <- function(p,
                          tran_fun,
                          combine = FALSE,
                          gene_coeff_args = NULL,
                          generate_data,
                          gene_data_args,
                          brep, 
                          nrep,
                          uncorr_method = NULL,
                          uncorr_args = NULL,
                          dim_red_method = NULL,
                          dim_red_args = NULL,
                          interaction_m = 0, 
                          corrected_main = FALSE,
                          inter_std = FALSE,
                          seed = 0, 
                          cores = 1) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  
  # generate coefficients
  if(is.null(gene_coeff_args)){
    stop("you need provide coefficient generating information")
  }
  
  set.seed(seed) # set seed for main coefficient
  # Generate main fixed betas
  betam_fixed <- generate_main(p, gene_coeff_args)
  # Generate interaction fixed gammas
  betai_fixed <- generate_inter(p, gene_coeff_args)

  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed) %dorng%   {
    # Initial output 
    result_tmp <- matrix(0, nrow = nrep, ncol = 9)
    
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_data_args)

    # Standardized main covariates
    b <- b_raw %>% std_fn(.) %>% add_inter(.)
    b_m <- b[,1:p]
    b_i <- b[,-(1:p)]
    
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)

    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)

    # Sparsity
    sparse_index <- sparsify_coeff(colnames(b_m), colnames(b_i))  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    
    
    # Generate the signals
    signalm <- b_m%*%betam
    signali <- b_i%*%betai
    result_tmp[,2] <- var(signali)
    
    # record all the variance 
    result_tmp[,7] <- var(signalm)
    result_tmp[,8] <- var(signali)
    result_tmp[,9] <- 2*cov(signali,signalm)
    # center the main/interaction terms
    if(inter_std == TRUE)
      b_i <- std_fn(b = b_i)
    
    if(combine == TRUE){
      result_tmp[, 1]=var(signalm + signali)
      b_final <- cbind(b_m, b_i)
    } else {
      result_tmp[, 1]=var(signalm)
      b_final <- b_m
    }
    
    # Uncorrelated data
    if(corrected_main == TRUE){
      x<- uncorr_fn(cbind(b_m, b_i), uncorr_method, uncorr_args, dim_red_method, dim_red_args)
    } else {
      x <- uncorr_fn(b_final, uncorr_method, uncorr_args, dim_red_method, dim_red_args)
    }
    
    # Estimating effects with iterations to reduce the variance
    for(irep in 1:nrep){
      # Generate health outcome given fixed random effects
      y=signalm+signali+rnorm(length(signalm),sd=4)
      
      # Call the original GCTA method 
      fit=Yang(y,b_final,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
      # Call the proposed GCTA method
      fit=Yang(y,x,interact = interaction_m)
      result_tmp[irep,5] <- fit$G
      result_tmp[irep,6] <- fit$RACT
    }
    
    
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_data_args), 
                                       as.list(uncorr_args), 
                                       gene_coeff_args, 
                                       dim_red_args, 
                                       list(interaction_m = interaction_m, 
                                            combine = combine, 
                                            n = nrow(b_raw),
                                            inter_std = inter_std)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output
    
    # save the result
    result_final <- rbind(apply(result_tmp, 2, mean), apply(result_tmp, 2,sd))
    result_final <- data.frame(result_final, additional) ## adding attributes as plot categories
    
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  
  colnames(result_raw)[7:9] <- c("var_main_effect","var_inter_effect","cov_main_inter_effect")
  colnames(result_raw)[1:6] <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "prop_main", "prop_interaction")
  if(combine == TRUE){
    colnames(result_raw)[1:6] <- c("true_total", "true_interaction", "GCTA_total", "GCTA_interaction", "prop_total", "prop_interaction")
  }
  result_raw
}


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
  gene_data_args_emp$n <- emp_n
  x_emp_raw <- do.call(generate_data, gene_data_args_emp) 
  x_emp <- gene_model_data(x_emp_raw, p)
  sigma_main_emp <- var(x_emp$b_m)
  sigma_inter_emp <- var(x_emp$b_i)
  sigma_cov_emp <- cov(x_emp$b_m, x_emp$b_i)
  sigma_total_emp <- cbind(x_emp$b_m, x_emp$b_i) %>% var(.)
  
  # check the difference of empirical sigma and true sigma
  if (gene_data_args_emp$structure == "un"){
    if (attributes(x_emp_raw)$x_dist == "chi")
      sigma_main_approx <- mean(abs(sigma_main_emp - (gene_data_args_emp$pre_cor[[1]])^2))
    else 
      sigma_main_approx <- mean(abs(sigma_main_emp - gene_data_args_emp$pre_cor[[1]]))  
  } else if (gene_data_args_emp$structure == "I"){
      sigma_main_approx <- mean(abs(sigma_main_emp - diag(p)))
  }

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
    b_gene_model <- gene_model_data(b_raw, p)
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)
    # Sparsity
    sparse_index <- sparsify_coeff(colnames(b_gene_model$b_m), colnames(b_gene_model$b_i))  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    
    # rescale the magnitude of beta's
      sigma_main_x <- var(b_gene_model$b_m)
      sigma_inter_x <- var(b_gene_model$b_i)
      sigma_total_x <- var(cbind(b_gene_model$b_m,b_gene_model$b_i))
      if(combine == FALSE){
        betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma_main_x%*%betam))*betam # as.numieric is to transform from array to vector
      } else {
        betam <- as.numeric(sqrt(8)/sqrt(t(betam)%*%sigma_main_x%*%betam))*betam
        betai <- as.numeric(sqrt(2)/sqrt(t(betai)%*%sigma_inter_x%*%betai))*betai
        betat <- c(betam,betai)
        betat <- as.numeric(sqrt(10)/sqrt(t(betat)%*%sigma_total_x%*%betat))*betat
        betam <- betat[1:length(betam)]
        betai <- betat[-(1:length(betam))]
      }
    
    # Generate the signals
    signalm <- b_gene_model$b_m%*%betam
    signali <- b_gene_model$b_i%*%betai    
      
    # record all the variance 
    result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
    result_tmp[,2] <- t(betai)%*%sigma_inter_emp%*%betai
    result_tmp[,3] <- t(betam)%*%sigma_cov_emp%*%betai
    betat <- c(betam, betai)
    result_tmp[,4] <- t(betat)%*%sigma_total_emp%*%betat
    
    # Generate y given fixed random effects
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
                                  dim_red_args)
    
    # Call the original GCTA method 
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
      sub_data <- generate_sub(data = list(y = y,
                                           b_raw = b_raw), 
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
                              dim_red_args)
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
                                            n_sub = n_sub,
                                            rho_e = rho_e,
                                            emp_n = emp_n,
                                            inter_std = inter_std,
                                            sigma_main_approx = sigma_main_approx)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output

    # save the result
    if(!(is.null(inter_result_path))) data.frame(result_tmp, additional, i = ibrep) %>% write.csv(., file = paste0(inter_result_path,"sub_sampling_",ibrep,".csv"), row.names = FALSE)
    paste0(ibrep, " is done at ", timestamp())
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  print(result_raw)
}



#######################################################################################
## real data analysis
#######################################################################################

fit_real_data_fn <- function( combine = FALSE,
                              generate_data,
                              gene_data_args,
                              brep, 
                              uncorr_method = NULL,
                              uncorr_args = NULL,
                              dim_red_method = NULL,
                              dim_red_args = NULL,
                              interaction_m = 0, 
                              inter_std = FALSE,
                              seed = 0, 
                              cores = 1) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed) %dorng%   {
    # Initial output 
    result_tmp <- matrix(0, nrow = 1, ncol = 5)
    
    # Generate covariates  
    input_data <- do.call(generate_data, gene_data_args)
    b_raw <- input_data$x
    p <- ncol(b_raw)
    
    # Standardized main covariates
    b <- b_raw %>% std_fn(.) %>% add_inter(.)
    b_m <- b[,1:p]
    b_i <- b[,-(1:p)]
    
    # center the main/interaction terms
    if(inter_std == TRUE)
      b_i <- std_fn(b = b_i)
    
    if(combine == TRUE){
      b_final <- cbind(b_m, b_i)
    } else {
      b_final <- b_m
    }
    
    # Uncorrelated data
    x <- uncorr_fn(b_final, uncorr_method, uncorr_args, dim_red_method, dim_red_args)
    
    # Generate health outcome given fixed random effects
    y <- input_data$y %>% std_fn(.)
    result_tmp[1,1] <- var(y)
    
    # Call the original GCTA method 
    fit=Yang(y,b_final,interact = interaction_m)
    result_tmp[1,2] <- fit$G
    result_tmp[1,3] <- fit$RACT
    
    # Call the proposed GCTA method
    fit=Yang(y,x,interact = interaction_m)
    result_tmp[1,4] <- fit$G
    result_tmp[1,5] <- fit$RACT
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(x_dist = attributes(b_raw)$x_dist)
    
    additional <- append(additional, c(as.list(gene_data_args), 
                                       as.list(uncorr_args), 
                                       dim_red_args, 
                                       list(interaction_m = interaction_m, 
                                            combine = combine, 
                                            n = nrow(b_raw),
                                            inter_std = inter_std)))
    additional <- additional[unique(names(additional))] # remove duplicated attrs
    additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output
    additional$tran_fn_y <- names(additional$tran_fn_y)
    additional$tran_fn_x <- names(additional$tran_fn_x)
    
    # save the result
    result_final <- data.frame(result_tmp, additional) ## adding attributes as plot categories
    
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  
  colnames(result_raw)[1:5] <- c("y_var",  "GCTA_main", "GCTA_interaction", "prop_main", "prop_interaction")
  if(combine == TRUE){
    colnames(result_raw)[1:5] <- c("y_var", "GCTA_total", "GCTA_interaction", "prop_total", "prop_interaction")
  }
  result_raw
}

