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
                                  c_betai = 2,
                                  c_betam = 8,
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
                                  tran_fn = null_tran,
                                  historical_minic = TRUE,
                                  uncorr_method = NULL,
                                  uncorr_args = NULL,
                                  sparse_uncorr_method = NULL,
                                  sparse_uncorr_args = NULL,
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
  x_emp <- gene_model_data(x_emp_raw, p, combine = combine)
  sigma_main_emp <- var(x_emp$b_m)
  sigma_inter_emp <- var(x_emp$b_i)
  sigma_cov_emp <- cov(x_emp$b_m, x_emp$b_i)
  sigma_total_emp <- cbind(x_emp$b_m, x_emp$b_i) %>% var(.)
  
  # Minic the historical data noise
  if(historical_minic == TRUE){
    x_emp_h <- x_emp_raw %>% tran_fn(.) %>% gene_model_data(., p, combine = combine)
    sigma_main_emp_h <- x_emp_h$b_m %>% var(.) 
    sigma_total_emp_h <- cbind(x_emp_h$b_m,x_emp_h$b_i)  %>% var(.) 
  } else {
    sigma_main_emp_h <- sigma_main_emp
    sigma_total_emp_h <- sigma_total_emp
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
    b_gene_model <- gene_model_data(b_raw, p, combine) # standardization and adding interaction term if needed
    
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Generate interaction gammas
    betai <- betai_fixed + generate_inter_random(p, gene_coeff_args)
    
    # Sparsity defualt is 50%
    sparse_index <- sparsify_coeff(colnames(b_gene_model$b_m), colnames(b_gene_model$b_i))  
    betam[sparse_index$index_main] <- 0
    betai[sparse_index$index_inter] <- 0
    
    # rescale the magnitude of beta's: beta^T * Var(x) * beta = 10
    if(combine == FALSE){
      betam <- as.numeric(sqrt(c_betam)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam # as.numieric is to transform from array to vector
    } else {
      betam <- as.numeric(sqrt(c_betam)/sqrt(t(betam)%*%sigma_main_emp%*%betam))*betam
      betai <- as.numeric(sqrt(c_betai)/sqrt(t(betai)%*%sigma_inter_emp%*%betai))*betai
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
      total_signal  <- result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
    } else {
      result_tmp[,1] <- t(betam)%*%sigma_main_emp%*%betam
      result_tmp[,2] <- t(betai)%*%sigma_inter_emp%*%betai
      result_tmp[,3] <- t(betam)%*%sigma_cov_emp%*%betai
      total_signal  <- result_tmp[,4] <- t(betat)%*%sigma_total_emp%*%betat  
    }

    # Set the heritability ratio
    rho_e <- as.numeric(rho_e)
    sigma_e <- sqrt(total_signal*((1-rho_e)/rho_e))
    
    # Generate y
    y <- signalm+signali+rnorm(length(signalm),sd=sigma_e)

    ## estimating model
    # generate data for esitmating
    if(!is.null(uncorr_args$emp)) {
      uncorr_args$sigma_main_emp <- sigma_main_emp_h
      uncorr_args$sigma_total_emp <- sigma_total_emp_h  
    }
    
    b_est_model <- est_model_data(b_raw,
                                  y,
                                  p, 
                                  combined, 
                                  uncorr_method, 
                                  uncorr_args, 
                                  sparse_uncorr_method,
                                  sparse_uncorr_args,
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
    
    # generate sub_sampling for variance estimation
    # sub-sampling procedure includes: data standardization, decorrelation, dimension reduction
    
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
                              combined, 
                              uncorr_method, 
                              uncorr_args, 
                              sparse_uncorr_method,
                              sparse_uncorr_args,
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
    additional$sigma_total_emp <- NULL
    additional$sigma_main_emp <- NULL
    additional$data_path <- NULL
    
    # save the result
    if(!(is.null(inter_result_path))) data.frame(result_tmp, additional, i = ibrep, row.names = NULL, stringsAsFactors = FALSE) %>% write.csv(., 
                                                                                                  file = paste0(inter_result_path, "rho_e_", rho_e, "_n_", nrow(b_raw), "_pro_", pro,"_iteration_",ibrep,".csv"), 
                                                                                                  row.names = FALSE)
    paste0(ibrep, " is done at ", timestamp())
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  print(result_raw)
}


