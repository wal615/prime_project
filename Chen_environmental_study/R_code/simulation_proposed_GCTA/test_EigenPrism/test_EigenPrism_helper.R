gene_model_data_test <- function(b_raw, p){
  # Standardized main covariates
  b <- b_raw %>% std_fn(.)
  b_m <- b[,1:p, drop = FALSE]
  list(b_m = b_m)
}

est_model_data_test <- function(b_raw, y, p, uncorr_method, uncorr_args, dim_red_method, dim_red_args){
  # Standardized main covariates
  b_final <- b_raw %>% std_fn(.)
  
  # Uncorrelated data
  # s_final <- uncorr_fn(b_final, uncorr_method, uncorr_args, dim_red_method, dim_red_args)
  
  list(b_final = b_final,
       # s_final = s_final,
       y = y)
}

simulation_var_est_fn <- function(kernel = GCTA_kernel,
                                  kernel_args = NULL,
                                  kernel_result_col_names,
                                  combine = FALSE,
                                  gene_coeff_args = NULL,
                                  generate_data,
                                  gene_data_args,
                                  brep, 
                                  rho_e,
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
  
  # generate coefficients
  if(is.null(gene_coeff_args)){
    stop("you need provide coefficient generating information")
  }
  
  set.seed(seed_coeff)
  # Generate main fixed betas
  p <- gene_data_args$p
  betam_fixed <- generate_main(p, gene_coeff_args)
  # Calcualte the effects
  
  # Initial output 
  col_names <- c("var_main_effect")
  if(combine == TRUE){
    col_names <- c(col_names, kernel_result_col_names) %>% gsub(pattern = "_main$", replacement = "_total",x = .)
  } else {
    col_names <- c(col_names, kernel_result_col_names)
  }
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed_loop) %dorng%   {
    result_tmp <- matrix(0, nrow = 1, ncol = length(col_names))
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_data_args)
    b_gene_model <- gene_model_data_test(b_raw, p)
    
    # Generate main betas
    betam <- betam_fixed + generate_main_random(p, gene_coeff_args)
    # Sparsity
    sparse_index <- seq(1,p,2)
    betam[sparse_index] <- 0
    
    # rescale the magnitude of beta's
    sigma_main <- diag(length(betam))
    betam <- as.numeric(sqrt(p)/sqrt(t(betam)%*%sigma_main%*%betam))*betam # as.numieric is to transform from array to vector
    
    # Generate the signals
    signalm <- b_gene_model$b_m%*%betam

    # record all the variance 
    result_tmp[,1] <- t(betam)%*%sigma_main%*%betam

    # Generate y given fixed random effects
    sigma_e <- sqrt(p*((1-rho_e)/rho_e))
    y <- signalm+rnorm(length(signalm),sd=sigma_e)
    ## estimating model
    # generate data for esitmating
    b_est_model <- est_model_data_test(b_raw = b_raw, 
                                       y = y,
                                       p = p, 
                                       uncorr_method = uncorr_method, 
                                       uncorr_args = uncorr_args, 
                                       dim_red_method = dim_red_method, 
                                       dim_red_args = dim_red_args)
    
    # Call the original GCTA method 
    args <- append(b_est_model, kernel_args) 
    result_tmp[,(2:(1+length(kernel_result_col_names)))] <- do.call(kernel, args)
    result_tmp
  }
  
  
  # combined the all the attributes to b so we could plot them by the attributes
  additional <- list()
  additional <- append(additional, c(as.list(gene_data_args), 
                                     as.list(uncorr_args), 
                                     gene_coeff_args, 
                                     dim_red_args, 
                                     kernel_args,
                                     list(combine = combine, 
                                          n = n,
                                          rho_e = rho_e,
                                          inter_std = inter_std)))
  additional <- additional[unique(names(additional))] # remove duplicated attrs
  additional$pre_cor <- NULL # pre_cor is a covariance matrix so don't need to carry it to the output
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  colnames(result_raw) <- col_names
  
  # save the result
  data.frame(result_raw, additional)
}