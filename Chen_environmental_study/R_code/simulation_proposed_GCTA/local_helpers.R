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
                          main_pro = NULL,
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
      fit=Yang(y,b_final,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
      # Call the GCTA method
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


