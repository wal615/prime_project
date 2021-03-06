##################################################################################
## generate main effect
##################################################################################
generate_main <- function(p) {
  betam=rnorm(p, mean =0, sd =0.5) # main_effect ~ N(0,0.5)
  betam[2*c(1:floor(p/2))]=0  # mimic the zero coefficients 
  betam
}

##################################################################################
## generate main effect
##################################################################################
generate_inter <- function(p, interaction) {
  if(interaction==0) {
    betai <- 0
  } else {
    betai <- matrix(rnorm(p*p,m=0,sd=0.1),ncol=p) # interaction_effect ~ N(0,0.1)
    betai[lower.tri(betai, diag = TRUE)] <- 0 # the number of interaction terms is {p*(p-1)}/2
  }
  betai
}

##################################################################################
## generate correlated chi-square
##################################################################################
generate_chi <- function(n, p, rho, chi_coef = 1, combine = TRUE) {
  # generate individual chi_square
  p_normal <- p*chi_coef
  cor_str <- matrix(rep(rho,p_normal^2), ncol = p_normal)
  diag(cor_str) <- 1
  x <- mvrnorm(n = n,
               mu = rep(0,p_normal),
               Sigma = cor_str)
  x <- x^2
  
  # combine different chi square to get different degree of freedom
  if(chi_coef == 1) {len_index <- p; index_p <- sample(1:p)}
    else len_index <- 0
  
  while(len_index < p) {
    index_p <- sample(1:p, p_normal, replace = TRUE)  
    len_index <- unique(index_p) %>% length(.)
  } # make sure we sample all p different groups with replacement
  index_list <- split(1:p_normal, index_p)
  
  b <- lapply(X = index_list, 
              FUN = function(data, index) {rowSums(data[,index, drop = FALSE])}, data = x) %>%
       Reduce(cbind, x = .)
  
  if(combine) b <- model.matrix(~.*.+0, data.frame(b)) 
  
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "chi", 
                               corr = rho))
  b
}

##################################################################################
## generate correlated chi-square
##################################################################################

generate_chi_resample <- function(n, p, rho, chi_coef = 1, pro) {
  file_list <- list.files("./R_code/simulation_fixed_random/chi_square/")
  file_name <- paste(c(n,p,rho,chi_coef), collapse = "_")
  if(pmatch(file_name, file_list, nomatch = 0) == 0){
    b_all <- generate_chi(n,p,rho,chi_coef)
    write.csv(x = b_all, file = paste0("./R_code/simulation_fixed_random/chi_square/", file_name, ".csv"), row.names = FALSE)
  }
  # load data 
  b <- read.csv(file = paste0("./R_code/simulation_fixed_random/chi_square/", file_name, ".csv"), 
                header = TRUE) 
  # subset data
  index <- sample(1:n, round(pro*n,0), replace = FALSE)
  b <- b[index,]
  
  # return b 
  attributes(b) <- append(attributes(b), list(x_dist = "chi_resample"))
  b
}



##################################################################################
## simulation function with simulated covariate
#################################################################################

simulation_fn <- function(n,
                          p,
                          tran_fun = null_tran,
                          combine = FALSE,
                          main_fixed = TRUE,
                          inter_fixed = TRUE,
                          generate_data,
                          gene_args,
                          brep, 
                          nrep,
                          uncorr_method = NULL,
                          uncorr_args = NULL,
                          interaction = 0, 
                          interaction_m = 0, 
                          seed = 0, 
                          cores = 1) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  
  if(seed != 0) set.seed(seed) # set seed for foreach
  
  
  # Generate main betas
  if(main_fixed){
    betam <- generate_main(p)
  }
  
  
  # Generate interaction gammas
  if(inter_fixed){
    betai <- generate_inter(p, interaction)
  }
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove") %dorng%   {
    # Initial output 
    result_tmp <- matrix(0, nrow = nrep, ncol = 6)
    
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_args)
    
    # Standardized covariates
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(main_fixed = main_fixed, 
                       inter_fixed = inter_fixed,
                       x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_args), as.list(uncorr_args)))
    
    b <- std_fn(b = b_raw,
                p = ncol(b_raw),
                tran_FUN = tran_fun)
    b_m <- b[,1:p]
    b_i <- b[,-(1:p)]
    
    # Generate main betas
    if(!main_fixed){
      betam <- generate_main(p)
    }
    
    # Generate interaction gammas
    if(!inter_fixed){
      betai <- generate_inter(p, interaction)
    }
    
    # Generate the signals
    signalm <- b_m%*%betam
    signali <- b_i%*%betai[upper.tri(betai, diag = FALSE)]
    result_tmp[, 2]=var(signali)
    
    if(combine == TRUE){
      result_tmp[, 1]=var(signalm + signali)
      b_final <- cbind(b_m, b_i)
    } else {
      result_tmp[, 1]=var(signalm)
      b_final <- b_m
    }
    
    
    # Estimating total effects with iterations
    for(irep in 1:nrep){
      # Generate health outcome given fixed random effects
      y=signalm+signali+rnorm(length(signalm),sd=4)
      
      fit=Yang(y,b_final,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
      # uncorrelated data 
      x <- uncorr_fn(b_final, uncorr_method, uncorr_args)
      
      # Call the GCTA method
      fit=Yang(y,x,interact = interaction_m)
      result_tmp[irep,5] <- fit$G
      result_tmp[irep,6] <- fit$RACT
    }
    
    
    # save the result
    result_final <- rbind(apply(result_tmp, 2, mean), apply(result_tmp, 2,sd))
    result_final <- data.frame(result_final, additional) ## adding attributes as plot categories
    
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  colnames(result_raw)[1:6] <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "prop_main", "prop_interaction")
  
  if(combine == TRUE){
    colnames(result_raw)[1:6] <- c("true_total", "true_interaction", "GCTA_total", "GCTA_interaction", "prop_total", "prop_interaction")
  }
  result_raw
}


