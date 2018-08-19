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
generate_chi <- function(n, p, rho, chi_coef = 1, combine = FALSE) {
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
                               corr = rho, 
                               combine = combine,
                               chi_coef = chi_coef))
  b
}


##################################################################################
## simulation function with simulated covariate
##################################################################################


simulation_fn <- function(n,
                          p,
                          combine = FALSE,
                          main_fixed = TRUE,
                          inter_fixed = TRUE,
                          generate_data,
                          gene_args,
                          brep, 
                          nrep,
                          uncorr_method = NULL,
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
    result_tmp <- matrix(0, nrow = nrep, ncol = 6)
    # Generate covariates  
    b_raw <- do.call(generate_data, gene_args)
    
    # Standardized covariates
    # combined the all the attributes to b so we could plot them by the attributes
    additional = list(main_fixed = main_fixed, 
                      inter_fixed = inter_fixed,
                      x_dist = attributes(b_raw)$x_dist,
                      corr = attributes(b_raw)$corr,
                      combine = attributes(b_raw)$combine,
                      df = attributes(b_raw)$chi_coef)
    b <- std_fn(b = b_raw,
                p = ncol(b_raw),
                tran_FUN = null_tran,
                additional = additional)
    
    # Generate main betas
    if(!main_fixed){
      betam <- generate_main(p)
    }
    
    # Generate interaction gammas
    if(!inter_fixed){
      betai <- generate_inter(p, interaction)
    }
    
    # Generate the signals
    if(combine == TRUE) {
      beta <- c(betam, betai[upper.tri(betai, diag = FALSE)])
      signalm <- b%*%beta
    } else {
      signalm=b%*%betam
      }
    signali <- if(interaction == 0 | combine == TRUE){
      rep(0,n) } else {
        apply(X = b, MARGIN = 1, FUN = function(x) t(x)%*%betai%*%x)
      } 
    result_tmp[, 1]=var(signalm)
    result_tmp[, 2]=var(signali)
    
    # Estimating total effects with iterations
    for(irep in 1:nrep){
      
      # Generate health outcome given fixed random effects
      y=signalm+signali+rnorm(n,sd=4)
      
      fit=Yang(y,b,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
      # uncorrelated data 
      if(is.null(uncorr_method) == TRUE) 
        x <- uncorr_fn(b)
      else x <- uncorr_fn(b, uncorr_method)
      
      # Call the GCTA method
      fit=Yang(y,x,interact = interaction_m)
      result_tmp[irep,5] <- fit$G
      result_tmp[irep,6] <- fit$RACT
    }
    

    # save the result
    result_final <- rbind(apply(result_tmp, 2, mean), apply(result_tmp, 2,sd))
    common_attr_index <- match(c("dim", "dimnames", "assign"), names(attributes(b))) %>% na.omit(.)
    result_final <- data.frame(result_final, attributes(b)[-common_attr_index]) ## adding attributes as plot categories
    
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  colnames(result_raw)[1:6] <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "prop_main", "prop_interaction")
  result_raw
}


