
##################################################################################
## simulation fitting function with PCB data
##################################################################################
# assume that b is aleardy standardized and normalized

compare_corr_GCTA <- function(b, 
                              brep, 
                              nrep,
                              uncorr_method = NULL,
                              interaction = 0, 
                              interaction_m = 0, 
                              seed = 0, 
                              cores = 1,
                              interm_result = FALSE,
                              interm_result_path = NULL) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  
  n=dim(b)[1]
  p=dim(b)[2]
  
  if(seed != 0) set.seed(seed) # set seed for foreach
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE) %dorng%   {
    
    result_tmp <- matrix(0, nrow = nrep, ncol = 6)
    
    # Generate betas
    betam=rnorm(p, mean =0, sd =0.5) # main_effect ~ N(0,0.5)
    betam[2*c(1:floor(p/2))]=0  # mimic the zero coefficients
    
    if(interaction==0) {
      betai <- 0
    } else {
      betai <- matrix(rnorm(p*p,m=0,sd=0.1),ncol=p) # interaction_effect ~ N(0,0.1)
      betai[lower.tri(betai, diag = TRUE)] <- 0 # the number of interaction terms is {p*(p-1)}/2
    } 
    
    # Generate the signals
    signalm=b%*%betam
    signali <- if(interaction == 0){
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
    
    
    if (interm_result == TRUE) {
      common_attr_index <- match(c("dim", "dimnames"), names(attributes(b))) %>% na.omit(.)
      interm_result_table <- data.frame(result_tmp, attributes(b)[-common_attr_index]) # extract the attributes which has unique infomration about the data
      
      write.csv(interm_result_table, file = paste0(interm_result_path, paste(unlist(attributes(b)[-common_attr_index]), collapse = "_"),"_",ibrep,".csv"))
    }
    
    result_final <- rbind(apply(result_tmp, 2, mean), apply(result_tmp, 2,sd))
    result_final
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  # result_raw <- matrix(result_raw, nrow = 1)
  colnames(result_raw) <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "prop_main", "prop_interaction")
  result_raw
}