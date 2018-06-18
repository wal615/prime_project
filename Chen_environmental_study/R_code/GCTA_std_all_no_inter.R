setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

compare_corr_GCTA <- function(b, 
                              brep, 
                              nrep,
                              interaction = 0, 
                              interaction_m = 0, 
                              seed = 0, 
                              cores = 8) {
  
  doParallel::registerDoParallel(cores = cores) # setting cores
  b_m <- b[,1:34]
  b_i <- b[,-(1:34)]
  
  n=dim(b_m)[1]
  p=dim(b_m)[2]
  
  if(seed != 0) set.seed(seed) # set seed for foreach
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE) %dorng%   {
    
    result_tmp <- matrix(0, nrow = 1, ncol = 6)
    
    # 1. Generate health outcome
    betam=rnorm(p, m=0, sd=0.5) # main_effect ~ N(0,0.5)
    betam[2*c(1:17)]=0  # miminc the zero coeficients
    
    if(interaction==0) {
      betai <- rep(0, ncol(b)-ncol(b_m))
    } else {
      p_i <- p*(p-1)/2
      betai <- rnorm(p_i,m=0,sd=0.1)
    } 
    
    # Generate the signals
    signalm=b_m%*%betam
    signali=b_i%*%betai
    result_tmp[1, 1]=var(signalm)
    result_tmp[1, 2]=var(signali)
    
    # Iterations with fixed random effects
    for(irep in 1:nrep){
      y=signalm+signali+rnorm(n,sd=4)
      
      fit=Yang(y,b,interact = interaction_m)
      result_tmp[1,3] <- fit$G
      result_tmp[1,4] <- fit$RACT
      
      # Estimating total effects
      # transform covariates into uncorrelated
      Sigma=cov(b,b)
      # Compute Sigma^{-1/2}
      Seign=eigen(Sigma)
      Sinvsqrt=Seign$vectors %*% diag(1/sqrt(Seign$values)) %*% t(Seign$vectors)
      x=b%*%Sinvsqrt
      #cor(X,X)
      
      # Call the GCTA method
      fit=Yang(y,x,interact = interaction_m)
      result_tmp[1,5] <- fit$G
      result_tmp[1,6] <- fit$RACT
    }
    
    apply(result_tmp, 2, mean)
    
    
  }
  
  colnames(result_raw) <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "pro_main", "pro_interaction")
  result_raw
}

a=read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
b=data.matrix(a[,2:35], rownames.force = NA) %>% 
  data.frame(.) %>% model.matrix(~.*.+0, .) %>% std_fn(., 595) # include all the two-way interaction
b_norm=matrix(rnorm(1000*34),ncol=34) %>% 
  data.frame(.) %>% model.matrix(~.*.+0, .) %>% std_fn(., 595) # simulated covariates
b_norm_uncorr <- mvrnorm(n = 1000, mu = rep(0,34), Sigma = diag(rep(1,34))) %>% 
  data.frame(.) %>% model.matrix(~.*.+0, .) %>% std_fn(., 595) 
b_norm_corr <- mvrnorm(n = 1000, mu = rep(0,34), Sigma = diag(rep(1,34)) + matrix(rep(2,34*34), nrow = 34)) %>% 
  data.frame(.) %>% model.matrix(~.*.+0, .) %>% std_fn(., 595) 
#b=log(b)

data_list <- list(PCB_no_inter_m = b, 
                  PCB_no_inter = b, 
                  b_norm_uncorr = b_norm_uncorr, 
                  b_norm_corr = b_norm_corr, 
                  PCB_with_inter = b)

interaction_list <- list(0,0,1,1,1)
interaction_m_list <- list(0,0,0,0,0)

# test <- compare_corr_GCTA(b = data_list[[1]],
#                           interaction = interaction_list[[1]],
#                           interaction_m = interaction_m_list[[1]],
#                           brep = 2,
#                           nrep = 10,
#                           seed = 1,
#                           cores = 1)


result_list_combined <- mapply(FUN = compare_corr_GCTA,
                      b = data_list,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 100, nrep = 2, seed = 1014, cores = 10),
                      SIMPLIFY = FALSE)

save(result_list_combined, file = "./result/result_combined_reproduce")



