setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

##################################################################################
## mutiple simulation fitting function
##################################################################################

compare_corr_GCTA <- function(b, 
                              brep, 
                              nrep,
                              interaction = 0, 
                              interaction_m = 0, 
                              seed = 0, 
                              cores = 8) {
  
  doParallel::registerDoParallel(cores = cores) # setting cores
  
  n=dim(b)[1]
  p=dim(b)[2]
  b <- std_fn(b, p) # standardized enviromental dataset
  
  if(seed != 0) set.seed(seed) # set seed for foreach
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE) %dorng%   {
    
    result_tmp <- matrix(0, nrow = nrep, ncol = 6)
    
    # 1. Generate health outcome
    betam=rnorm(p, m=0, sd=0.5) # main_effect ~ N(0,0.5)
    betam[2*c(1:17)]=0  # miminc the zero coeficients
    
    if(interaction==0) {
      betai <- 0
    } else {
      betai <- matrix(rnorm(p*p,m=0,sd=0.1),ncol=p)
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
    
    # Iterations with fixed random effects
    for(irep in 1:nrep){
      y=signalm+signali+rnorm(n,sd=4)
      
      fit=Yang(y,b,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
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
      result_tmp[irep,5] <- fit$G
      result_tmp[irep,6] <- fit$RACT
    }
    
    result_tmp <- rbind(apply(result_tmp, 2, mean), apply(result_tmp, 2,sd))
    result_tmp
    
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  # result_raw <- matrix(result_raw, nrow = 1)
  colnames(result_raw) <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "pro_main", "pro_interaction")
  result_raw
}


a=read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
a=data.matrix(a[,2:35], rownames.force = NA)
b_log <- log(a)
b_sqrt <- sqrt(a)
b_cox <- box_cox_tran(a)

data_list <- list(PCB = a,
                  PCB_log = b_log,
                  PCB_sqrt = b_sqrt,
                  PCB_cox = b_cox)

interaction_list <- list(1,1,1,1)
interaction_m_list <- list(1,1,1,1)

# cat("testing...\n")
# 
# test <- compare_corr_GCTA(b = data_list[[1]],
#                           interaction = interaction_list[[1]],
#                           interaction_m = interaction_m_list[[2]],
#                           brep = 2,
#                           nrep = 10,
#                           seed = 1,
#                           cores = 1)

result_list <- mapply(FUN = compare_corr_GCTA,
                      b = data_list,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 80, nrep = 20, seed = 123, cores = 10),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_log_sqrt_tranformation")
