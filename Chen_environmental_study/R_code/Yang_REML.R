library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

##################################################################################
## YANG's REML function
##################################################################################
# interact parameter indicates if the model includes interaction terms (interact == 0) 
Yang=function(y,x,interact=0){  
  ## data y[1:nr], x[1:nr,1:nc]
  nr=dim(x)[1]
  nc=dim(x)[2]
  WW=x%*%t(x)/nc
  II=diag(rep(1,nr))-rep(1,nr)%*%t(rep(1,nr))/nr
  
  RACT=0 # interaction effects
  if(interact==0){
    s=svd(WW,nu=nr,nv=0,LINPACK=TRUE) # singular value decomposition
    YD=(t(s$u)%*%y)^2
    ID=rep(1,nr)-(t(s$u)%*%rep(1,nr))^2/nr
    
    # REML estimator
    sigmaG=var(y)[1,1]/2 # need sigmaG to be a numeric instead of a matrix
    sigmaE=sigmaG
    for(i in 1:50){    
      nlen=nr-1  # should be nonzero eigenvalues
      A11=sum(s$d[1:nlen]^2/(sigmaE+sigmaG*s$d[1:nlen])^2) # sigmaG is a 1 by 1 matrix? 
      A12=sum(s$d[1:nlen]/(sigmaE+sigmaG*s$d[1:nlen])^2)
      A22=sum(1/(sigmaE+sigmaG*s$d[1:nlen])^2)
      B1=sum(s$d[1:nlen]*YD[1:nlen]/(sigmaE+sigmaG*s$d[1:nlen])^2)
      B2=sum(YD[1:nlen]/(sigmaE+sigmaG*s$d[1:nlen])^2)
      den=A11*A22-A12*A12
      sigmaGnew=(A22*B1-A12*B2)/den
      sigmaEnew=(A11*B2-A12*B1)/den
      
      #use complementary slackness
      if(sigmaGnew<0 && sigmaEnew>=0){sigmaGnew=0; sigmaEnew=B2/A22}
      if(sigmaGnew>=0 && sigmaEnew<0){sigmaGnew=B1/A11; sigmaEnew=0}
      if(sigmaGnew<0 && sigmaEnew<0){sigmaGnew=0; sigmaEnew=0}
      
      delta=abs(sigmaGnew-sigmaG)+abs(sigmaEnew-sigmaE)
      sigmaG=sigmaGnew
      sigmaE=sigmaEnew

      if(delta<1e-6){break} #print(round(c(sigmaG,sigmaE),3));
      #print(c(i,sigmaG,sigmaE,sigmaGnew,sigmaEnew,delta))
    }
    #print(c(sigmaG,sigmaE,var(y)))
  }else{
    WW2=WW^2
    WW2=II%*%WW2%*%II
    YY=(y-mean(y))%*%t(y-mean(y))
    
    sc=rep(0,3)
    inf=matrix(0,ncol=3,nrow=3)
    
    sc[1]=sum(diag(II%*%YY))
    sc[2]=sum(diag(WW%*%YY))
    sc[3]=sum(diag(WW2%*%YY))
    
    inf[1,1]=sum(diag(II%*%II)) 
    inf[1,2]=sum(diag(II%*%WW))
    inf[1,3]=sum(diag(II%*%WW2))
    inf[2,2]=sum(diag(WW%*%WW))
    inf[2,3]=sum(diag(WW%*%WW2)) 
    inf[3,3]=sum(diag(WW2%*%WW2))
    
    inf[2,1]=inf[1,2]
    inf[3,1]=inf[1,3]
    inf[3,2]=inf[2,3]
    
    delta=solve(inf)%*%sc
    
    sigmaE=delta[1]
    sigmaG=delta[2]
    RACT=delta[3]
  }  
  
  return(list(G=sigmaG,E=sigmaE,RACT=RACT))
}

##################################################################################
## multiple simulation fitting function
##################################################################################
# assume that b is aleardy standardized and normalized

compare_corr_GCTA <- function(b, 
                              brep, 
                              nrep,
                              interaction = 0, 
                              interaction_m = 0, 
                              seed = 0, 
                              cores = 1) {
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
    betam=rnorm(p, m=0, sd=0.5) # main_effect ~ N(0,0.5)
    betam[2*c(1:17)]=0  # mimic the zero coefficients
    
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
    
    # Estimating total effects
    for(irep in 1:nrep){

      # Generate health outcome fixed random effects
      y=signalm+signali+rnorm(n,sd=4)
      
      fit=Yang(y,b,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
      # transform covariates into uncorrelated (proposed method)
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

##################################################################################
## standardized function with tranformation features
##################################################################################
# default 
std_fn <- function(b, p, tran_FUN = null_tran, ...){
  b <- apply(b, 2, tran_FUN, ...)
  for(k in 1:p){
    me=mean(b[,k])
    std=sqrt(var(b[,k]))
    b[,k]=(b[,k]-me)/std
    # b[,k]=(b[,k]-mean(b[,k]))/sqrt(var(b[,k]))
  }
  b
}

##################################################################################
## Rank transformation function
##################################################################################

null_tran <- function(y) {
  y
}

##################################################################################
## cox_box transformation function
##################################################################################

box_cox_tran <- function(y) {
  bc <- MASS::boxcox(y~1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]  # find the optimal lambda based on lm model
  if(lambda == 0) bc_y <- log(y) 
    else bc_y <- (y^lambda - 1)/lambda
  bc_y
}

##################################################################################
## Rank transformation function
##################################################################################

rank_tran <- function(y) {
  rank(y)
}

##################################################################################
## Normal quantile transformation function
##################################################################################

norm_quantile_tran <- function(y) {
  emprircal_cdf <- ecdf(y) # empricial dist 
    y[which.min(y)] <- y[which.min(y)] + 0.0001 # modify the max and min values to avoid Inf 
    y[which.max(y)] <- y[which.max(y)] - 0.0001
    y <- emprircal_cdf(y) %>% qnorm(.) 
}
