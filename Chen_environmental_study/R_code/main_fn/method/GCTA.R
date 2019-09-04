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
Yang=function(y,x,interact=0, target = "beta2"){ 
  x <- std_fn(x)
  ## data y[1:nr], x[1:nr,1:nc]
  nr=dim(x)[1]
  nc=dim(x)[2]
  WW=x%*%t(x)/nc
  II=diag(rep(1,nr))-rep(1,nr)%*%t(rep(1,nr))/nr
  
  RACT=0 # interaction effects
  if(interact==0){
    s <- tryCatch(svd(WW,nu=nr,nv=0,LINPACK=TRUE), error = function(e) e) # return return NA if there is an error for LINPACK
    if(inherits(s, "error")) return(list(G=NA,E=NA,RACT=NA))  # singular value decomposition
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
      
      if(is.na(sigmaGnew) | is.na(sigmaEnew)) return(list(G=NA,E=NA,RACT=NA)) # return NA if there is an error
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
  
  if(target == 'heritability'){
    sigmaG <- sigmaG/var(y)
    RACT <- RACT/var(y)
  }
  return(list(G=sigmaG,E=sigmaE,RACT=RACT))
}
