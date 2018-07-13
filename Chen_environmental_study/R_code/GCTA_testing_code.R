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
    sigmaG=var(y)[1,1]/2
    sigmaE=sigmaG
    for(i in 1:50){    
       nlen=nr-1  # should be nonzero eigenvalues
       A11=sum(s$d[1:nlen]^2/(sigmaE+sigmaG*s$d[1:nlen])^2)
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
       if(delta<1e-6){break} # print(round(c(sigmaG,sigmaE),3));    
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

library("sas7bdat")
library(tidyverse)
a=read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
a=data.matrix(a[,2:35], rownames.force = NA)

cat("original scale with interaction in the models\n")
b=a

# cat("box-cox transformation (lambda = 2) with interaction in the models\n")
# b= MASS::boxcox(lm(a[,1]~1))

set.seed(1014)
nrep=100
result=array(0,c(nrep,6))
# 1. Generate health outcome
n=dim(b)[1]
p=dim(b)[2]
for(k in 1:p){
    b[,k]=rank(b[,k]) # using ranking to improve the normality
    # emprircal_cdf <- ecdf(b[,k]) # empricial dist
    # b[,k][which.min(b[,k])] <- b[,k][which.min(b[,k])] + 0.0001 # modify the max and min values to avoid Inf
    # b[,k][which.max(b[,k])] <- b[,k][which.max(b[,k])] - 0.0001
    # b[,k] <- emprircal_cdf(b[,k]) %>% qnorm(.)
    me=mean(b[,k])
    std=sqrt(var(b[,k]))
    b[,k]=(b[,k]-me)/std
}

b_inter <- (data.frame(b)) %>% model.matrix(~.*.+0, data = .)

# betam=rnorm(p, m=0, sd=0.5)
# betai=matrix(rnorm(p*p,m=0,sd=0.1),ncol=p) # beta for interaction
# betam[2*c(1:17)]=0
# for(k in 1:p){
#   betai[k,k]=0
# }
# signalm=b%*%betam
# 
# signali=rep(0,n)
# for(i in 1:n){
#    signali[i]=t(b[i,])%*%betai%*%b[i,]
#     }

betaa=c(rnorm(p, m=0, sd=0.5), rnorm(595 - p, m=0, sd=0.5))
signala=b_inter%*%betaa
signalm = signala[1:34]
signali = signala[-(1:34)]
for(irep in 1:nrep){
 print(c(irep,nrep))
 
 y=signalm+signali+rnorm(n,sd=4)
 
 result[irep,1]=var(signalm)
 result[irep,2]=var(signali)
 if (irep == 1) {
   print(var(signalm))
   print(var(signali))
 } 

 fit=Yang(y,b,interact=1)
 result[irep,3]=fit$G
 result[irep,4]=fit$RACT

# 2. Estimating total effects
  # transform covariates into uncorrelated
Sigma=cov(b,b)
 # Compute Sigma^{-1/2}
Seign=eigen(Sigma)
Sinvsqrt=Seign$vectors %*% diag(1/sqrt(Seign$values)) %*% t(Seign$vectors)
x=b%*%Sinvsqrt   
#cor(X,X)

# Call the GCTA method
fit=Yang(y,x,interact=1)
result[irep,5]=fit$G
result[irep,6]=fit$RACT

}

print(apply(result,2,mean))
print(apply(result,2,var))

