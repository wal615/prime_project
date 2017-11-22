###### R code for the paper SIMR.pdf
###### using R package "dr"
###### construct weighted ASAVE and corresponding Chisquare test

### install the package 
library("dr")

####### dimension reduction regression
#### SIR method

#### self-defined function for non-negative matrix decomposition
#### A=P^2, return P
#### idea: A=UDV^T, P=U*sqrt(D)*V^T (U=V if A=A^T)
AAself<-function(A) {   ## A: p*p non-negative matrix
  p<-dim(A)[1];
  Asvd<-svd(A);
  D<-matrix(0,p,p);
  for(i in 1:p) {
    D[i,i]<-Asvd$d[i];
    if(D[i,i]<0) {cat("\n Not non-negative matrix!\n");return(-1);}
    D[i,i]<-sqrt(D[i,i]);
  }
  P<-Asvd$u%*%D%*%t(Asvd$v);
  return(P);
}

#### self-defined function for non-negative matrix decomposition
#### A=P^2, return P^{-1}=A^{-1/2}
#### idea: A=UDV^T, P^{-1}=U*sqrt(1/D)*V^T (U=V if A=A^T)
A1self<-function(A) {   ## A: p*p non-negative matrix
  p<-dim(A)[1];
  Asvd<-svd(A);
  D<-matrix(0,p,p);
  for(i in 1:p) {
    D[i,i]<-Asvd$d[i];
    if(D[i,i]<0) {cat("\n Not non-negative matrix!\n");return(-1);}
    D[i,i]<-sqrt(1/D[i,i]);
  }
  P<-Asvd$u%*%D%*%t(Asvd$v);
  return(P);
}


## self-defined function for SIR, based on Li(1991)
## output: eigenvalues, eigenvectors, matrix v 
sirself<-function(y,x,n,p,slice) {
  # n: sample size;  p: number of explanatory variables
  # y: response, n*1; 
  # x: explanatory variables, n*p
  # slice: number of slices
  z<-x;
  for(i in 1:p) z[,i]<-x[,i]-mean(x[,i]);
  sigxx<-t(z)%*%z/n;          ## sigma_x
  sigxx2<-A1self(sigxx);      ## sigma_x^{-1/2}
  z<-z%*%sigxx2;              ## z=sigma_x^{-1/2}(x-\bar{x})
  yorder<-order(y);
  y<-y[yorder];
  z<-z[yorder,];
  hslice<-matrix(0,2,slice);  ## i^th slice is hslice[1,i]~hslice[2,i]
  hslice[1,1]<-1;
  hslice[2,slice]<-n;
  for(i in 1:(slice-1)) { 
    hslice[2,i]<-floor(n*i/slice);
    hslice[1,i+1]<-hslice[2,i]+1;
  }
  rho<-rep(0,slice);          ## proportion of each slice
  for(i in 1:slice) rho[i]<-(hslice[2,i]-hslice[1,i]+1)/n;
  m<-matrix(0,slice,p);       ## sliced sample mean of z
  for(i in 1:slice) m[i,]<-apply(z[hslice[1,i]:hslice[2,i],],2,mean);
  V<-matrix(0,p,p);           ## weighted covariance matrix
  for(i in 1:slice) V<-V+rho[i]*m[i,]%*%t(m[i,]);
  sireigen<-eigen(V);
  eigenvalues<-sireigen$values;
  eigenvectors<-sigxx2%*%sireigen$vectors;
  for(i in 1:p) eigenvectors[,i]<-eigenvectors[,i]/sqrt(sum(eigenvectors[,i]^2));
  answer<-list(eigenvalues=eigenvalues,eigenvectors=eigenvectors, v = V); 
  answer; 
}
#sirself(Y,Xm[,1:4],nNum,4,8)
#sirfit<-dr(Y~X1+X2+X3+X4,method="sir")
#dr.permutation.test(sirfit,npermute=200)