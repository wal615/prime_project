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
## output: eigenvalues, eigenvectors
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

#### y-pHd method
#yphdfit<-dr(Y~X1+X2+X3+X4,method="phdy")

#### r-pHd method
#rphdfit<-dr(Y~X1+X2+X3+X4,method="phdres")

#### SAVE method
## self-defined function for SAVE, based on Cook & Weisberg(1991)
## need functions: dr.slices in "dr" package
## output: eigenvalues, eigenvectors,etc
saveself<-function(y,x,slice=NULL) {
  # n: sample size;  p: number of explanatory variables
  # y: response, n*1; 
  # x: explanatory variables, n*p
  # slice: number of slices
  n<-length(y);
  p<-dim(x)[2];
  z<-x;
  for(i in 1:p) z[,i]<-x[,i]-mean(x[,i]);
  sigxx<-t(z)%*%z/n;
  sigxx2<-A1self(sigxx);
  z<-z%*%sigxx2;
  slice<-if(!is.null(slice)) slice else max(8,NCOL(z)+3);
  slices<-dr.slices(y,slice);
  slice<-slices$nslices;      ## modify real #slice due to discontinuity of y
  for(i in 1:slice) {         ## z is sliced centerlized.
    ttemp<-slices$slice.indicator==i;
    for(j in 1:p) z[ttemp,j]<-z[ttemp,j]-mean(z[ttemp,j]);
  }
  SAVE<-matrix(0,p,p);           ## estimated candidate matrix
  id<-diag(rep(1,p));            ## identity matrix
  for(i in 1:slice) {
    ttemp<-slices$slice.indicator==i;
    nh<-slices$slice.sizes[i];
    varzh<-t(z[ttemp,])%*%z[ttemp,]/(nh-1);        ## estimated var(z|y in I_h)
    SAVE<-SAVE+(id-varzh)%*%(id-varzh)*nh/n;
  }
  saveeigen<-eigen(SAVE);
  eigenvalues<-saveeigen$values;
  eigenvectors<-sigxx2%*%saveeigen$vectors;
  for(i in 1:p) eigenvectors[,i]<-eigenvectors[,i]/sqrt(sum(eigenvectors[,i]^2));
  ans<-list(x=x,y=y,call=match.call(),cases=n,evectors=eigenvectors,evalues=eigenvalues,slice.info=slices,numdir=p); 
  ans; 
}
#savefitself<-saveself(Y,Xm[,1:4],slice=9)
#savefit<-dr(Y~X1+X2+X3+X4,method="save",nslices=9)

#### self-defined permutation test
## dealing with self-defined SAVEfit, ASAVEfit objects
permutestself<-function(object, npermute=50, numdir=object$numdir)
{
    call<-object$call;
    x<-object$x;
    n<-dim(x)[1];
    xm<-apply(x,2,mean);
    for(i in 1:n) x[i,]<-x[i,]-xm;
    x<-x%*%object$evectors;
    nd <- min(numdir, length(which(abs(object$evalues)>1e-08))-1);
    nt <- nd + 1;
    obstest<-rep(0,nt);
    for(i in 1:nt) obstest[i]<-n*sum(object$evalues[i:nt]);
    count <- rep(0, nt);
    val <- rep(0, nt);
    for (j in 1:npermute) {
        perm <- sample(1:object$cases);
        for (col in 0:nd) {
            call$x<-if (col == 0) x[perm, ] else cbind(x[, (1:col)], x[perm, -(1:col)]);
            iperm <- eval(call);
            val[col + 1] <- n*sum(iperm$evalues[(col+1):nt]);
        }
        count[val > obstest] <- count[val > obstest] + 1
    }
    pval <- (count)/(npermute + 1)
    ans1 <- data.frame(cbind(obstest, pval))
    dimnames(ans1) <- list(paste(0:(nt - 1), "D vs >= ", 1:nt,"D", sep = ""), c("Stat", "p-value"))
    ans <- list(summary = ans1, npermute = npermute)
    class(ans) <- "dr.permutation.test.self"
    ans
}
#dr.permutation.test(savefit,npermute=200)
#permutestself(savefitself,npermute=200)


#### ASAVE method, based on Ye(2005)
## need functions: dr.slices in "dr" package
## output: eigenvalues, eigenvectors,etc
asaveself<-function(y,x,slice=NULL) {
  # n: sample size;  p: number of explanatory variables
  # y: response, n*1; 
  # x: explanatory variables, n*p
  # slice: number of slices
  n<-length(y);
  p<-dim(x)[2];
  z<-x;
  for(i in 1:p) z[,i]<-x[,i]-mean(x[,i]);
  sigxx<-t(z)%*%z/n;          ## Sigma_xx
  sigxx2<-A1self(sigxx);      ## Sigma^{-1/2}
  z<-z%*%sigxx2;              ## standardized z
  slice<-if(!is.null(slice)) slice else max(8,NCOL(z)+3);
  slices<-dr.slices(y,slice);
  slice<-slices$nslices;      ## modify real #slice due to discontinuity of y
  ASAVE<-matrix(0,p,p);          ## estimated candidate matrix
  id<-diag(rep(1,p));            ## identity matrix
  for(i in 1:slice) {
    zh<-z[slices$slice.indicator==i,];
    nh<-slices$slice.sizes[i];
    muh<-apply(zh,2,mean);       ## estimated E(z|y in I_h)
    zzave<-t(zh)%*%zh/nh;        ## estimated E(zz'|y in I_h)
    ASAVE<-ASAVE+((id-zzave)%*%(id-zzave)+muh%*%t(muh))*nh/n;
  }
  asaveeigen<-eigen(ASAVE);
  eigenvalues<-asaveeigen$values;
  eigenvectors<-sigxx2%*%asaveeigen$vectors;
  for(i in 1:p) eigenvectors[,i]<-eigenvectors[,i]/sqrt(sum(eigenvectors[,i]^2));
  ans<-list(x=x,y=y,call=match.call(),cases=n,evectors=eigenvectors,evalues=eigenvalues,slice.info=slices,numdir=p); 
  ans; 
}
#asavefit<-asaveself(Y,Xm[,1:4],slice=8)
#permutestself(asavefit,npermute=200)


#### Extended Chi-squared test for ASAVE method, based on Ye(2005)
chisqself<-function(y,x,slice=NULL) {
  # y: response, n*1; 
  # x: explanatory variables, n*p
  # slice: number of slices
  n<-length(y);
  p<-dim(x)[2];
  if(dim(x)[1]!=n) {
    cat("\n Dimensions don't match!");
    return(-1);
  }
  z<-x;                          ## z
  mux<-apply(x,2,mean);          ## mu_x
  for(i in 1:p) z[,i]<-x[,i]-mux[i];
  sigxx<-t(z)%*%z/n;             ## sigma_xx
  sigxx2<-A1self(sigxx);         ## sigma^{-1/2}
  z<-z%*%sigxx2;                 ## standardized z
  slice<-if(!is.null(slice)) slice else max(8,NCOL(x)+3);
  slices<-dr.slices(y,slice);
  slice<-slices$nslices;         ## modify real #slice due to discontinuity of y
  nh<-slices$slice.sizes;        ## number of observations in slices
  fh<-nh/n;                      ## f_h
  muxh<-matrix(0,slice,p);       ## mu_h(x)
  sigxh<-rep(0,p*p*slice)        ## sig_h(x,x)
  dim(sigxh)<-c(p,p,slice);
  muxxh<-sigxh;                  ## mu_h(xx')
  sigxx21<-rep(0,p*p*p*slice);   ## sig_h(xx',x)
  dim(sigxx21)<-c(p*p,p,slice);  
  sigxx22<-rep(0,p*p*p*p*slice); ## sig_h(xx',xx')
  dim(sigxx22)<-c(p*p,p*p,slice);
  for(i in 1:slice) {
    xtemp<-x[slices$slice.indicator==i,];
    muxh[i,]<-apply(xtemp,2,mean);
    muxxh[,,i]<-t(xtemp)%*%xtemp/nh[i];
    xxtemp<-matrix(0,nh[i],p*p); ## xx'-mu_h(xx')
    for(j in 1:nh[i]) xxtemp[j,]<-as.vector(xtemp[j,]%*%t(xtemp[j,])-muxxh[,,i]);
    for(j in 1:nh[i]) xtemp[j,]<-xtemp[j,]-muxh[i,];
    sigxh[,,i]<-t(xtemp)%*%xtemp/nh[i];
    sigxx21[,,i]<-t(xxtemp)%*%xtemp/nh[i];
    sigxx22[,,i]<-t(xxtemp)%*%xxtemp/nh[i];
  }
  delta0<-matrix(0,p*(p*slice+slice+1),p*(p*slice+slice+1));     ## Delta_0
  for(h in 1:slice) {
    itemp<-p*p*(h-1);
    delta0[(itemp+1):(itemp+p*p),(itemp+1):(itemp+p*p)]<-sigxx22[,,h]/fh[h];
    itemp<-p*p*slice+p*(h-1);
    delta0[(itemp+1):(itemp+p),(itemp+1):(itemp+p)]<-sigxh[,,h]/fh[h];
    jtemp<-p*p*(h-1);
    delta0[(itemp+1):(itemp+p),(jtemp+1):(jtemp+p*p)]<-t(sigxx21[,,h])/fh[h];
    delta0[(jtemp+1):(jtemp+p*p),(itemp+1):(itemp+p)]<-sigxx21[,,h]/fh[h];
    itemp<-p*(p*slice+slice);
    delta0[(itemp+1):(itemp+p),(jtemp+1):(jtemp+p*p)]<-t(sigxx21[,,h]);
    delta0[(jtemp+1):(jtemp+p*p),(itemp+1):(itemp+p)]<-sigxx21[,,h];
    jtemp<-p*p*slice+p*(h-1);
    delta0[(itemp+1):(itemp+p),(jtemp+1):(jtemp+p)]<-sigxh[,,h];
    delta0[(jtemp+1):(jtemp+p),(itemp+1):(itemp+p)]<-sigxh[,,h];
  }
  itemp<-p*(p*slice+slice);
  delta0[(itemp+1):(itemp+p),(itemp+1):(itemp+p)]<-sigxx;
  Mn<-t(muxh);                                                   ## M_n
  gvec<-matrix(0,slice*p*(p+1),p*(slice*(p+1)+1));               ## \dot g[vec(O,M,mu_x)]
  gvec[1:(p*p*slice),1:(p*p*slice)]<-diag(rep(1,p*p*slice));
  itemp<-p*p*slice;
  gvec[(itemp+1):(itemp+p*slice),(itemp+1):(itemp+p*slice)]<-diag(rep(1,p*slice));
  mtemp<--diag(rep(1,slice))%x%(as.matrix(mux))%x%diag(rep(1,p))-diag(rep(1,p*slice))%x%(as.matrix(mux));
  gvec[1:(p*p*slice),(itemp+1):(itemp+p*slice)]<-mtemp;  
  mtemp<--(as.matrix(as.vector(Mn)))%x%diag(rep(1,p));
  for(h in 1:slice) {
    mtemp[(p*p*(h-1)+1):(p*p*h),]<-mtemp[(p*p*(h-1)+1):(p*p*h),]-diag(rep(1,p))%x%(as.matrix(muxh[h,]));
  }
  jtemp<-p*slice*(p+1);
  gvec[1:(p*p*slice),(jtemp+1):(jtemp+p)]<-mtemp;
  delta<-gvec%*%delta0%*%t(gvec);                                ## Delta
  fm<-diag(rep(1,slice))-fh%*%t(rep(1,slice));                   ## F
  gm<-diag(sqrt(fh));                                            ## G
  fgm<-fm%*%gm;                                                  ## FG
  muzh<-matrix(0,slice,p);                                       ## mu_h(z)
  muzzh<-rep(0,p*p*slice);                                       ## mu_h(zz')
  dim(muzzh)<-c(p,p,slice);
  um<-matrix(0,p,slice*(p+1));                                   ## U_n
  for(h in 1:slice) {
    ztemp<-z[slices$slice.indicator==h,];
    muzh[h,]<-apply(ztemp,2,mean);
    muzzh[,,h]<-t(ztemp)%*%ztemp/nh[h];
    itemp<-p*(h-1);
    um[,(itemp+1):(itemp+p)]<-(muzzh[,,h]-diag(rep(1,p)))*sqrt(fh[h]);
    um[,p*slice+h]<-muzh[h,]*sqrt(fh[h]);
  }
  umsvd<-svd(um,nu=p,nv=slice*(p+1));                            ## UDV'
  gamma1<-umsvd$u;                                               ## (Gamma_11,Gamma_12)
  gamma2<-umsvd$v;                                               ## (Gamma_21,Gamma_22)
  dv<-umsvd$d;                                                   ## D
  value<-matrix(0,p,4);                                          ## output: (d,Lambda_d, d.f., p-value)
  colnames(value)<-c("d","Stat","d.f.","p-value");
  for(d in 0:(p-1)) {
    gamma12<-gamma1[,(d+1):p];                                   ## Gamma_12
    gamma22<-gamma2[,(d+1):(slice*(p+1))];                       ## Gamma_22
    tempm<-matrix(0,slice*(p+1),slice*(p+1));
    itemp<-slice*p;
    tempm[1:itemp,1:itemp]<-fgm%x%sigxx2;                        ## FG(*)Sig_x^(-1/2)
    tempm[(itemp+1):(itemp+slice),(itemp+1):(itemp+slice)]<-fgm; ## FG
    tempm<-tempm%*%gamma22;
    tempm<-tempm%x%t(t(gamma12)%*%sigxx2);
    wm<-t(tempm)%*%delta%*%tempm;                                ## W
    tn<-round(sum(diag(wm))^2/sum(diag(wm%*%wm)));               ## tn
    lambdad<-n*sum(dv[(d+1):p]^2);                               ## Lambda_d
    lambdad<-lambdad*tn/sum(diag(wm));
    value[d+1,1]<-d;
    value[d+1,2]<-lambdad;
    value[d+1,3]<-tn;
    value[d+1,4]<-1-pchisq(lambdad,tn);                          ## p-value
  }
  value;  
}
#chisqself(Y,Xm[,1:4],slice=9)


################ Weighted ASAVE
##### M^alpha_ASAVE = alpha * M_SIR + (1-alpha) * M_zz'|y
##### 0 <= alpha <= 1
## need functions: dr.slices in "dr" package
## output: eigenvalues, eigenvectors,etc
Wasaveself<-function(y,x,slice=NULL,alpha) {
  # n: sample size;  p: number of explanatory variables
  # y: response, n*1; 
  # x: explanatory variables, n*p
  # slice: number of slices
  # alpha: 0 <= alpha <= 1
  n<-length(y);
  p<-dim(x)[2];
  z<-x;
  for(i in 1:p) z[,i]<-x[,i]-mean(x[,i]);
  sigxx<-t(z)%*%z/n;          ## Sigma_xx
  sigxx2<-A1self(sigxx);      ## Sigma^{-1/2}
  z<-z%*%sigxx2;              ## standardized z
  slice<-if(!is.null(slice)) slice else max(8,NCOL(z)+3);
  slices<-dr.slices(y,slice);
  slice<-slices$nslices;      ## modify real #slice due to discontinuity of y
  ASAVE<-matrix(0,p,p);          ## estimated candidate matrix
  id<-diag(rep(1,p));            ## identity matrix
  for(i in 1:slice) {
    zh<-z[slices$slice.indicator==i,];
    nh<-slices$slice.sizes[i];
    muh<-apply(zh,2,mean);       ## estimated E(z|y in I_h)
    zzave<-t(zh)%*%zh/nh;        ## estimated E(zz'|y in I_h)
    ASAVE<-ASAVE+((1-alpha)*(id-zzave)%*%(id-zzave)+alpha*muh%*%t(muh))*nh/n;
  }
  asaveeigen<-eigen(ASAVE);
  eigenvalues<-asaveeigen$values;
  eigenvectors<-sigxx2%*%asaveeigen$vectors;
  for(i in 1:p) eigenvectors[,i]<-eigenvectors[,i]/sqrt(sum(eigenvectors[,i]^2));
  ans<-list(x=x,y=y,call=match.call(),cases=n,evectors=eigenvectors,evalues=eigenvalues,slice.info=slices,numdir=p); 
  ans; 
}
#asavefit<-asaveself(Y,Xm[,1:4],slice=8)
#wasavefit05<-Wasaveself(Y,Xm[,1:4],slice=8,alpha=0.5)


#### Extended Chi-squared test for Weighted ASAVE method, based on Ye(2005)
Wchisqself<-function(y,x,slice=NULL,alpha) {
  # y: response, n*1; 
  # x: explanatory variables, n*p
  # slice: number of slices
  # 0 <= alpha <= 1
  n<-length(y);
  p<-dim(x)[2];
  if(dim(x)[1]!=n) {
    cat("\n Dimensions don't match!");
    return(-1);
  }
  if((alpha<0)||(alpha>1)) {
    cat("\n Alpha should be between 0 and 1!");
    return(-1);
  }
  z<-x;                          ## z
  mux<-apply(x,2,mean);          ## mu_x
  for(i in 1:p) z[,i]<-x[,i]-mux[i];
  sigxx<-t(z)%*%z/n;             ## sigma_xx
  sigxx2<-A1self(sigxx);         ## sigma^{-1/2}
  z<-z%*%sigxx2;                 ## standardized z
  slice<-if(!is.null(slice)) slice else max(8,NCOL(x)+3);
  slices<-dr.slices(y,slice);
  slice<-slices$nslices;         ## modify real #slice due to discontinuity of y
  nh<-slices$slice.sizes;        ## number of observations in slices
  fh<-nh/n;                      ## f_h
  muxh<-matrix(0,slice,p);       ## mu_h(x)
  sigxh<-rep(0,p*p*slice)        ## sig_h(x,x)
  dim(sigxh)<-c(p,p,slice);
  muxxh<-sigxh;                  ## mu_h(xx')
  sigxx21<-rep(0,p*p*p*slice);   ## sig_h(xx',x)
  dim(sigxx21)<-c(p*p,p,slice);  
  sigxx22<-rep(0,p*p*p*p*slice); ## sig_h(xx',xx')
  dim(sigxx22)<-c(p*p,p*p,slice);
  for(i in 1:slice) {
    xtemp<-x[slices$slice.indicator==i,];
    muxh[i,]<-apply(xtemp,2,mean);
    muxxh[,,i]<-t(xtemp)%*%xtemp/nh[i];
    xxtemp<-matrix(0,nh[i],p*p); ## xx'-mu_h(xx')
    for(j in 1:nh[i]) xxtemp[j,]<-as.vector(xtemp[j,]%*%t(xtemp[j,])-muxxh[,,i]);
    for(j in 1:nh[i]) xtemp[j,]<-xtemp[j,]-muxh[i,];
    sigxh[,,i]<-t(xtemp)%*%xtemp/nh[i];
    sigxx21[,,i]<-t(xxtemp)%*%xtemp/nh[i];
    sigxx22[,,i]<-t(xxtemp)%*%xxtemp/nh[i];
  }
  delta0<-matrix(0,p*(p*slice+slice+1),p*(p*slice+slice+1));     ## Delta_0
  for(h in 1:slice) {
    itemp<-p*p*(h-1);
    delta0[(itemp+1):(itemp+p*p),(itemp+1):(itemp+p*p)]<-sigxx22[,,h]/fh[h];
    itemp<-p*p*slice+p*(h-1);
    delta0[(itemp+1):(itemp+p),(itemp+1):(itemp+p)]<-sigxh[,,h]/fh[h];
    jtemp<-p*p*(h-1);
    delta0[(itemp+1):(itemp+p),(jtemp+1):(jtemp+p*p)]<-t(sigxx21[,,h])/fh[h];
    delta0[(jtemp+1):(jtemp+p*p),(itemp+1):(itemp+p)]<-sigxx21[,,h]/fh[h];
    itemp<-p*(p*slice+slice);
    delta0[(itemp+1):(itemp+p),(jtemp+1):(jtemp+p*p)]<-t(sigxx21[,,h]);
    delta0[(jtemp+1):(jtemp+p*p),(itemp+1):(itemp+p)]<-sigxx21[,,h];
    jtemp<-p*p*slice+p*(h-1);
    delta0[(itemp+1):(itemp+p),(jtemp+1):(jtemp+p)]<-sigxh[,,h];
    delta0[(jtemp+1):(jtemp+p),(itemp+1):(itemp+p)]<-sigxh[,,h];
  }
  itemp<-p*(p*slice+slice);
  delta0[(itemp+1):(itemp+p),(itemp+1):(itemp+p)]<-sigxx;
  Mn<-t(muxh);                                                   ## M_n
  gvec<-matrix(0,slice*p*(p+1),p*(slice*(p+1)+1));               ## \dot g[vec(O,M,mu_x)]
  gvec[1:(p*p*slice),1:(p*p*slice)]<-diag(rep(1,p*p*slice));
  itemp<-p*p*slice;
  gvec[(itemp+1):(itemp+p*slice),(itemp+1):(itemp+p*slice)]<-diag(rep(1,p*slice));
  mtemp<--diag(rep(1,slice))%x%(as.matrix(mux))%x%diag(rep(1,p))-diag(rep(1,p*slice))%x%(as.matrix(mux));
  gvec[1:(p*p*slice),(itemp+1):(itemp+p*slice)]<-mtemp;  
  mtemp<--(as.matrix(as.vector(Mn)))%x%diag(rep(1,p));
  for(h in 1:slice) {
    mtemp[(p*p*(h-1)+1):(p*p*h),]<-mtemp[(p*p*(h-1)+1):(p*p*h),]-diag(rep(1,p))%x%(as.matrix(muxh[h,]));
  }
  jtemp<-p*slice*(p+1);
  gvec[1:(p*p*slice),(jtemp+1):(jtemp+p)]<-mtemp;
  delta<-gvec%*%delta0%*%t(gvec);                                ## Delta
  temp<-matrix(0,p*(p+1)*slice,p*(p+1)*slice);
  diag(temp)[1:(p^2*slice)]<-sqrt(1-alpha);
  diag(temp)[(p^2*slice+1):(p*(p+1)*slice)]<-sqrt(alpha);
  delta<-temp %*% delta %*% temp;                                ## Delta^alpha
  fm<-diag(rep(1,slice))-fh%*%t(rep(1,slice));                   ## F
  gm<-diag(sqrt(fh));                                            ## G
  fgm<-fm%*%gm;                                                  ## FG
  muzh<-matrix(0,slice,p);                                       ## mu_h(z)
  muzzh<-rep(0,p*p*slice);                                       ## mu_h(zz')
  dim(muzzh)<-c(p,p,slice);
  um<-matrix(0,p,slice*(p+1));                                   ## U_n
  for(h in 1:slice) {
    ztemp<-z[slices$slice.indicator==h,];
    muzh[h,]<-apply(ztemp,2,mean);
    muzzh[,,h]<-t(ztemp)%*%ztemp/nh[h];
    itemp<-p*(h-1);
    um[,(itemp+1):(itemp+p)]<-(muzzh[,,h]-diag(rep(1,p)))*sqrt(fh[h]);
    um[,p*slice+h]<-muzh[h,]*sqrt(fh[h]);
  }
  temp<-matrix(0,(p+1)*slice,(p+1)*slice);
  diag(temp)[1:(p*slice)]<-sqrt(1-alpha);
  diag(temp)[(p*slice+1):((p+1)*slice)]<-sqrt(alpha);
  um<-um %*% temp;                                               ## U^alpha_n
  umsvd<-svd(um,nu=p,nv=slice*(p+1));                            ## UDV'
  gamma1<-umsvd$u;                                               ## (Gamma_11,Gamma_12)
  gamma2<-umsvd$v;                                               ## (Gamma_21,Gamma_22)
  dv<-umsvd$d;                                                   ## D
  value<-matrix(0,p,4);                                          ## output: (d,Lambda_d, d.f., p-value)
  colnames(value)<-c("d","Stat","d.f.","p-value");
  for(d in 0:(p-1)) {
    gamma12<-gamma1[,(d+1):p];                                   ## Gamma_12
    gamma22<-gamma2[,(d+1):(slice*(p+1))];                       ## Gamma_22
    tempm<-matrix(0,slice*(p+1),slice*(p+1));
    itemp<-slice*p;
    tempm[1:itemp,1:itemp]<-fgm%x%sigxx2;                        ## FG(*)Sig_x^(-1/2)
    tempm[(itemp+1):(itemp+slice),(itemp+1):(itemp+slice)]<-fgm; ## FG
    tempm<-tempm%*%gamma22;
    tempm<-tempm%x%t(t(gamma12)%*%sigxx2);
    wm<-t(tempm)%*%delta%*%tempm;                                ## W^alpha_n
    tn<-round(sum(diag(wm))^2/sum(diag(wm%*%wm)));               ## tn
    lambdad<-n*sum(dv[(d+1):p]^2);                               ## Lambda_d
    lambdad<-lambdad*tn/sum(diag(wm));
    value[d+1,1]<-d;
    value[d+1,2]<-lambdad;
    value[d+1,3]<-tn;
    value[d+1,4]<-1-pchisq(lambdad,tn);                          ## p-value
  }
  value;  
}


## function to compute the vector correlation coefficient q (Hotlling 1936)
##          and the trace correlation r (Hooper 1959), see Ye&Weiss(2003)
## B=span{B_{4*3}},  A=span{(1,0,0,0)', (0,1,0,0)', (0,0,1,0)'}
## q^2=|B'AA'B|, r^2=sum(rho^2_i)/k, rho^2_i's are eigenvalues of B'AA'B
## output: (1-r), (1-q), arccos(q)
vcorrelation <- function(B) {
  A <- diag(c(1,1,1,0))     # AA'
  AB <- t(B) %*% A %*% B    # B'AA'B
  
  

### compare the power with different alpha
alphavec<-seq(0,1,by=0.1)
anum<-length(alphavec)

sink("wasave.txt")
nsimu<-1000
nNumvec<-c(200,400,600)
nslicevec<-c(5,10,15)
power05<-rep(0,4*3*3*anum)        ## 0:1,1:2,2:3,3:4; 5,10,15; 200,400,600; alphavec
dim(power05)<-c(4,3,3,anum)
power01<-power05

for(inNum in 1:3) {            ### beginning of ny loops
nNum<-nNumvec[inNum];
tempt<-proc.time();
for(simu in 1:nsimu) {         ### beginning of simulations
  X1<-rnorm(nNum);
  X2<-rnorm(nNum);
  X3<-rnorm(nNum);
  X4<-rnorm(nNum);
  ep<-rnorm(nNum);
  Xm<-cbind(X1,X2,X3,X4);
  Y<-2*X1*ep+X2^2+X3;
  for(inslice in 1:3) {        ### beginning of nslices
    nslice<-nslicevec[inslice];
    for(ia in 1:anum) {
      temp<-Wchisqself(Y,Xm,slice=nslice,alpha=alphavec[ia]);
      for(i in 1:4) {
        if(temp[i,4]<0.05) power05[i,inslice,inNum,ia]<-power05[i,inslice,inNum,ia]+1;
        if(temp[i,4]<0.01) power01[i,inslice,inNum,ia]<-power01[i,inslice,inNum,ia]+1;
      }
    }   
  }
#  cat("\n n=",nNum,"; simu=",simu);
}                             ### end of simulations
tempt<-proc.time()-tempt;
cat("\n Time spent:");
print(tempt);
for(inslice in 1:3) {
  temp<-power05[,inslice,inNum,];
  rownames(temp)<-c("d=0","d=1","d=2","d=3");
  colnames(temp)<-alphavec;
  cat("\n n=",nNum,"; nslice=",nslicevec[inslice],"(0.05): \n");
  print(temp);
}
for(inslice in 1:3) {
  temp<-power01[,inslice,inNum,];
  rownames(temp)<-c("d=0","d=1","d=2","d=3");
  colnames(temp)<-alphavec;
  cat("\n n=",nNum,"; nslice=",nslicevec[inslice],"(0.01): \n");
  print(temp);
}
}                             ### end of ny loops
sink()
