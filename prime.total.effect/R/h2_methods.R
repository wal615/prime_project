# Variance esitmation methods for high dimensional data
#' GCTA method 
#' @param y A continuous vector
#' @param x A matrix for covariates
#' @param target A character for estimation target
#' @import rrBLUP
#' @export
GCTA.rr <- function(x,y, target = c("beta2","h2")[1]){
  x <- std.fn(x)
  res <- mixed.solve(y = y, Z = x)
  # in GCTA setting we have Vu = Vg/p, so to estimate Vg we need multiple p
  G <- res$Vu * ncol(x)
  if(target == "h2"){
    G <- G/(G + res$Ve)
  }
  G
}

#' Dicker 2013
#' the function is based on the Lee H Dicker's 2013 paper: Variance estimation i high-dimensional linear models
#' @param y A continuous vector
#' @param x A matrix for covariates
#' @param target A character for estimation target
#' @export 
Dicker.2013 <- function(x, y, target = c("beta2","h2")[1]){
  x <- std.fn(x) # note that we need to center the covariates
  # n <- nrow(x) - 1 # note 
  n <- nrow(x)
  d <- ncol(x)
  if(d < n){
    warning("n is large p, this method can only work when n is smaller than p")
    return(rep(NA,4))
  }
  norm2.y <- sum(y^2) 
  norm2.xy <- sum((t(x)%*%y)^2)
  sigma2.hat <- (d + n + 1)/(n*(n+1))*norm2.y - 1/(n*(n+1))*norm2.xy
  tau2.hat <- -d/(n*(n+1))*norm2.y + 1/(n*(n+1))*norm2.xy
  r2.hat <- tau2.hat/(tau2.hat + sigma2.hat)
  phi2.1 <- 2*(d/n*(sigma2.hat + tau2.hat)^2 + sigma2.hat^2 + tau2.hat^2) 
  phi2.2 <- 2*((1+d/n)*(sigma2.hat + tau2.hat)^2 - sigma2.hat^2 + 3*tau2.hat^2)
  phi2.0 <- 2/(tau2.hat + sigma2.hat)^2 * ((1+d/n)*(tau2.hat + sigma2.hat)^2 - sigma2.hat^2)
  sigma2.var <- phi2.1/n
  tau2.var <- phi2.2/n
  r2.var <- phi2.0/n
  result <- c(sigma2.hat = sigma2.hat,
              sigma2.var = sigma2.var,
              beta2 = tau2.hat,
              beta2.var = tau2.var,
              h2 = r2.hat,
              h2.var = r2.var)
  if(target == "beta2"){
    result[3:4]
  } else if (target == "h2")
    result[5:6]
}



#' EigenPrism 
#' Author: Lucas Janson (statweb.stanford.edu/~ljanson)
#' @param y A continuous vector
#' @param X A matrix for covariates
#' @param target A character for estimation target
#' @import cccp
#' @export 

EigenPrism <- function(y,
                       x,
                       invsqrtSig=NULL,
                       alpha=0.05,
                       target = c("beta2","h2")[1],
                       zero.ind=c(),
                       diagnostics=F){
  # Author: Lucas Janson (statweb.stanford.edu/~ljanson)
  # Runs EigenPrism procedure for estimating and generating confidence
  #  intervals for variance components in high-dimensional linear model:
  #       y = x%*%beta + e,   rows of x iid~ N(0,Sig),   e iid~ N(0,sigma^2)
  #  Requires cccp package for solving second order cone optimization.
  #  Note confidence interval endpoints may lie outside parameter domain, so it may be appropriate
  #   to clip them after the fact.
  # 
  # Inputs:
  #  y: response vector of length n (will automatically be centered)
  #  x: n by p design matrix; columns will automatically be centered and scaled to variance 1;
  #      should not contain intercept column, since both y and x will be centered
  #  invsqrtSig: if columns of x not independent, p by p positive definite matrix which is the square-root
  #               of the inverse of Sig, where Sig is the *correlation* matrix of the x (default is identity)
  #  alpha: significance level for confidence interval (default = 0.05)
  #  target: target of estimation/inference
  #		  'beta2' (default) is the squared 2-norm of the coefficient vector: sum(beta^2)
  #           'sigma2' is the noise variance sigma^2
  #           'heritability' is the fraction of variance of y explained by x%*%beta: t(beta)%*%Sig%*%beta/var(y)
  #  zero.ind: vector of which indices of the weight vector w to constrain to zero (default is none)
  #  diagnostics: boolean (default = T) for whether to generate diagnostic plots for the V.i, lambda.i, and w.i
  #  
  # Outputs:
  #  estimate: unbiased estimate of the target (for heritability, only approximately unbiased)
  #  CI: 100*(1-alpha)% confidence interval for target
  
  # Get dimensionality of problem
  n = nrow(x)
  p = ncol(x)
  if(n > p) {
    warning("n is large p, this method can only work when n is smaller than p")
    return(rep(NA,3))}
  # Transform y and x to proper form
  y = y-mean(y)
  # x = scale(x,T,T)*n/(n-1)
  x = std.fn(x)
  if(!is.null(invsqrtSig)) x = x%*%invsqrtSig
  
  # Take singular value decomposition and rescale singular values
  svd = svd(x)
  lambda = svd$d^2/p
  
  # Defined cone-constrained linear problem to optimize weights; [v; w] is vector of optimization variables
  q = c(1,rep(0,n)) #coefficient vector in objective function
  A = rbind(c(0,rep(1,n)),c(0,lambda)) #matrix for linear constraints
  b = c(0,1) #vector for linear constraints
  if(target=='sigma2') b = c(1,0) #switch constraints if target is sigma^2
  # Constrain some weights to be zero if desired
  if(!is.null(zero.ind)){
    A = rbind(A,cbind(rep(0,length(zero.ind)),diag(rep(1,n))[zero.ind,]))
    b = c(b,rep(0,length(zero.ind)))
  }
  # Define second-order cone constraints
  soc1 = socc(diag(c(1/4,rep(1,n))),c(-1/2,rep(0,n)),c(1/4,rep(0,n)),1/2)
  soc2 = socc(diag(c(1/4,lambda)),c(-1/2,rep(0,n)),c(1/4,rep(0,n)),1/2)
  prob = dlp(as.vector(q),A,as.vector(b),list(soc1,soc2))
  # Solve optimization problem and extract variables
  opt = cps(prob,ctrl(trace=F))
  v = getx(opt)[1]
  w = getx(opt)[-1]
  # Compute estimate and y's variance
  est = sum(w*(t(svd$u)%*%y)^2)
  yvar = sum(y^2)/n
  
  # Compute confidence interval
  CI = est + yvar*sqrt(v)*qnorm(1-alpha/2)*c(-1,1)
  if(target=='h2'){
    est = est/yvar
    CI = CI/yvar
  }
  # Generate list with results
  # result=list()
  # result$estimate = est
  # result$CI = CI
  result = rep(NA,3)
  result[1] = est
  result[2:3] = CI
  if(target=='h2'){
    names(result) <- c("h2", "CI1", "CI2")
  } else {
    names(result) <- c("beta2", "CI1", "CI2")
  }
  
  # Generate diagnostic plots
  if(diagnostics){
    par(mfrow=c(1,3))
    
    # Check that eigenvectors are approximately Gaussian
    nV = floor(log10(n))
    srtV = svd$v[,10^(0:nV)]
    labs = c()
    for(i in 1:(nV+1)){
      srtV[,i] = sort(srtV[,i])
      ind = 10^(i-1)
      labs = c(labs,bquote(V[.(ind)]))
    }
    matplot(qnorm((1:p)/(p+1)),srtV,type="l",lwd=2,
            ylab="Quantiles of Eigenvectors",xlab="Gaussian Quantiles",
            main=expression(paste("Check Gaussianity of Eigenvectors ",V[i])))
    legend("topleft",as.expression(labs),col=1:(nV+1),lty=1:(nV+1),lwd=2)
    
    # Check that there are no outliers in the eigenvalues
    hist(lambda,main=expression(paste("Histogram of Normalized Eigenvalues ",lambda[i])),
         xlab=expression(lambda[i]))
    
    # Check that the weights are not dominated by just a few values
    srtw = sort(abs(w),T)
    plot(1:n,cumsum(srtw)/sum(srtw),type="l",lwd=2,
         main=expression(paste("Fraction of Total Weight in Largest k ",w[i])),
         xlab="k",ylab="Fraction of Total Weight")
  }
  
  return(result)
}

