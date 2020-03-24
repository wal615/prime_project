#' GLASSO method for covaraite decorrelation
#' @param x A matrix for covariates
#' @param rho A penalty parameter 
#' @import glasso
#' @export
GLASSO.method <- function(x, rho = 0.001){
  sigma=cov(x, x)
  # Compute sigma
  sigma <- glasso(s = sigma, rho = rho, thr = 0.05)$w

  # Compute Signa^{-1/2}
  sigma.isqrt <- invsqrt(sigma)

  # uncorrelated.data
  uncorr.data=x%*%sigma.isqrt

  list(uncorr.data = uncorr.data)
}

#' Historical data based method for covaraite decorrelation
#' @param x A matrix for covariates
#' @param emp.sigma A non-singular covariance matrix calculated from historical data 
#' @export
empirical.cov.method <- function(x,  emp.sigma){
  
  sigma.isqrt <- invsqrt(emp.sigma)  
  
  # uncorrelated.data
  uncorr.data <- x%*%sigma.isqrt
  
  list(uncorr.data = uncorr.data)
  
}

# s here is non-zero cofficients
dgp.BIC <- function(X, sigma, n, s){
  l <- sum(diag(sigma %*% X)) - log(det(X))
  BIC <- -2*l + s*log(n)
  BIC
} 

dgp.AIC <- function(X, sigma, n, s){
  l <- sum(diag(sigma %*% X)) - log(det(X))
  AIC <- -2*l + s*2
  AIC
} 
  
dgp.path <- function(sigma, step = 10, n = n){
  rho.list <- list()
  precision.list <- list()
  AIC.list <- list()
  q<-max(abs(sigma[row(sigma)> col(sigma)]))
  invX <- X <- NULL
  for(i in 1:step){
    rho <- 0.8^(i)*(0.9*q) # follow the suggestion from the paper p12
    if(is.null(X) == TRUE) {
      res.tmp <- dpglasso::dpglasso(sigma = sigma, rho = rho, outer.tol = 0.005)
    } else {
      res.tmp <- dpglasso::dpglasso(sigma = sigma, X = X, invX =invX, rho = rho, outer.tol = 0.005)
    }
    invX <- res.tmp$invX
    X <- res.tmp$X
    s <- sum(X[upper.tri(X, diag=T)] >0)
    # BIC <- dgp.BIC(X = X, sigma = sigma, s = s, n = n)
    AIC <- dgp.AIC(X = X, sigma = sigma, s = s, n = n)
    rho.list <- append(rho.list, rho)
    precision.list <- append(precision.list, list(res.tmp$invX))
    AIC.list <- append(AIC.list, AIC)
  }
  list(rho.list = rho.list,
       precision.list = precision.list,
       AIC.list = AIC.list)
}

dgpGLASSO.method <- function(x, rho = NULL){
  sigma=cov(x, x)
  if(is.null(rho)){
    rho.path <- dgp.path(sigma, n = nrow(x))
    index <- which.min(unlist(rho.path$AIC.list))
    sigma.i <- rho.path$precision.list[[index]]
  } else {
    # Compute sigma^{-1}
    sigma.i <- dpglasso::dpglasso(sigma = sigma, rho = rho, outer.tol = 0.05)$X
  }
  
  # Compute Signa^{-1/2}
  sigma.isqrt <- msqrt(sigma.i)
  
  # uncorrelated.data  
  uncorr.data=x%*%sigma.isqrt
  
  list(uncorr.data = uncorr.data)
}

QUIC.method <- function(input.data, rho = 0.005){
  sigma<-cov(input.data, input.data)
  sigma.i <- QUIC::QUIC(S = sigma, rho = rho, tol = 0.05)$W
  # Compute Signa^{-1/2}
  sigma.isqrt <- msqrt(sigma.i)
  
  # uncorrelated.data  
  uncorr.data=input.data%*%sigma.isqrt
  
  list(uncorr.data = uncorr.data)
}


PCA.method <- function(input.data) {
  pca.x <- prcomp(input.data, retx = TRUE)
  uncorr.data <- pca.x$x
  list(uncorr.data = uncorr.data)
}
