##################################################################################
## matrix off diag element 
##################################################################################
offdiag <- function(M) {
  index <- upper.tri(M)
  M[index] %>% as.vector(.)
}

##################################################################################
## uncorrelated function
##################################################################################
uncorr_fn <- function(input_data, 
                      uncorr_method = SVD_method , 
                      uncorr_args = NULL,
                      sparse_uncorr_method = NULL, 
                      sparse_uncorr_args = NULL) {
  
  # decorrelation
  args <- append(list(input_data = input_data), uncorr_args) # generate the args for the uncorrelation function
  res <- do.call(uncorr_method, args)
  # sparse decorrelation
  if(!is.null(sparse_uncorr_method)){
    args <- append(list(input_data = res$uncorr_data), sparse_uncorr_args)
    res <- do.call(sparse_uncorr_method, args)
  }
  res$uncorr_data
}

##################################################################################
## SVD method of decorrelation method
##################################################################################

SVD_method <- function(input_data) {
  
  Sigma <- cov(input_data,input_data)
  Sinvsqrt <- invsqrt(Sigma)
  
  # decorrelating the input_data
  uncorr_data=input_data%*%Sinvsqrt 
  
  list(uncorr_data = uncorr_data)
}

##################################################################################
## GLASSO method of decorrelation method
##################################################################################

GLASSO_method <- function(input_data, rho = 0.001){
  Sigma=cov(input_data, input_data)
  # Compute Sigma
  Sigma <- glasso::glasso(s = Sigma, rho = rho, thr = 0.05)$w

  # Compute Signa^{-1/2}
  Sigma_isqrt <- invsqrt(Sigma)

  # uncorrelated_data
  uncorr_data=input_data%*%Sigma_isqrt

  list(uncorr_data = uncorr_data)
}


# GLASSO_method <- function(input_data, rho = 0.01){
#   Sigma=cov(input_data, input_data)
#   # Compute Sigma
#   Sigma <- glasso::glasso(s = Sigma, rho = rho, thr = 0.1)$wi
#   
#   # Compute Signa^{-1/2}
#   Sigma_isqrt <- t(chol(Sigma, pivot = TRUE))
#   
#   # uncorrelated_data  
#   uncorr_data=input_data%*%Sigma_isqrt
#   
#   list(uncorr_data = uncorr_data)
# }


# n=10000; p = 20;
# X<-array(rnorm(n*p),dim=c(n,p)); # data-matrix
# Sigma=cor(X); # sample covariance matrix
# q<-max(abs(Sigma[row(Sigma)> col(Sigma)]));
# rho=q*7;
# B<-dpglasso(Sigma,rho=rho,outer.Maxiter=20,outer.tol=10^-6);
# # uses the default initializations for the covariance and precision matrices
# # now solve the problem for a smaller value of rho,
# # using the previous solution as warm-start
# rho.new=rho*.8;
# B.new<-dpglasso(Sigma,X=B$X,invX=B$invX,
#                 rho=rho.new,outer.Maxiter=20,outer.tol=10^-6);


# s here is non-zero cofficients
dgp_BIC <- function(X, Sigma, n, s){
  l <- sum(diag(Sigma %*% X)) - log(det(X))
  BIC <- -2*l + s*log(n)
  BIC
} 

dgp_AIC <- function(X, Sigma, n, s){
  l <- sum(diag(Sigma %*% X)) - log(det(X))
  AIC <- -2*l + s*2
  AIC
} 
  
dgp_path <- function(Sigma, step = 10, n = n){
  rho_list <- list()
  precision_list <- list()
  AIC_list <- list()
  q<-max(abs(Sigma[row(Sigma)> col(Sigma)]))
  invX <- X <- NULL
  for(i in 1:step){
    rho <- 0.8^(i)*(0.9*q) # follow the suggestion from the paper p12
    if(is.null(X) == TRUE) {
      res_tmp <- dpglasso::dpglasso(Sigma = Sigma, rho = rho, outer.tol = 0.005)
    } else {
      res_tmp <- dpglasso::dpglasso(Sigma = Sigma, X = X, invX =invX, rho = rho, outer.tol = 0.005)
    }
    invX <- res_tmp$invX
    X <- res_tmp$X
    s <- sum(X[upper.tri(X, diag=T)] >0)
    # BIC <- dgp_BIC(X = X, Sigma = Sigma, s = s, n = n)
    AIC <- dgp_AIC(X = X, Sigma = Sigma, s = s, n = n)
    rho_list <- append(rho_list, rho)
    precision_list <- append(precision_list, list(res_tmp$invX))
    AIC_list <- append(AIC_list, AIC)
  }
  list(rho_list = rho_list,
       precision_list = precision_list,
       AIC_list = AIC_list)
}

dgpGLASSO_method <- function(input_data, rho = NULL){
  Sigma=cov(input_data, input_data)
  if(is.null(rho)){
    rho_path <- dgp_path(Sigma, n = nrow(input_data))
    index <- which.min(unlist(rho_path$AIC_list))
    Sigma_i <- rho_path$precision_list[[index]]
  } else {
    # Compute Sigma^{-1}
    Sigma_i <- dpglasso::dpglasso(Sigma = Sigma, rho = rho, outer.tol = 0.05)$X
  }
  
  # Compute Signa^{-1/2}
  Sigma_isqrt <- msqrt(Sigma_i)
  
  # uncorrelated_data  
  uncorr_data=input_data%*%Sigma_isqrt
  
  list(uncorr_data = uncorr_data)
}

QUIC_method <- function(input_data, rho = 0.005){
  Sigma<-cov(input_data, input_data)
  Sigma_i <- QUIC::QUIC(S = Sigma, rho = rho, tol = 0.05)$W
  # Compute Signa^{-1/2}
  Sigma_isqrt <- msqrt(Sigma_i)
  
  # uncorrelated_data  
  uncorr_data=input_data%*%Sigma_isqrt
  
  list(uncorr_data = uncorr_data)
}

##################################################################################
## PCA dimension reduction method
##################################################################################

# PCA_method <- function(input_data,p = NULL, combine = FALSE) {
#   if(combine == TRUE){
#     pca_main <-prcomp(input_data[,1:p], retx = TRUE)
#     pca_inter <- prcomp(input_data[,-(1:p)], retx = TRUE)
#     uncorr_data <- cbind(SVD_method(input_data[,1:p])$uncorr_data, pca_inter$x)
#   } else {
#     pca_x <- prcomp(input_data, retx = TRUE)
#     uncorr_data <- pca_x$x  
#   }
#   
#   list(uncorr_data = uncorr_data)
# }


PCA_method <- function(input_data) {
  pca_x <- prcomp(input_data, retx = TRUE)
  uncorr_data <- pca_x$x
  list(uncorr_data = uncorr_data)
}

# PCA_method <- function(input_data) {
#   pca_x <- prcomp(input_data, retx = TRUE)
#   index <- pca_x$sdev >= 0.009
#   uncorr_data <- pca_x$x[,index]
#   list(uncorr_data = uncorr_data)
# }

##################################################################################
## using true value
##################################################################################
true_value_method <- function(input_data, Sigma=NULL, 
                              emp = FALSE, combine = NULL,
                              sigma_total_emp = NULL, sigma_main_emp = NULL){
  if (emp == TRUE) {
    if(combine == TRUE){
      Sigma <- sigma_total_emp
    } else {
      Sigma <- sigma_main_emp
    }
  }
  
  Sigma_isqrt <- invsqrt(Sigma)  
  
  # uncorrelated_data
  uncorr_data <- input_data%*%Sigma_isqrt
  
  list(uncorr_data = uncorr_data)
  
}