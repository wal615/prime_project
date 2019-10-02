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

dgpGLASSO_method <- function(input_data, rho = 0.01){
  Sigma=cov(input_data, input_data)
  # Compute Sigma^{-1}
  Sigma_i <- dpglasso::dpglasso(Sigma = Sigma, rho = rho, outer.tol = 0.05)$X

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