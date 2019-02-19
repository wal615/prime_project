##################################################################################
## SVD dimension reduction method
##################################################################################

SVD_dim_reduction <- function(x, reduce_coef) {
  n <- nrow(x)
  p <- ncol(x)
  dim <- round(min(n,p) * reduce_coef, 0) 
  svd_x <- svd(x, nu = dim, nv = dim)
  x_r <- (svd_x$u) %*% diag(svd_x$d[1:dim]) %*% t(svd_x$v[1:dim,]) # ignore the right U_3 part 
  x_r
}


##################################################################################
## PCA dimension reduction method
##################################################################################

PCA_dim_reduction <- function(x) {
  pca_x <- prcomp(x, retx = TRUE)
  x_r <- pca_x$x
  x_r
}