##################################################################################
## SVD dimension reduction method
##################################################################################

# SVD_dim_reduction <- function(x, reduce_coef) {
# 
#   if (reduce_coef > 1) {
#     warning("no dimension reduction is used")
#     return(x)
#   }
# 
#   n <- nrow(x)
#   p <- ncol(x)
#   dim <- round(min(n,p) * reduce_coef, 0)
#   svd_x <- svd(x, nu = dim, nv = dim)
#   x_r <- (svd_x$u) %*% diag(svd_x$d[1:dim]) %*% t(svd_x$v[1:dim,]) # ignore the right U_3 part
#   x_r
# }

SVD_dim_reduction <- function(x, reduce_coef) {

  if (reduce_coef > 1) {
    warning("no dimension reduction is used")
    return(x)
  }

  n <- nrow(x)
  p <- ncol(x)
  dim <- round(min(n,p) * reduce_coef, 0)
  # dim <- round(p * reduce_coef, 0)
  svd_x <- svd(x)
  x_r <- (svd_x$u) %*% diag(svd_x$d) %*% t(svd_x$v[1:dim,]) # ignore the right U_3 part
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