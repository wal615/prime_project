##################################################################################
## standardized function with tranformation features
##################################################################################
# default 
std_fn <- function(b){
  # mean-variance standardized for main effect
  for(k in 1:ncol(b)){
    me=mean(b[,k])
    std=sqrt(var(b[,k]))
    b[,k]=(b[,k]-me)/std
  }

  b
}

trans <- function(b, tran_FUN = null_tran){
  # transform 
  b <- apply(b, 2, tran_FUN, ...)
  b
}

add_inter <- function(b){
  # interaction 
  b <- model.matrix(~.*.+0, data.frame(b))# adding the interaction term
  b
}

##################################################################################
## Null transformation function
##################################################################################

null_tran <- function(y) {
  y
}

##################################################################################
## log transformation function
##################################################################################

log_tran <- function(y) {
  log(y)
}

##################################################################################
## square root transformation function
##################################################################################

sqrt_tran <- function(y) {
  sqrt(y)
}

##################################################################################
## cox_box transformation function
##################################################################################

box_cox_tran <- function(y) {
  bc <- MASS::boxcox(y~1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]  # find the optimal lambda based on lm model
  if(lambda == 0) bc_y <- log(y) 
  else bc_y <- (y^lambda - 1)/lambda
  bc_y
}

##################################################################################
## Rank transformation function
##################################################################################

rank_tran <- function(y) {
  rank(y)
}

##################################################################################
## Normal quantile transformation function
##################################################################################

norm_quantile_tran <- function(y) {
  emprircal_cdf <- ecdf(y) # empricial dist 
  y[which.min(y)] <- y[which.min(y)] + 0.0001 # modify the max and min values to avoid Inf 
  y[which.max(y)] <- y[which.max(y)] - 0.0001
  y <- emprircal_cdf(y) %>% qnorm(.) 
}

##################################################################################
## Normal score transformation
##################################################################################
norm_score_tran <- function(y) {
  (rank(y)/(length(y)+1)) %>% qnorm(.)
}

##################################################################################
## Categorized transformation function
##################################################################################

categorized_tran <- function(x, by) {
  breaks <- c(quantile(x, probs = seq(0, 1, by = by))) %>% unique(.)
  cut(x = x, 
      breaks = breaks,
      labels = 1:(length(breaks)-1), 
      include.lowest = TRUE) %>% 
    as.character(.) %>%
    as.numeric(.)
}


##################################################################################
## inverse and square-root
##################################################################################

invsqrt <- function(Sigma, tol = 1e-15) {
  ## get rid of zero egienvalues
  Seign <- eigen(Sigma)
  eign_value <- Seign$values
  eign_value_rec_sqrt <- if_else(eign_value > tol, 1/sqrt(eign_value), 0)
  Seign$vectors %*% diag(eign_value_rec_sqrt) %*% t(Seign$vectors)
}

##################################################################################
## inverse
##################################################################################

inv <- function(Sigma, tol = 1e-15) {
  ## get rid of zero egienvalues
  Seign <- eigen(Sigma)
  eign_value <- Seign$values
  eign_value_sqrt <- if_else(eign_value > tol, 1/eign_value, 0)
  Seign$vectors %*% diag(eign_value_sqrt) %*% t(Seign$vectors)
}


##################################################################################
## square-root
##################################################################################

msqrt <- function(Sigma) {
  ## get rid of zero egienvalues
  Seign <- eigen(Sigma)
  eign_value <- Seign$values
  if(any(eign_value<0)) stop("cov matrix has to be PSD")
  eign_value_sqrt <- sqrt(eign_value)
  Seign$vectors %*% diag(eign_value_sqrt) %*% t(Seign$vectors)
}

mdist <- function(A,B){
  sum((A-B)^2) 
}

##################################################################################
## square-root
##################################################################################
tran_add_noise <- function(x) {
  p <- ncol(x)
  n <- nrow(x)
  x + matrix(rnorm(p*n, sd = sqrt(0.1)), ncol =p)
}

tran_com <- function(x) {
  p <- ncol(x)
  t <- 8
  col_tran <- diag(t)
  col_tran[upper.tri(col_tran)] <- 1
  col_tran <- Matrix::bdiag(col_tran, diag(p-t)) %>% as.matrix(.)
  x %*% col_tran
}
