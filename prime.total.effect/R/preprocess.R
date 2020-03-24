##################################################################################
## standardized function with tranformation features
##################################################################################
# default 
#' @export
std.fn <- function(b){
  # mean-variance standardized for main effect
  for(k in 1:ncol(b)){
    me=mean(b[,k])
    std=sqrt(var(b[,k]))
    b[,k]=(b[,k]-me)/std
  }

  b
}

trans <- function(b, tran.FUN = null.tran){
  # transform 
  b <- apply(b, 2, tran.FUN, ...)
  b
}

#' @export
add.inter <- function(b){
  # interaction 
  b <- model.matrix(~.*.+0, data.frame(b))# adding the interaction term
  b
}

##################################################################################
## Null transformation function
##################################################################################

null.tran <- function(y) {
  y
}

##################################################################################
## log transformation function
##################################################################################

log.tran <- function(y) {
  log(y)
}

##################################################################################
## square root transformation function
##################################################################################

sqrt.tran <- function(y) {
  sqrt(y)
}

##################################################################################
## cox.box transformation function
##################################################################################

box.cox.tran <- function(y) {
  bc <- MASS::boxcox(y~1, plotit = FALSE)
  lambda <- bc$x[which.max(bc$y)]  # find the optimal lambda based on lm model
  if(lambda == 0) bc.y <- log(y) 
  else bc.y <- (y^lambda - 1)/lambda
  bc.y
}

##################################################################################
## Rank transformation function
##################################################################################

rank.tran <- function(y) {
  rank(y)
}

##################################################################################
## Normal quantile transformation function
##################################################################################

norm.quantile.tran <- function(y) {
  emprircal.cdf <- ecdf(y) # empricial dist 
  y[which.min(y)] <- y[which.min(y)] + 0.0001 # modify the max and min values to avoid Inf 
  y[which.max(y)] <- y[which.max(y)] - 0.0001
  y <- emprircal.cdf(y) %>% qnorm(.) 
}

##################################################################################
## Normal score transformation
##################################################################################
norm.score.tran <- function(y) {
  (rank(y)/(length(y)+1)) %>% qnorm(.)
}

##################################################################################
## Categorized transformation function
##################################################################################

categorized.tran <- function(x, by) {
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
  eign.value <- Seign$values
  eign.value.rec.sqrt <- ifelse(eign.value > tol, 1/sqrt(eign.value), 0)
  Seign$vectors %*% diag(eign.value.rec.sqrt) %*% t(Seign$vectors)
}

##################################################################################
## inverse
##################################################################################

inv <- function(Sigma, tol = 1e-15) {
  ## get rid of zero egienvalues
  Seign <- eigen(Sigma)
  eign.value <- Seign$values
  eign.value.sqrt <- if.else(eign.value > tol, 1/eign.value, 0)
  Seign$vectors %*% diag(eign.value.sqrt) %*% t(Seign$vectors)
}


##################################################################################
## square-root
##################################################################################

msqrt <- function(Sigma) {
  ## get rid of zero egienvalues
  Seign <- eigen(Sigma)
  eign.value <- Seign$values
  if(any(eign.value<0)) stop("cov matrix has to be PSD")
  eign.value.sqrt <- sqrt(eign.value)
  Seign$vectors %*% diag(eign.value.sqrt) %*% t(Seign$vectors)
}

mdist <- function(A,B){
  sum((A-B)^2) 
}

##################################################################################
## square-root
##################################################################################
tran.add.noise <- function(x) {
  p <- ncol(x)
  n <- nrow(x)
  x + matrix(rnorm(p*n, sd = sqrt(0.1)), ncol =p)
}

tran.com <- function(x) {
  p <- ncol(x)
  t <- 8
  col.tran <- diag(t)
  col.tran[upper.tri(col.tran)] <- 1
  col.tran <- Matrix::bdiag(col.tran, diag(p-t)) %>% as.matrix(.)
  x %*% col.tran
}
