single_var <- function(x){
  result <- numeric(3)
  if(ncol(x) >2) stop("dim of x is n * 2")
  result[1:2] <- diag(var(x))
  result[3] <- cov(x)[1,2]
  result
}