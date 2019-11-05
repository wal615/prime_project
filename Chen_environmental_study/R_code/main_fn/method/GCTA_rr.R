library(rrBLUP)
GCTA_rr <- function(x,y){
  x <- std_fn(x)
  res <- mixed.solve(y = y, Z = x)
  G <- res$Vu * ncol(x)
  G
}