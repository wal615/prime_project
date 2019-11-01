library(rrBLUP)
GCTA_rr <- function(x,y){
  res <- mixed.solve(y = y, Z = x)
  G <- res$Vu * ncol(x)
  G
}