library(rrBLUP)
GCTA_rr <- function(x,y, target = c("beta2","h2")[1]){
  x <- std_fn(x)
  res <- mixed.solve(y = y, Z = x)
  # in GCTA setting we have Vu = Vg/p, so to estimate Vg we need multiple p
  G <- res$Vu * ncol(x)
  if(target == "h2"){
    G <- G/(G + res$Ve)
  }
  G
}