# Dicker 2013
# the function is based on the Lee H Dicker's 2013 paper: Variance estimation i high-dimensional linear models
Dicker_2013 <- function(x, y, target = c("beta2","h2")[1]){
  # x <- std_fn(x) # note that we need to center the covariates 
  # n <- nrow(x) - 1 # note 
  n <- nrow(x)
  d <- ncol(x)
  if(d < n){
    return(rep(NA,4))
  }
  norm2_y <- sum(y^2) 
  norm2_xy <- sum((t(x)%*%y)^2)
  sigma2_hat <- (d + n + 1)/(n*(n+1))*norm2_y - 1/(n*(n+1))*norm2_xy
  tau2_hat <- -d/(n*(n+1))*norm2_y + 1/(n*(n+1))*norm2_xy
  r2_hat <- tau2_hat/(tau2_hat + sigma2_hat)
  phi2_1 <- 2*(d/n*(sigma2_hat + tau2_hat)^2 + sigma2_hat^2 + tau2_hat^2) 
  phi2_2 <- 2*((1+d/n)*(sigma2_hat + tau2_hat)^2 - sigma2_hat^2 + 3*tau2_hat^2)
  phi2_0 <- 2/(tau2_hat + sigma2_hat)^2 * ((1+d/n)*(tau2_hat + sigma2_hat)^2 - sigma2_hat^2)
  sigma2_var <- phi2_1/n
  tau2_var <- phi2_2/n
  r2_var <- phi2_0/n
  result <- c(sigma2_hat = sigma2_hat,
              tau2_hat = tau2_hat,
              sigma2_var = sigma2_var,
              tau2_var = tau2_var,
              r2_hat = r2_hat,
              r2_var = r2_var)
  if(target == "beta2"){
    result[1:4]
  } else if (target == "h2")
    result[5:6]
}