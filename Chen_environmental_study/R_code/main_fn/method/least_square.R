least_square <- function(x, y){
  data <- data.frame(x =x , y = y)
  lm_fit <- lm(y ~. + 0, data = data)
  sigma_hat <- sigma(lm_fit)
  total_effect <- var(y) - (sigma_hat)^2
  total_effect
}

least_square_e <- function(x, y){
  data <- data.frame(x =x , y = y)
  lm_fit <- lm(y ~. + 0, data = data)
  sigma_hat <- sigma(lm_fit)
  result <- (sigma_hat)^2
  result
}