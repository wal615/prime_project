signal <- function(x, betam, betai){
  result <- numeric(4)
  p <- ncol(x)
  x <- gene_model_data(x, p)
  sigma_main_emp <- var(x$b_m)
  sigma_inter_emp <- var(x$b_i)
  sigma_cov_emp <- cov(x$b_m, x$b_i)
  sigma_total_emp <- cbind(x$b_m, x$b_i) %>% var(.)
  result[1] <- t(betam)%*%sigma_main_emp%*%betam
  result[2] <- t(betai)%*%sigma_inter_emp%*%betai
  result[3] <- t(betam)%*%sigma_cov_emp%*%betai
  betat <- c(betam, betai)
  result[4] <- t(betat)%*%sigma_total_emp%*%betat
  result
}