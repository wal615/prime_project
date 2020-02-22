##################################################################################
## SVD dimension reduction method
##################################################################################

SVD_dim_reduction <- function(x, reduce_coef=1, last = FALSE, method) {
  n <- nrow(x)
  p <- ncol(x)
  if(last==TRUE){
    dim <- n - 1
  } else {
    dim <- round(n * reduce_coef, 0)  
  }
  svd_x <- svd(x)
  x_r <- (svd_x$u[,1:dim]) %*% diag(svd_x$d[1:dim]) %*% t(svd_x$v[1:dim,1:dim]) # ignore the right U_3 part
  x_r
}



##################################################################################
## p-value variable selection dimension reduction method
##################################################################################
beta_lm = function(X,Y,m,n,intercept=TRUE){
  beta_seq = rep(NA,m)
  beta_se_seq = rep(NA,m)
  if (intercept){
    X0 = as.matrix(rep(1,n))
    for (j in 1:m){
      result = summary(lm(Y ~ X0 + X[,j] - 1))
      beta_seq[j] = result$coefficients[2]
      beta_se_seq[j] = result$coefficients[4]
    }
  }
  else{
    for (j in 1:m){
      result = summary(lm(Y ~ X[,j] - 1))
      beta_seq[j] = result$coefficients[1]
      beta_se_seq[j] = result$coefficients[2]
    }
  }
  return(list(beta_est = beta_seq, beta_se = beta_se_seq))
}


pvalue_dim_reduction <- function(x, y, index, method) {
  if(length(index) == 0){
    return(x[index,])
  }
  x_dim <- x[-index,]
  y_dim <- y[-index,]
  x_select <- x[index,]
  beta_estimate <- beta_lm(x_dim,y_dim,ncol(x_dim),
                           nrow(x_dim),intercept = FALSE)
  beta_hat_seq = beta_estimate$beta_est
  beta_se_seq = beta_estimate$beta_se
  select_index =abs(beta_hat_seq/beta_se_seq)  %>% abs(.) %>% order(.,decreasing = T)
  select_index <- select_index[1:(round(0.4*500,0))]
  x_r <- x_select[,select_index]
  x_r
}