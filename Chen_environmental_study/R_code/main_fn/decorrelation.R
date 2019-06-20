
##################################################################################
## uncorrelated function
##################################################################################
uncorr_fn <- function(input_data, 
                      uncorr_method = SVD_method , 
                      uncorr_args = NULL,
                      dim_red_method = NULL, 
                      dim_red_args = NULL) {
  
  # dimension reduction 
  if(!is.null(dim_red_method)){
    args <- append(list(x = input_data), dim_red_args)
    input_data <- do.call(dim_red_method, args)
  }
  # decorrelation
  args <- append(list(input_data = input_data), uncorr_args) # generate the args for the uncorrelation function
  res <- do.call(uncorr_method, args)
  
  res$uncorr_data
}

##################################################################################
## SVD method of decorrelation method
##################################################################################

SVD_method <- function(input_data, p, main = FALSE, inter = FALSE, main_pro = NULL) {
  
  I_m <- diag(p)
  I_i <- diag(p*(p-1)/2)
  
  if(main == TRUE) # only decorrelating main effect
    I_m <- cov(input_data[,1:p])
  
  if(inter == TRUE) # only decorrelating inter effect
    I_i <- cov(input_data[,-(1:p)])
  
  if(any(main, inter) == TRUE)
    Sinvsqrt <- magic::adiag(invsqrt(I_m), invsqrt(I_i))
  else {
    Sigma <- cov(input_data,input_data)
    Sinvsqrt <- invsqrt(Sigma)
  } 
  
  # decorrelating the input_data
  uncorr_data=input_data%*%Sinvsqrt 
  
  # for main estimation
  if(!is.null(main_pro))
    uncorr_data <- uncorr_data[,(1:round(main_pro*p,0))]
  
  list(uncorr_data = uncorr_data)
}

##################################################################################
## GLASSO method of decorrelation method
##################################################################################

GLASSO_method <- function(input_data, rho){
  Sigma=cov(input_data, input_data)
  
  # Compute Sigma^{-1/2}
  Sinvsqrt=glasso::glasso(s = Sigma, rho = rho, approx = TRUE)$wi
  
  # uncorrelated_data  
  uncorr_data=input_data%*%Sinvsqrt
  
  list(uncorr_data = uncorr_data)
}
