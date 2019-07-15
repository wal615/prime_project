
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

SVD_method <- function(input_data) {
  
  Sigma <- cov(input_data,input_data)
  Sinvsqrt <- invsqrt(Sigma)
  
  # decorrelating the input_data
  uncorr_data=input_data%*%Sinvsqrt 
  
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
