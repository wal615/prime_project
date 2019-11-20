
##################################################################################
## generate main effect
##################################################################################
generate_main <- function(p, additional_args) {
  beta <- rnorm(p, mean =0, additional_args$main_fixed_var %>% sqrt(.))
  beta
}

##################################################################################
## generate main effect random
##################################################################################
generate_main_random <- function(p, additional_args) {
  beta <- rnorm(p, mean =0, additional_args$main_random_var %>% sqrt(.)) 
  beta
}

##################################################################################
## generate interaction effect
##################################################################################
generate_inter <- function(p, additional_args) {
  beta <- matrix(rnorm(p*p,m=0,sd=additional_args$inter_fixed_var %>% sqrt(.)),ncol=p) 
  beta[upper.tri(beta, diag = FALSE)]
}

##################################################################################
## generate interaction effect random
##################################################################################
generate_inter_random <- function(p, additional_args) {
  beta <- matrix(rnorm(p*p,m=0,sd=additional_args$inter_random_var %>% sqrt(.)),ncol=p) 
  beta[upper.tri(beta, diag = FALSE)]
}

##################################################################################
## generate sparsity property for the coefficients (main)
##################################################################################
sparsify_coeff <- function(main_coeff, inter_coeff, sparse_ratio = 0.5){
  p <- length(main_coeff)
  index_main <- ((1/sparse_ratio)*1:floor(p*sparse_ratio)) %>% round(.,0)
  index_inter <- grep(paste("^",main_coeff[index_main], ":", collapse="|", sep = ""), 
                      inter_coeff, value = FALSE) # based on the variable names 
  list(index_main = index_main,
       index_inter = index_inter)
}

##################################################################################
## generate AR correlation matrix
##################################################################################

autocorr.mat <- function(p = 100, rho = 0.9) {
  mat <- diag(p)
  return(rho^abs(row(mat)-col(mat)))
}

##################################################################################
## generate UN correlation matrix
##################################################################################

unstr_corr.mat <- function(p, k = 10) {
  set.seed(123)
  P <- matrix(runif(p*k), ncol=p) # k control the magnitude of off-diagonal elements
  cor_str <- crossprod(P) + diag(runif(p))
  cor_str <- diag(1/sqrt(diag(cor_str))) %*% cor_str %*% diag(1/sqrt(diag(cor_str)))
  cor_str
}

##################################################################################
## generate emperical correlation matrix from a real datasetdataset
##################################################################################

real_data_corr.mat <- function(path,...) {
  real_data <- read.csv(path,...)
  cor(real_data)
}


generate_normal <- function(n, p, rho = NULL, sig_coef = 1, 
                            structure = c("cs","un","ar", "I")[1], 
                            pre_cor = NULL){
    if(structure == "cs"){
      cor_str <- matrix(rep(rho,p^2), ncol = p)
      diag(cor_str) <- 1
      cor_str <- cor_str * sig_coef
      
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = cor_str^2)
    }
    
    if(structure == "un"){
      if(class(pre_cor) == "list") {pre_cor <- pre_cor[[1]]}
      cor_str <- pre_cor * sig_coef # to keep the covariance matrix same for each simulation iterations
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = cor_str^2)
    }
    
    if(structure == "ar"){
      cor_str <- autocorr.mat(p, rho)
      cor_str <- cor_str * sig_coef
      
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = cor_str^2)
    }
    
    if(structure == "I"){
      
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = diag(p))
    }
    
    b <- x
    colnames(b) <- paste0("X", 1:ncol(b))
    
    attributes(b) <- append(attributes(b), 
                            list(x_dist = "normal", 
                                 str = structure,
                                 corr = rho))
    b %>% std_fn(.)
  }


##################################################################################
## generate correlated chi-square
##################################################################################
generate_chi <- function(n, p, rho = NULL, sig_coef = 1, 
                         structure = c("cs","un","ar", "I")[1], 
                         pre_cor = NULL) {
  # generate individual chi_square
  p_normal <- p
  if(structure == "cs"){
    cor_str <- matrix(rep(rho,p_normal^2), ncol = p_normal)
    diag(cor_str) <- 1
    cor_str <- cor_str * sig_coef # 
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = cor_str) 
  }
  
  if(structure == "un"){
    if(class(pre_cor) == "list") {pre_cor <- pre_cor[[1]]}
    cor_str <- pre_cor * sig_coef # to keep the covariance matrix same for each simulation iterations
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = cor_str) 
  }
  
  if(structure == "ar"){
    cor_str <- autocorr.mat(p_normal, rho)
    cor_str <- cor_str * sig_coef
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = cor_str)
  }
  
  if(structure == "I"){
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = diag(p_normal))
  }
  
  b <- x^2
  colnames(b) <- paste0("X", 1:ncol(b))
  
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "chi", 
                               str = structure,
                               corr = rho))
  b %>% std_fn(.)
}

##################################################################################
## subset PCB data
##################################################################################

generate_PCB <- function(data_path, n, p = NULL, data_name = NULL, structure) {

  data <- fread(data_path)

  # subset 
  n_total <- nrow(data)
  index <- sample(1:n_total, n, replace = FALSE)
  data <- data[index,]
  
  # covaraites
  x <- data %>% data.matrix(.)
  
  # standardization
  # x <- apply(x, 2, rank)
  x <- x %>% std_fn(.)
  
  # add distribution attributes
  attributes(x) <- append(attributes(x), 
                          list(x_dist = data_name))
  x

}

generate_real_test <- function(data_path, pro, data_name=NULL, resp_name = "y", tran_fn_y, tran_fn_x) {
  if(class(tran_fn_y) == 'list') tran_fn_y <- tran_fn_y[[1]] # incase the function is pass as a list
  if(class(tran_fn_x) == "list") tran_fn_x <- tran_fn_x[[1]] # incase the function is pass as a list
  
  data <- read.csv(data_path,header = TRUE, stringsAsFactors = FALSE)
  # subset 
  n <- nrow(data)
  if(pro <1){
    index <- sample(1:n, round(pro*n,0), replace = FALSE)
    data <- data[index,]
    }
  
  # covaraites
  x <- data[,!(colnames(data) %in% resp_name)] %>% data.matrix(.) %>% apply(.,2, tran_fn_x)
  
  # add distribution attributes
  attributes(x) <- append(attributes(x), 
                          list(x_dist = data_name))
  # response
  y <- data[,(resp_name), drop = FALSE] %>% data.matrix(.)  %>% tran_fn_y(.) %>% matrix(., ncol = 1)
  
  b <- list(x = x, y = y)
}


generate_sub <- function(data, d, n, bs = c("leave-d","leave-1","leave-1-2","bs")[1], iteration = NULL) {
  if(bs == "bs") {
    index <- sample(1:n, replace = TRUE)
  } else if (bs == "leave-d") {
    index <- sample(1:n, n-d, replace = FALSE)
  } else if (bs == "leave-1"|bs == "leave-1-2") {
    index <- (1:n)[-iteration]
  } else if (bs == "full"){
    index <- 1:n
  }
  sub_data <- lapply(data, FUN = function(x) x[index, ,drop = FALSE])
  sub_data
}


gene_model_data <- function(b_raw, p, combine = FALSE){
  # Standardized main covariates
  b_m <- b_raw 
  b_i <- matrix(0,nrow = nrow(b_raw))
  if(combine == TRUE){
    b <- b_raw %>% add_inter(.) 
    b_m <- b[,1:p, drop = FALSE]
    b_i <- b[,-(1:p), drop = FALSE]
  }
  list(b_m = b_m,
       b_i = b_i)
}


est_model_data <- function(b_raw, y, p, 
                           combined, 
                           uncorr_method, uncorr_args,
                           sparse_uncorr_method, sparse_uncorr_args, 
                           uncorre = FALSE){
  # Standardized main covariates
  b_m <- b_raw
  
  if(combine == TRUE){
    b <- b_raw %>% add_inter(.) 
    b_m <- b[,1:p, drop = FALSE]
    b_i <- b[,-(1:p), drop = FALSE]
    b_final <- cbind(b_m, b_i)
  } else {
    b_final <- b_m
  }
  
  # Uncorrelated data
  if(uncorre == TRUE) {
    s_final <- uncorr_fn(b_final, uncorr_method, uncorr_args, sparse_uncorr_method, sparse_uncorr_args)
  } else {
    s_final <- NA
  }
  list(b_final = b_final,
       s_final = s_final,
       y = y)
}
