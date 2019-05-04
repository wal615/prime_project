
##################################################################################
## generate main effect
##################################################################################
generate_main <- function(p, additional_args) {
  beta <- rnorm(p, mean =0, additional_args$main_fixed_var %>% sqrt(.))
  3*beta/norm(beta,"2")*sqrt((p-1)/2) # normalized and multiple with p to have equal magnitude 
                           # interaction coefficients
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
  if(norm(beta[upper.tri(beta, diag = FALSE)],"2") >0) {
    beta <- 5*beta[upper.tri(beta, diag = FALSE)]/norm(beta[upper.tri(beta, diag = FALSE)],"2") # modify the strength of beta
    return(beta)
  } else {
    return(beta[upper.tri(beta, diag = FALSE)])
  }
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
  con_str <- crossprod(P) + diag(runif(p))
  con_str <- diag(1/sqrt(diag(con_str))) %*% con_str %*% diag(1/sqrt(diag(con_str)))
  con_str
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
                            chi_coef = 1, 
                            pre_cor = NULL){
    if(structure == "cs"){
      cor_str <- matrix(rep(rho,p^2), ncol = p)
      diag(cor_str) <- 1
      cor_str <- cor_str * sig_coef
      
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = cor_str)
    }
    
    if(structure == "un"){
      if(class(pre_cor) == "list") {pre_cor <- pre_cor[[1]]}
      con_str <- pre_cor * sig_coef # to keep the covariance matrix same for each simulation iterations
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = con_str)
    }
    
    if(structure == "ar"){
      con_str <- autocorr.mat(p, rho)
      con_str <- con_str * sig_coef
      
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = con_str)
    }
    
    if(structure == "I"){
      
      x <- mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = diag(p))
    }
    
    b <- x
    colnames(b) <- paste0("X", 1:ncol(b))
    
    attributes(b) <- append(attributes(b), 
                            list(x_dist = "Normal", 
                                 str = structure,
                                 corr = rho))
    b
  }


##################################################################################
## generate correlated chi-square
##################################################################################
generate_chi <- function(n, p, rho = NULL, sig_coef = 1, 
                         structure = c("cs","un","ar", "I")[1], 
                         chi_coef = 1, 
                         pre_cor = NULL) {
  # generate individual chi_square
  p_normal <- p*chi_coef
  if(structure == "cs"){
    cor_str <- matrix(rep(rho,p_normal^2), ncol = p_normal)
    diag(cor_str) <- 1
    cor_str <- cor_str * sig_coef
    
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = cor_str)
  }
  
  if(structure == "un"){
    if(class(pre_cor) == "list") {pre_cor <- pre_cor[[1]]}
    con_str <- pre_cor * sig_coef # to keep the covariance matrix same for each simulation iterations
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = con_str)
  }
  
  if(structure == "ar"){
    con_str <- autocorr.mat(p_normal, rho)
    con_str <- con_str * sig_coef
    
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = con_str)
  }
  
  if(structure == "I"){
    
    x <- mvrnorm(n = n,
                 mu = rep(0,p_normal),
                 Sigma = diag(p_normal))
  }
  
  x <- x^2
  
  # combine different chi square to get different degree of freedom
  if(chi_coef == 1) {
    len_index <- p # for later the while condition 
    index_p <- sample(1:p)
  }
  else len_index <- 0
  
  while(len_index < p) {
    index_p <- sample(1:p, p_normal, replace = TRUE)  
    len_index <- unique(index_p) %>% length(.)
  } # make sure we sample all p different groups with replacement
  index_list <- split(1:p_normal, index_p)
  
  # generate the chi-square with specificed df
  b <- lapply(X = index_list, 
              FUN = function(data, index) {rowSums(data[,index, drop = FALSE])}, data = x) %>%
    Reduce(cbind, x = .)
  colnames(b) <- paste0("X", 1:ncol(b))
  
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "chi", 
                               str = structure,
                               corr = rho))
  b
}

##################################################################################
## subset PCB data
##################################################################################
generate_PCB <- function(data_path, pro) {
  b <- read.csv(data_path, stringsAsFactors = FALSE)
  
  # subset b 
  n <- nrow(b)
  index <- sample(1:n, round(pro*n,0), replace = FALSE)
  b <- b[index,]
  
  # add distribution attributes
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "PCB"))
  b
}

##################################################################################
## real data 
##################################################################################

generate_real <- function(data_path, pro, data_name=NULL) {
  data <- read.csv(data_path)
  
  # subset 
  n <- nrow(data)
  index <- sample(1:n, round(pro*n,0), replace = FALSE)
  data <- data[index,]
  
  # covaraites
  x <- data[,!(colnames(data) %in% "y")]  %>% data.matrix(.)
  # add distribution attributes
  attributes(x) <- append(attributes(x), 
                          list(x_dist = data_name))
  
  # response
  y <- data[,"y", drop = FALSE]  %>% data.matrix(.)
  b <- list(x = x, y = y)
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



generate_sub <- function(data, pro, n, bs = FALSE) {
  if(bs == TRUE) {
    index <- sample(1:n, replace = TRUE)
  } else {
    index <- sample(1:n, round(pro*n,0), replace = FALSE)
  }
  
  sub_data <- lapply(data, FUN = function(x) x[index, ,drop = FALSE])
  sub_data
}

generate_std_decorr <- function(b_raw, p, inter_std, combined, uncorr_method, uncorr_args, dim_red_method, dim_red_args){
  # Standardized main covariates
  b <- b_raw %>% std_fn(.) %>% add_inter(.)
  b_m <- b[,1:p]
  b_i <- b[,-(1:p)]
  # center the main/interaction terms
  if(inter_std == TRUE)
    b_i <- std_fn(b = b_i)
  
  if(combine == TRUE){
    b_final <- cbind(b_m, b_i)
  } else {
    b_final <- b_m
  }
  
  # Uncorrelated data
  x <- uncorr_fn(b_final, uncorr_method, uncorr_args, dim_red_method, dim_red_args)
  
  list(b_final = b_final,
       b_m = b_m,
       b_i = b_i,
       x = x)
}
