library("sas7bdat")
library("MASS")
##################################################################################
## generate main effect
##################################################################################
generate_main <- function(p) {
  betam=rnorm(p, mean =0, sd =0.5) # main_effect ~ N(0,0.5)
  betam[2*c(1:floor(p/2))]=0  # mimic the zero coefficients
  betam
}

##################################################################################
## generate main effect
##################################################################################
generate_inter <- function(p, interaction) {
  if(interaction==0) {
    betai <- matrix(0,ncol=p, nrow = p)
  } else {
    betai <- matrix(rnorm(p*p,m=0,sd=0.1),ncol=p) # interaction_effect ~ N(0,0.1)
    betai[lower.tri(betai, diag = TRUE)] <- 0 # the number of interaction terms is {p*(p-1)}/2
  }
  betai
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
## generate correlated chi-square
##################################################################################
generate_chi <- function(n, p, rho = NULL, sig_coef = 1, 
                         structure = c("cs","un","ar")[1], 
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
  
  b <- lapply(X = index_list, 
              FUN = function(data, index) {rowSums(data[,index, drop = FALSE])}, data = x) %>%
    Reduce(cbind, x = .)
  
  b <- model.matrix(~.*.+0, data.frame(b)) # adding the interaction term
  
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "chi", 
                               str = structure,
                               corr = rho))
  b
}

##################################################################################
## subset PCB data
##################################################################################
generate_PCB <- function(pro, p) {
  # read the PCB data
  # a <- read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
  # b <- data.matrix(a[,2:(p+1)], rownames.force = NA)
  a <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/pcb_99_13_no_missing.csv")
  b <- data.matrix(a[,-1], rownames.force = NA)
  n <- nrow(b)
  index <- sample(1:n, round(pro*n,0), replace = FALSE)
  b <- b[index,]
  
  b <- model.matrix(~.*.+0, data.frame(b)) 
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "PCB"))
  b
}

##################################################################################
## subset of chi-square
##################################################################################
generate_chi_sub <- function(pro) {
  # generate a chi-square
  cor_str <- matrix(rep(0.5,34^2), ncol = 34)
  diag(cor_str) <- 1
  x <- mvrnorm(n = 1000,
               mu = rep(0,34),
               Sigma = cor_str)
  b <- x^2
  n <- nrow(b)
  index <- sample(1:n, round(pro*n,0), replace = FALSE)
  b <- b[index,]
  
  b <- model.matrix(~.*.+0, data.frame(b)) 
  
  attributes(b) <- append(attributes(b), 
                          list(x_dist = "Chi", 
                               pro = pro))
  b
}



##################################################################################
## simulation function with simulated covariate
##################################################################################


simulation_fn <- function(
                          p,
                          tran_fun,
                          combine = FALSE,
                          main_fixed = TRUE,
                          inter_fixed = TRUE,
                          generate_data,
                          gene_args,
                          brep, 
                          nrep,
                          uncorr_method = NULL,
                          uncorr_args = NULL,
                          dim_red_method = NULL,
                          dim_red_args = NULL,
                          interaction = 0, 
                          interaction_m = 0, 
                          corrected_main = FALSE,
                          main_pro = NULL,
                          seed = 0, 
                          cores = 1) {
  if (cores == 1) 
    foreach::registerDoSEQ() 
  else 
    doParallel::registerDoParallel(cores = cores) # setting cores
  
  if(seed != 0) set.seed(seed) # set seed for foreach
  
  
  # Generate main betas
  if(main_fixed){
    betam <- generate_main(p)
  }
  
  
  # Generate interaction gammas
  if(inter_fixed){
    betai <- generate_inter(p, interaction)
  }
  
  result_raw <- foreach(ibrep = 1:brep, .combine = rbind, .verbose = TRUE, .errorhandling = "remove") %dorng%   {
    # Initial output 
    result_tmp <- matrix(0, nrow = nrep, ncol = 6)

    # Generate covariates  
    b_raw <- do.call(generate_data, gene_args)
    
    # Standardized covariates
    # combined the all the attributes to b so we could plot them by the attributes
    additional <- list(main_fixed = main_fixed, 
                       inter_fixed = inter_fixed,
                       x_dist = attributes(b_raw)$x_dist)
    additional <- append(additional, c(as.list(gene_args), as.list(uncorr_args)))
    
    additional$pre_cor <- NULL
    
    b <- std_fn(b = b_raw,
                p = ncol(b_raw),
                tran_FUN = tran_fun)
    b_m <- b[,1:p]
    b_i <- b[,-(1:p)]
    
    # Generate main betas
    if(!main_fixed){
      betam <- generate_main(p)
    }
    
    # Generate interaction gammas
    if(!inter_fixed){
      betai <- generate_inter(p, interaction)
    }

    # Generate the signals
    signalm <- b_m%*%betam
    signali <- b_i%*%betai[upper.tri(betai, diag = FALSE)]
    result_tmp[, 2]=var(signali)
    
    if(combine == TRUE){
      result_tmp[, 1]=var(signalm + signali)
      b_final <- cbind(b_m, b_i)
    } else {
      result_tmp[, 1]=var(signalm)
      b_final <- b_m
    }
    
    # Uncorrelated data
     if(corrected_main == TRUE){
      x<- uncorr_fn(cbind(b_m, b_i), uncorr_method, uncorr_args, dim_red_method, dim_red_args)
     } else {
       x <- uncorr_fn(b_final, uncorr_method, uncorr_args, dim_red_method, dim_red_args)
     }
    
    # Estimating total effects with iterations
    for(irep in 1:nrep){
      # Generate health outcome given fixed random effects
      y=signalm+signali+rnorm(length(signalm),sd=4)
      
      fit=Yang(y,b_final,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT

      # Call the GCTA method
      fit=Yang(y,x,interact = interaction_m)
      result_tmp[irep,5] <- fit$G
      result_tmp[irep,6] <- fit$RACT
    }
    
    
    # save the result
    result_final <- rbind(apply(result_tmp, 2, mean), apply(result_tmp, 2,sd))
    result_final <- data.frame(result_final, additional) ## adding attributes as plot categories
    
  }
  attributes(result_raw)$rng <- NULL # rm the random sampling info
  colnames(result_raw)[1:6] <- c("true_main", "true_interaction", "GCTA_main", "GCTA_interaction", "prop_main", "prop_interaction")
  
  if(combine == TRUE){
    colnames(result_raw)[1:6] <- c("true_total", "true_interaction", "GCTA_total", "GCTA_interaction", "prop_total", "prop_interaction")
  }
  result_raw
}


