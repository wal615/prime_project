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
    betai <- 0
  } else {
    betai <- matrix(rnorm(p*p,m=0,sd=0.1),ncol=p) # interaction_effect ~ N(0,0.1)
    betai[lower.tri(betai, diag = TRUE)] <- 0 # the number of interaction terms is {p*(p-1)}/2
  }
  betai
}

##################################################################################
## subset PCB data
##################################################################################
generate_PCB <- function(pro) {
  # read the PCB data
  a <- read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
  b <- data.matrix(a[,2:35], rownames.force = NA)
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


simulation_fn <- function(n,
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
                          interaction = 0, 
                          interaction_m = 0, 
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
    } else {
      result_tmp[, 1]=var(signalm)
    }
    

    # Estimating total effects with iterations
    for(irep in 1:nrep){
      # Generate health outcome given fixed random effects
      y=signalm+signali+rnorm(n,sd=4)
      
      fit=Yang(y,b_final,interact = interaction_m)
      result_tmp[irep,3] <- fit$G
      result_tmp[irep,4] <- fit$RACT
      
      # uncorrelated data 
      x <- uncorr_fn(b_final, uncorr_method, uncorr_args)
      
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


