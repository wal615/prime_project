# Following simulation is to test if under normal distribution, we can estimate the interaction effect unbaisly by proposed method

setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_fixed_random/normal_fixed_random_simulation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)


n <- 1000
p <- 34
rho <- seq(0.1,0.9,0.1)
Sigma_str <- as.list(numeric(9))
names(Sigma_str) <- paste0("correlation_", rho)

for(i in (1:length(Sigma_str))){
  cor_str <- matrix(rep(rho[i],34^2), ncol = 34)
  diag(cor_str) <- 1
  Sigma_str[[i]] <- cor_str
}




test_result <- mapply(FUN = simulation_fn, 
                      Sigma = Sigma_str[1:5],
                      MoreArgs = list(n = n,
                                      p = p, 
                                      main_fixed = TRUE, 
                                      inter_fixed = FALSE, 
                                      generate_data = generate_norm,
                                      brep = 5,
                                      nrep = 2,
                                      seed = 123,
                                      cores = 2,
                                      interaction = 1,
                                      interaction_m = 1),
                      SIMPLIFY = FALSE)
  
  
  


save(test_result, file = "./result/simulation_fixed_random_test")


