setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

a=read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
b=data.matrix(a[,2:35], rownames.force = NA)
b_norm=matrix(rnorm(1000*34),ncol=34) # simulated covariates
b_norm_uncorr <- mvrnorm(n = 1000, mu = rep(0,34), Sigma = diag(rep(1,34)))
b_norm_corr <- mvrnorm(n = 1000, mu = rep(0,34), Sigma = diag(rep(1,34)) + matrix(rep(2,34*34), nrow = 34))
#b=log(b)

data_list <- list(PCB_no_inter_m = b, 
                  PCB_no_inter = b, 
                  b_norm_uncorr = b_norm_uncorr, 
                  b_norm_corr = b_norm_corr, 
                  PCB_with_inter = b)

interaction_list <- list(0,0,1,1,1)
interaction_m_list <- list(0,1,1,1,1)

# cat("testing...\n")
# 
# test <- compare_corr_GCTA(b = data_list[[1]],
#                           interaction = interaction_list[[1]],
#                           interaction_m = interaction_m_list[[2]],
#                           brep = 2,
#                           nrep = 10,
#                           seed = 1,
#                           cores = 1)

result_list <- mapply(FUN = compare_corr_GCTA,
                      b = data_list,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 50, nrep = 20, seed = 123, cores = 1),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_result_reproduce_with_rep_beta")
