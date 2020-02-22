setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)


a=read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
a=data.matrix(a[,2:35], rownames.force = NA)
b <- a 
b_null <- std_fn(b, ncol(b), tran_FUN = null_tran)
b_rank <- std_fn(b, ncol(b), tran_FUN = rank_tran) 
b_quantile <- std_fn(b, ncol(b), tran_FUN = norm_quantile_tran) 

data_list <- list(PCB_rank = b_rank,
                  PCB_quantile = b_quantile,
                  PCB = b_null)

interaction_list <- list(1,1,1)
interaction_m_list <- list(1,1,1)

result_list <- mapply(FUN = compare_corr_GCTA,
                      b = data_list,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 2, nrep = 2, seed = 123, cores = 10),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_uncorrelated_method")
