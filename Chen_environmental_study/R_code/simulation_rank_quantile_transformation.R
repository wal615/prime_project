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
b_rank <- std_fn(b, ncol(b), tran_FUN = rank_tran) 
b_quantile <- std_fn(b, ncol(b), tran_FUN = norm_quantile_tran) 

data_list <- list(PCB_rank = b_rank,
                  PCB_quantile = b_quantile)

interaction_list <- list(1,1)
interaction_m_list <- list(1,1)

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
                      MoreArgs = list(brep = 80, nrep = 20, seed = 123, cores = 10),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_rank_quantile_tranformation")
