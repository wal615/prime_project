# the following simulation is trying to compare the differnce between the proposed method 
# and the proposed method on fixed effects. 

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


## Transromation with full data
b <- a
b_null_full <- std_fn(b, ncol(b), tran_FUN = null_tran, list(tran = "null", subset = "full")) 
b_rank_full <- std_fn(b, ncol(b), tran_FUN = rank_tran, list(tran = "rank", subset = "full")) 
b_quantile_full <- std_fn(b, ncol(b), tran_FUN = norm_quantile_tran, list(tran = "norm_quan", subset = "full")) 
b_log_full <- std_fn(b, ncol(b), tran_FUN = log_tran, list(tran = "log", subset = "full"))
b_sqrt_full <- std_fn(b, ncol(b), tran_FUN = sqrt_tran, list(tran = "sqrt", subset = "full"))
b_cat_10_full <- std_fn(b, ncol(b), tran_FUN = categorized_tran, by = 0.1, list(tran = "cate", by = "0.1", subset = "full"))
b_cat_5_full <- std_fn(b, ncol(b), tran_FUN = categorized_tran, by = 0.2, list(tran = "cate", by = "0.2", subset = "full"))
b_cat_2_full <- std_fn(b, ncol(b), tran_FUN = categorized_tran, by = 0.5, list(tran = "cate", by = "0.5", subset = "full"))

data_list_fixed_full <- list(b_null_full  = b_null_full,
                             b_rank_full = b_rank_full,
                             b_quantile_full = b_quantile_full,
                             b_log_full = b_log_full,
                             b_sqrt_full = b_sqrt_full,
                             b_cat_10_full = b_cat_10_full,
                             b_cat_5_full = b_cat_5_full,
                             b_cat_2_full = b_cat_2_full)

interaction_list <- as.list(rep(1,length(data_list_fixed_full)))
interaction_m_list <- as.list(rep(1,length(data_list_fixed_full)))

result_list <- mapply(FUN = compare_corr_GCTA,
                      b = data_list_fixed_full,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 3, nrep = 100, seed = 123, cores = 3, 
                                      interm_result = TRUE, 
                                      interm_result_path = "~/dev/projects/Chen_environmental_study/result/inter_result/"),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_fixed_full_8tran")

