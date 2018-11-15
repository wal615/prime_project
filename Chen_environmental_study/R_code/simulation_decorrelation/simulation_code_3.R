options(error = recover)
setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_decorrelation/decorrelation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
cores <- 25

n_total <- c(100,200,300,400,500, 600, 700, 800)
# rho <- seq(0.1,0.9,0.1)
p <- 34
combine <- TRUE
pre_cor <- unstr_corr.mat(p)

gene_args <- expand.grid(structure = "un", p = p, rho = rho, n = n_total, combine = combine, pre_cor = list(pre_cor))
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)
dim_red_args <- list(reduce_coef = 0.5)

result_list_fixed_fixed <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = TRUE,
                                                  inter_fixed = TRUE,
                                                  uncorr_method = SVD_method,
                                                  uncorr_args = uncorr_args,
                                                  dim_red_method = SVD_dim_reduction,
                                                  dim_red_args = dim_red_args,
                                                  generate_data = generate_chi,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 1234,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_fixed, file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_rho_0.1_0.9_n_100_800_p_34_svd_0.5_un")


# result_list_fixed_random <- mapply(FUN = simulation_fn,
#                                   gene_args = gene_args,
#                                   combine = TRUE,
#                                   MoreArgs = list(p = p,
#                                                   tran_fun = null_tran,
#                                                   main_fixed = TRUE,
#                                                   inter_fixed = FALSE,
#                                                   uncorr_method = SVD_method,
#                                                   uncorr_args = uncorr_args,
#                                                   generate_data = generate_chi,
#                                                   brep = 200,
#                                                   nrep = 20,
#                                                   seed = 123,
#                                                   cores = cores,
#                                                   interaction = 1,
#                                                   interaction_m = 0),
#                                   SIMPLIFY = FALSE)
# save(result_list_fixed_random, file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_rho_0.1_0.9_n_100_800_p_34_svd_dim_100")

# result_list_random_random <- mapply(FUN = simulation_fn,
#                                   gene_args = gene_args,
#                                   combine = TRUE,
#                                   MoreArgs = list(p = p,
#                                                   tran_fun = null_tran,
#                                                   main_fixed = FALSE,
#                                                   inter_fixed = FALSE,
#                                                   uncorr_method = SVD_method,
#                                                   generate_data = generate_chi,
#                                                   brep = 200,
#                                                   nrep = 20,
#                                                   seed = 123,
#                                                   cores = cores,
#                                                   interaction = 1,
#                                                   interaction_m = 0),
#                                   SIMPLIFY = FALSE)
# save(result_list_random_random, file = "./result/simulation_decorrelation/simulation_result_list_random_random_ind_chi_cho_0.5_n_100_500_p_34_svd_reduction")

# pro <- seq(0.1,0.9,0.1)
# p <- 34
# combine <- TRUE
# 
# gene_args <- expand.grid(pro = pro, p = p)
# gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe
# uncorr_args <- list(p = p)
# 
# result_list_fixed_fixed <- mapply(FUN = simulation_fn,
#                                   gene_args = gene_args,
#                                   combine = TRUE,
#                                   MoreArgs = list(p = p,
#                                                   tran_fun = null_tran,
#                                                   main_fixed = TRUE,
#                                                   inter_fixed = TRUE,
#                                                   uncorr_method = SVD_method,
#                                                   uncorr_args = uncorr_args,
#                                                   generate_data = generate_PCB,
#                                                   brep = 200,
#                                                   nrep = 20,
#                                                   seed = 123,
#                                                   cores = cores,
#                                                   interaction = 1,
#                                                   interaction_m = 0),
#                                   SIMPLIFY = FALSE)
# save(result_list_fixed_fixed, file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_PCB_pro_0.1_0.9_red_0.5")

