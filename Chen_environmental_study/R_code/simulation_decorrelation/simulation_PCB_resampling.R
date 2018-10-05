setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_decorrelation/decorrelation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

n_total <- c(100,200,300,400,500, 600)
cores <- 35
p <- 34
combine <- TRUE
gene_args <- data.frame(rho = 0, p = p, n = n_total, combine = combine)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe

result_list_fixed_fixed <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = TRUE,
                                                  inter_fixed = TRUE,
                                                  uncorr_method = SVD_method,
                                                  generate_data = generate_chi,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_fixed, file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_n_100_500_p_34")


result_list_fixed_random <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = TRUE,
                                                  inter_fixed = FALSE,
                                                  uncorr_method = SVD_method,
                                                  generate_data = generate_chi,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_random, file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_n_100_500_p_34")

result_list_random_random <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = FALSE,
                                                  inter_fixed = FALSE,
                                                  uncorr_method = SVD_method,
                                                  generate_data = generate_chi,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_random_random, file = "./result/simulation_decorrelation/simulation_result_list_random_random_ind_chi_n_100_500_p_34")
