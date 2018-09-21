# Following simulation is to test if under normal distribution, we can estimate the interaction effect unbaisly by proposed method

setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_fixed_random/chi_square_fixed_random_simulation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

cores <- 20
n <- 1000
p <- 34
gene_args <- data.frame(n =n, p = p, rho = 0.6, pro = seq(0.1,0.9,0.1), chi_coef = 1)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe
uncorr_args <- list(main = TRUE, inter = FALSE)

result_list_fixed_fixed <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(n = n,
                                                  p = p,
                                                  main_fixed = TRUE,
                                                  inter_fixed = TRUE,
                                                  uncorr_method = SVD_method,
                                                  uncorr_args = uncorr_args,
                                                  generate_data = generate_chi_resample,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_fixed, file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_fixed_combine_df_1_resample_decorr_main")

result_list_fixed_random <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(n = n,
                                                  p = p,
                                                  main_fixed = TRUE,
                                                  inter_fixed = FALSE,
                                                  uncorr_method = SVD_method,
                                                  uncorr_args = uncorr_args,
                                                  generate_data = generate_chi_resample,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_random, file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_random_combine_df_1_resample_decorr_main")


result_list_random_random <- mapply(FUN = simulation_fn,
                                   gene_args = gene_args,
                                   combine = TRUE,
                                   MoreArgs = list(n = n,
                                                   p = p,
                                                   main_fixed = FALSE,
                                                   inter_fixed = FALSE,
                                                   uncorr_method = SVD_method,
                                                   uncorr_args = uncorr_args,
                                                   generate_data = generate_chi_resample,
                                                   brep = 200,
                                                   nrep = 20,
                                                   seed = 123,
                                                   cores = cores,
                                                   interaction = 1,
                                                   interaction_m = 0),
                                   SIMPLIFY = FALSE)
save(result_list_random_random, file = "./result/simulation_fixed_random/chi_square/simulation_result_list_random_random_combine_df_1_resample_decorr_main")
