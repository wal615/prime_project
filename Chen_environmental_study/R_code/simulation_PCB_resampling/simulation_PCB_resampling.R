setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_PCB_resampling/PCB_resampling_simulation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

n_total <- 2176
cores <- 15
p <- 33
pro <- seq(0.1,0.9,0.1)
n <- round(n_total*pro, 0)
combine <- TRUE
gene_args <- data.frame(pro = pro, p = p)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe

result_list_fixed_fixed <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(n = n,
                                                  p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = TRUE,
                                                  inter_fixed = TRUE,
                                                  uncorr_method = SVD_method,
                                                  generate_data = generate_PCB,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_fixed, file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_p_33_99_13")


result_list_fixed_random <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(n = n,
                                                  p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = TRUE,
                                                  inter_fixed = FALSE,
                                                  uncorr_method = SVD_method,
                                                  generate_data = generate_PCB,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_random, file = "./result/PCB_resampling/simulation_result_list_fixed_random_p_33_99_13")

result_list_random_random <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(n = n,
                                                  p = p,
                                                  tran_fun = null_tran,
                                                  main_fixed = FALSE,
                                                  inter_fixed = FALSE,
                                                  uncorr_method = SVD_method,
                                                  generate_data = generate_PCB,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = cores,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_random_random, file = "./result/PCB_resampling/simulation_result_list_random_random_p_33_99_13")
