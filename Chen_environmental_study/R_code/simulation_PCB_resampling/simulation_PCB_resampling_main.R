# Following simulation is to test if under normal distribution, we can estimate the interaction effect unbaisly by proposed method

setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_PCB_resampling/PCB_resampling_simulation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)



p <- 34
pro <- seq(0.6,0.9,0.1)
n <- round(1000*pro, 0)
combine = TRUE
gene_args <- data.frame(pro = pro, combine = combine)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe

result_list_fixed_fixed <- mapply(FUN = simulation_fn,
                                  n = n,
                                  gene_args = gene_args,
                                  MoreArgs = list(p = p,
                                                  tran_fun = null_tran,
                                                  combine = combine,
                                                  main_effect_only = TRUE,
                                                  main_fixed = TRUE,
                                                  inter_fixed = TRUE,
                                                  generate_data = generate_PCB,
                                                  brep = 2,
                                                  nrep = 2,
                                                  seed = 123,
                                                  cores = 1,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
save(result_list_fixed_fixed, file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_main")


result_list_fixed_random <- mapply(FUN = simulation_fn,
                                   n = n,
                                   gene_args = gene_args,
                                   MoreArgs = list(p = p,
                                                   tran_fun = null_tran,
                                                   combine = combine,
                                                   main_fixed = TRUE,
                                                   inter_fixed = FALSE,
                                                   generate_data = generate_PCB,
                                                   brep = 200,
                                                   nrep = 20,
                                                   seed = 123,
                                                   cores = 10,
                                                   interaction = 1,
                                                   interaction_m = 0),
                                   SIMPLIFY = FALSE)
save(result_list_fixed_random, file = "./result/PCB_resampling/simulation_result_list_fixed_random")

result_list_random_random <- mapply(FUN = simulation_fn,
                                    n = n,
                                    gene_args = gene_args,
                                    MoreArgs = list(p = p,
                                                    tran_fun = null_tran,
                                                    combine = combine,
                                                    main_fixed = FALSE,
                                                    inter_fixed = FALSE,
                                                    generate_data = generate_PCB,
                                                    brep = 200,
                                                    nrep = 20,
                                                    seed = 123,
                                                    cores = 10,
                                                    interaction = 1,
                                                    interaction_m = 0),
                                    SIMPLIFY = FALSE)
save(result_list_random_random, file = "./result/PCB_resampling/simulation_result_list_random_random")
