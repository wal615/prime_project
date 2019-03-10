options(error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn", modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/pcb_99_13_no_missing.csv"
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

cores <- 15
n_iter <- 200
n_iter_2 <- 20

###############################################################################################################################
## Chi-squre fixed case
###############################################################################################################################


###############################################################################################################################
## inter_1 inter_m = 0
###############################################################################################################################

combine <- FALSE
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0.1,
                        inter_random_var = 0)
p <- 33
pro <- c(0.2,0.6,0.8)
gene_data_args_PCB <- expand.grid(data_path = data_path, pro = pro, stringsAsFactors = FALSE)
gene_data_args <- gene_data_args_PCB

gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)
result_list_fixed_PCB_main_0.5_inter_0.1_main_inter <- mapply(FUN = simulation_fn,
                                                        gene_data_args = gene_data_args,
                                                        MoreArgs = list(p = p,
                                                                        tran_fun = null_tran,
                                                                        combine = combine,
                                                                        gene_coeff_args = gene_coeff_args,
                                                                        uncorr_method = SVD_method,
                                                                        uncorr_args = uncorr_args,
                                                                        dim_red_method = NULL,
                                                                        generate_data = generate_PCB,
                                                                        brep = n_iter,
                                                                        nrep = n_iter_2,
                                                                        seed = 1234,
                                                                        cores = cores,
                                                                        interaction_m = 1),
                                                        SIMPLIFY = FALSE)

saveRDS(result_list_fixed_PCB_main_0.5_inter_0.1_main_inter, file = "./result/simulation_proposed_GCTA_paper/result_list_fixed_PCB_main_0.5_inter_0.1_main_inter")



###############################################################################################################################
## inter_0 inter_m = 0
###############################################################################################################################

combine <- FALSE
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0,
                        inter_random_var = 0)
p <- 33
pro <- c(0.2,0.6,0.8)
gene_data_args_PCB <- expand.grid(data_path = data_path, pro = pro, stringsAsFactors = FALSE)
gene_data_args <- gene_data_args_PCB

gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)
result_list_fixed_PCB_main_0.5_inter_0_main_inter <- mapply(FUN = simulation_fn,
                                                      gene_data_args = gene_data_args,
                                                      MoreArgs = list(p = p,
                                                                      tran_fun = null_tran,
                                                                      combine = combine,
                                                                      gene_coeff_args = gene_coeff_args,
                                                                      uncorr_method = SVD_method,
                                                                      uncorr_args = uncorr_args,
                                                                      dim_red_method = NULL,
                                                                      generate_data = generate_PCB,
                                                                      brep = n_iter,
                                                                      nrep = n_iter_2,
                                                                      seed = 1234,
                                                                      cores = cores,
                                                                      interaction_m = 1),
                                                      SIMPLIFY = FALSE)

saveRDS(result_list_fixed_PCB_main_0.5_inter_0_main_inter, file = "./result/simulation_proposed_GCTA_paper/result_list_fixed_PCB_main_0.5_inter_0_main_inter")
