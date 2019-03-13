options(error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn")
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/pcb_99_13_no_missing.csv"
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

cores <- 10
n_iter <- 200
n_iter_2 <- 20

###############################################################################################################################
## Chi-squre fixed case
###############################################################################################################################


###############################################################################################################################
## inter_1 inter_m = 0
###############################################################################################################################

combine <- TRUE
n_total <- c(1000)
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0.1,
                        inter_random_var = 0)
pre_cor <- real_data_corr.mat(data_path)
p <- dim(pre_cor)[1]
gene_data_args_un <- expand.grid(structure = "un", p = p, n = n_total, pre_cor = list(pre_cor))
gene_data_args <- gene_data_args_un

gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)
result_list_fixed_normal_un_main_0.5_inter_0.1_total <- mapply(FUN = simulation_fn,
                                                           gene_data_args = gene_data_args,
                                                           MoreArgs = list(p = p,
                                                                           tran_fun = null_tran,
                                                                           combine = combine,
                                                                           gene_coeff_args = gene_coeff_args,
                                                                           uncorr_method = SVD_method,
                                                                           uncorr_args = uncorr_args,
                                                                           dim_red_method = NULL,
                                                                           generate_data = generate_normal,
                                                                           brep = n_iter,
                                                                           nrep = n_iter_2,
                                                                           seed = 1234,
                                                                           cores = cores,
                                                                           interaction_m = 0),
                                                           SIMPLIFY = FALSE)

saveRDS(result_list_fixed_normal_un_main_0.5_inter_0.1_total, file = "./result/simulation_proposed_GCTA_paper/result_list_fixed_normal_un_main_0.5_inter_0.1_total")



###############################################################################################################################
## inter_0 inter_m = 0
###############################################################################################################################

combine <- FALSE
n_total <- c(1000)
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0,
                        inter_random_var = 0)
pre_cor <- real_data_corr.mat(data_path)
p <- dim(pre_cor)[1]
gene_data_args_un <- expand.grid(structure = "un", p = p, n = n_total, pre_cor = list(pre_cor))
gene_data_args <- gene_data_args_un

gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)
result_list_fixed_normal_un_main_0.5_inter_0_total <- mapply(FUN = simulation_fn,
                                                         gene_data_args = gene_data_args,
                                                         MoreArgs = list(p = p,
                                                                         tran_fun = null_tran,
                                                                         combine = combine,
                                                                         gene_coeff_args = gene_coeff_args,
                                                                         uncorr_method = SVD_method,
                                                                         uncorr_args = uncorr_args,
                                                                         dim_red_method = NULL,
                                                                         generate_data = generate_normal,
                                                                         brep = n_iter,
                                                                         nrep = n_iter_2,
                                                                         seed = 1234,
                                                                         cores = cores,
                                                                         interaction_m = 0),
                                                         SIMPLIFY = FALSE)

saveRDS(result_list_fixed_normal_un_main_0.5_inter_0_total, file = "./result/simulation_proposed_GCTA_paper/result_list_fixed_normal_un_main_0.5_inter_0_total")
