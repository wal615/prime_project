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

cores <- 20
n_iter <- 10
n_iter_2 <- 10

###############################################################################################################################
## Chi-squre fixed case
###############################################################################################################################


###############################################################################################################################
## inter_1 inter_m = 0
###############################################################################################################################

combine <- TRUE
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0.1,
                        inter_random_var = 0)
p <- 33
pro <- seq(0.1, 0.3, 0.1)
reduce_coef <- c(0.8, 0.9,1)

gene_data_args_PCB <- expand.grid(data_path = data_path, pro = pro, reduce_coef = reduce_coef,stringsAsFactors = FALSE)
gene_data_args <- gene_data_args_PCB[,-3]
dim_red_args <- gene_data_args_PCB[,3,drop = FALSE]

gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
dim_red_args <- dim_red_args %>% split(x = ., f = seq(nrow(dim_red_args)))
uncorr_args <- list(p = p)

# inter_std
result_list_fixed_PCB_main_0.5_inter_0.1_total_dim_inter_std <- mapply(FUN = simulation_fn,
                                                gene_data_args = gene_data_args,
                                                dim_red_args = dim_red_args,
                                                MoreArgs = list(p = p,
                                                                tran_fun = null_tran,
                                                                combine = combine,
                                                                gene_coeff_args = gene_coeff_args,
                                                                uncorr_method = SVD_method,
                                                                uncorr_args = uncorr_args,
                                                                dim_red_method = SVD_dim_reduction,
                                                                generate_data = generate_PCB,
                                                                brep = n_iter,
                                                                nrep = n_iter_2,
                                                                seed = 1234,
                                                                cores = cores,
                                                                inter_std = TRUE,
                                                                interaction_m = 0),
                                                SIMPLIFY = FALSE)

saveRDS(result_list_fixed_PCB_main_0.5_inter_0.1_total_dim_inter_std, file = "./result/simulation_proposed_GCTA_paper/PCB_std_est/result_list_fixed_PCB_main_0.5_inter_0.1_total_dim_inter_std")

# inter_not_std
result_list_fixed_PCB_main_0.5_inter_0.1_total_dim <- mapply(FUN = simulation_fn,
                                                             gene_data_args = gene_data_args,
                                                             dim_red_args = dim_red_args,
                                                             MoreArgs = list(p = p,
                                                                             tran_fun = null_tran,
                                                                             combine = combine,
                                                                             gene_coeff_args = gene_coeff_args,
                                                                             uncorr_method = SVD_method,
                                                                             uncorr_args = uncorr_args,
                                                                             dim_red_method = SVD_dim_reduction,
                                                                             generate_data = generate_PCB,
                                                                             brep = n_iter,
                                                                             nrep = n_iter_2,
                                                                             seed = 1234,
                                                                             cores = cores,
                                                                             interaction_m = 0),
                                                             SIMPLIFY = FALSE)

saveRDS(result_list_fixed_PCB_main_0.5_inter_0.1_total_dim, file = "./result/simulation_proposed_GCTA_paper/PCB_std_est/result_list_fixed_PCB_main_0.5_inter_0.1_total_dim")


###############################################################################################################################
## inter_0 inter_m = 0
###############################################################################################################################

combine <- TRUE
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0,
                        inter_random_var = 0)

p <- 33
pro <- seq(0.1, 0.3, 0.1)
reduce_coef <- c(0.8, 0.9,1)

gene_data_args_PCB <- expand.grid(data_path = data_path, pro = pro, reduce_coef = reduce_coef,stringsAsFactors = FALSE)
gene_data_args <- gene_data_args_PCB[,-3]
dim_red_args <- gene_data_args_PCB[,3,drop = FALSE]

gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
dim_red_args <- dim_red_args %>% split(x = ., f = seq(nrow(dim_red_args)))
uncorr_args <- list(p = p)

result_list_fixed_PCB_main_0.5_inter_0_total_dim_inter_std <- mapply(FUN = simulation_fn,
                                                gene_data_args = gene_data_args,
                                                dim_red_args = dim_red_args,
                                                MoreArgs = list(p = p,
                                                                tran_fun = null_tran,
                                                                combine = combine,
                                                                gene_coeff_args = gene_coeff_args,
                                                                uncorr_method = SVD_method,
                                                                uncorr_args = uncorr_args,
                                                                dim_red_method = SVD_dim_reduction,
                                                                generate_data = generate_PCB,
                                                                brep = n_iter,
                                                                nrep = n_iter_2,
                                                                seed = 1234,
                                                                cores = cores,
                                                                inter_std = TRUE,
                                                                interaction_m = 0),
                                                SIMPLIFY = FALSE)

saveRDS(result_list_fixed_PCB_main_0.5_inter_0_total_dim_inter_std, file = "./result/simulation_proposed_GCTA_paper/PCB_std_est/result_list_fixed_PCB_main_0.5_inter_0_total_dim_inter_std")


result_list_fixed_PCB_main_0.5_inter_0_total_dim <- mapply(FUN = simulation_fn,
                                                           gene_data_args = gene_data_args,
                                                           dim_red_args = dim_red_args,
                                                           MoreArgs = list(p = p,
                                                                           tran_fun = null_tran,
                                                                           combine = combine,
                                                                           gene_coeff_args = gene_coeff_args,
                                                                           uncorr_method = SVD_method,
                                                                           uncorr_args = uncorr_args,
                                                                           dim_red_method = SVD_dim_reduction,
                                                                           generate_data = generate_PCB,
                                                                           brep = n_iter,
                                                                           nrep = n_iter_2,
                                                                           seed = 1234,
                                                                           cores = cores,
                                                                           interaction_m = 0),
                                                           SIMPLIFY = FALSE)

saveRDS(result_list_fixed_PCB_main_0.5_inter_0_total_dim, file = "./result/simulation_proposed_GCTA_paper/PCB_std_est/result_list_fixed_PCB_main_0.5_inter_0_total_dim")
