options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn",modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/pcb_99_13_no_missing.csv"
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/"
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

cores <- 1
n_iter <- 1
n_sub <- 5
seed <- 1234
###############################################################################################################################
## inter_1 inter_m = 0
###############################################################################################################################

# steup parameters
dist <- "chi"
generate_data <- generate_chi
est <- "main"
interaction_m <- 0

dim_red_method <- SVD_dim_reduction
dim_red_args <- list(reduce_coef = 0.8)

combine <- FALSE
n_total <- c(1500)
main_fixed_var <- 0.5
main_random_var <- 0
inter_fixed_var <- 0
inter_random_var <- 0
structure <- "un"
gene_coeff_args <- list(main_fixed_var = main_fixed_var,
                        main_random_var = main_random_var,
                        inter_fixed_var = inter_fixed_var,
                        inter_random_var = inter_random_var)
pre_cor <- real_data_corr.mat(data_path)
p <- dim(pre_cor)[1]
pro <- 0.5
bs <- FALSE

# generate args list
args_all <- expand.grid(structure = structure, p = p, n = n_total, pre_cor = list(pre_cor))
gene_data_args <- args_all
gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)

# setup folders for results
result_name <- paste("result_list_fixed_sub", dist, "structure", structure, "main", main_fixed_var, "inter",
                     inter_fixed_var, "n", n_total, "p", p, "dim_red_coeff", dim_red_args, est, sep = "_")
result_folder_path <- paste0(save_path, result_name, "/")
dir.create(result_folder_path)

# run simulation
result_list <- mapply(FUN = simulation_var_est_fn,
                      gene_data_args = gene_data_args,
                      MoreArgs = list(p = p,
                                      pro = pro,
                                      bs = bs,
                                      combine = combine,
                                      gene_coeff_args = gene_coeff_args,
                                      uncorr_method = SVD_method,
                                      uncorr_args = uncorr_args,
                                      dim_red_method = dim_red_method,
                                      dim_red_args = dim_red_args,
                                      generate_data = generate_data,
                                      brep = n_iter,
                                      n_sub = n_sub,
                                      seed = seed,
                                      cores = cores,
                                      interaction_m = interaction_m,
                                      inter_std = TRUE,
                                      inter_result_path = result_folder_path),
                      SIMPLIFY = FALSE)
