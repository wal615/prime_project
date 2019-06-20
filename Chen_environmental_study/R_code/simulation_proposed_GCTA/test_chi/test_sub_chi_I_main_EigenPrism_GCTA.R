options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn",modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/pcb_99_13_no_missing.csv"
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/non_decore/"
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

cores <- 30
n_iter <- 10^3
n_sub <- 200
seed_loop <- 1234
seed_coef <- 1014
# steup parameters

# sub_sampling
pro <- 0.5
bs <- "leave-d"

# data generation
emp_n <- 10^5
n_total <- c(100, 500, 1000)
dist <- "chi"
generate_data <- generate_chi
structure <- "I"
pre_cor <- real_data_corr.mat(data_path)
# p <- dim(pre_cor)[1]
p <- 10^3

# est 
combine <- FALSE
est <- "main"
kernel <- EigenPrism_kernel
kernel_args <- list(decor = FALSE)
kernel_name <- "EigenPrism_kernel"
kernel_result_col_names <- col_names_Eigen

# est2
kernel_args_2 <- list(interact = 0,decor = FALSE)
kernel_2 <- GCTA_kernel
kernel_name <- append(kernel_name,"GCTA_kernel") %>% paste(.,collapse = "_")
kernel_result_col_names_2 <- col_names_GCTA

# dim_reduction
dim_red_method <- NULL
dim_red_args <- NULL

# coef
main_fixed_var <- 0.5
main_random_var <- 0
inter_fixed_var <- 0
inter_random_var <- 0
rho_e <- c(0.2, 0.5, 0.7)
gene_coeff_args <- list(main_fixed_var = main_fixed_var,
                        main_random_var = main_random_var,
                        inter_fixed_var = inter_fixed_var,
                        inter_random_var = inter_random_var)

# generate args list
args_all <- expand.grid(structure = structure, p = p, n = n_total, rho_e = rho_e)
gene_data_args_list <- args_all[,1:3] %>% split(x = ., f = seq(nrow(.))) # generate a list from each row of a dataframe
rho_e_list <- args_all[,4, drop = FALSE] %>% split(x = ., f = seq(nrow(.)))
uncorr_args <- list(p = p)

# setup folders for results
result_name <- paste("result_list_fixed_sub", dist, "structure", structure, "main", main_fixed_var, "inter",
                     inter_fixed_var, "n", paste(n_total, collapse = "_"), "p", p, "rho_e", paste(rho_e,collapse = "_"), 
                     "dim_red_coeff", dim_red_args, "subpro", pro, "iter", n_iter, "nsub", n_sub,
                     kernel_name, "est", est, sep = "_")
result_folder_path <- paste0(save_path, result_name, "/")
dir.create(result_folder_path)

# run simulation
result_list <- mapply(FUN = simulation_var_est_fn,
                      gene_data_args = gene_data_args_list,
                      rho_e = rho_e_list,
                      MoreArgs = list(p = p,
                                      kernel = kernel,
                                      kernel_args = kernel_args,
                                      kernel_result_col_names = kernel_result_col_names,
                                      kernel_2 = kernel_2,
                                      kernel_args_2 = kernel_args_2,
                                      kernel_result_col_names_2 = kernel_result_col_names_2,
                                      pro = pro,
                                      bs = bs,
                                      emp_n,
                                      combine = combine,
                                      gene_coeff_args = gene_coeff_args,
                                      uncorr_method = SVD_method,
                                      uncorr_args = uncorr_args,
                                      dim_red_method = dim_red_method,
                                      dim_red_args = dim_red_args,
                                      generate_data = generate_data,
                                      brep = n_iter,
                                      n_sub = n_sub,
                                      seed_loop = seed_loop,
                                      seed_coef = seed_coef,
                                      cores = cores,
                                      inter_std = TRUE,
                                      inter_result_path = result_folder_path),
                      SIMPLIFY = FALSE)
