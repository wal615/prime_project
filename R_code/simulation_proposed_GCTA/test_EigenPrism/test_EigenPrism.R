options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn",modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/test_EigenPrism/test_EigenPrism_helper.R")
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/test_EigenPrism/"
library(data.table)
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

cores <- 30
n_iter <- 10^3
seed_loop <- 1234
seed_coef <- 1014
# steup parameters

rho_e <- c(0.2,0.5,0.7)
p <- c(500, 1000, 2000)
# data generation
n_total <- c(100, 500, 1000, 2000)
dist <- "chi"
generate_data <- generate_chi
structure <- "I"

# est 
combine <- FALSE
est <- "main"
kernel <- EigenPrism_kernel
kernel_args <- list(decor = FALSE)
kernel_name <- "EigenPrism_kernel"
kernel_result_col_names <- col_names_Eigen

# dim_reduction
dim_red_method <- NULL
dim_red_args <- NULL

# coef
main_fixed_var <- 0.5
main_random_var <- 0
inter_fixed_var <- 0
inter_random_var <- 0
gene_coeff_args <- list(main_fixed_var = main_fixed_var,
                        main_random_var = main_random_var,
                        inter_fixed_var = inter_fixed_var,
                        inter_random_var = inter_random_var)

# generate args list
args_all <- expand.grid(structure = structure, p = p, n = n_total, rho_e = rho_e) %>% data.table(.)
args_all <- args_all[n <= p,] 
gene_data_args_list <- args_all[,1:3]
gene_data_args_list <- gene_data_args_list %>% split(x = ., f = seq(nrow(gene_data_args_list))) # generate a list from each row of a dataframe
rho_e_list <- args_all[,4,drop = FALSE] %>% data.matrix(.)
rho_e_list <- rho_e_list %>% split(x = ., f = seq(nrow(rho_e_list)))



# setup folders for results
result_name <- paste("result_list_fixed_sub", dist, "structure", structure, "main", main_fixed_var, "inter",
                     inter_fixed_var, "n", paste(n_total,collapse = "_"), "p", paste(p,collapse = "_"), "rho_e",paste(rho_e,collapse = "_"), "dim_red_coeff", dim_red_args, "iter", n_iter,
                     kernel_name, "est", est, sep = "_")
result_folder_path <- paste0(save_path, result_name, "/")
dir.create(result_folder_path)

# run simulation
result_list <- mapply(FUN = simulation_var_est_fn,
                      gene_data_args = gene_data_args_list,
                      rho_e = rho_e_list,
                      MoreArgs = list(kernel = kernel,
                                      kernel_args = kernel_args,
                                      kernel_result_col_names = kernel_result_col_names,
                                      combine = combine,
                                      gene_coeff_args = gene_coeff_args,
                                      uncorr_method = SVD_method,
                                      uncorr_args = uncorr_args,
                                      dim_red_method = dim_red_method,
                                      dim_red_args = dim_red_args,
                                      generate_data = generate_data,
                                      brep = n_iter,
                                      seed_loop = seed_loop,
                                      seed_coef = seed_coef,
                                      cores = cores,
                                      inter_std = TRUE,
                                      inter_result_path = result_folder_path),
                      SIMPLIFY = FALSE)
result <- rbindlist(result_list)
write.csv(result, file = paste0(result_folder_path,"result.csv"))
