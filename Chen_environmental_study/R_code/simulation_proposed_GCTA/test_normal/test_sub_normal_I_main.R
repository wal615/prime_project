library(sas7bdat)
library(R.utils)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
# source("./reports/proposed_GCTA_paper/est_var_analysis/est_combined_data/covaraites_summary_2005_2014.R")
source("./reports/proposed_GCTA_paper/est_var_analysis/est_combined_data/covaraites_summary_1999_2004.R")
c_betam <- 8
c_betai <- 0
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_jackknife_reports_09_25_2019/"

cores <- 1
n_iter <- 1
n_sub <- 0
seed_loop <- 1234
seed_coef <- 1014
# steup parameters

# sub_sampling
pro <- 1012
bs <- "leave-1-2"

# data generation
emp_n <- 10^5
n_total <- c(100,150)
dist <- "normal"
generate_data <- generate_normal
structure <- "I"


# set.seed(123)
# index <- sample(1:nrow(PCB_1999_2004_common), 100, replace = F)
# # pre_cor <- cor(data.matrix(PCB_1999_2004_common[index, ..PCB_common]) %*% invsqrt(cov_1999_2004))
# pre_cor <- cor(data.matrix(PCB_1999_2004_common[index, ..PCB_common]))
Var <- "null"
p <- length(PCB_common)

combine <- FALSE
est <- "main"

# decorr
decor_method <- "hist"
# uncorr_method <- SVD_method
# uncorr_args <- NULL
uncorr_method <- true_value_method
uncorr_args <- list(emp = TRUE, combine = combine)
# uncorr_method <- dgpGLASSO_method
# uncorr_args <- NULL
# uncorr_method <- QUIC_method
# uncorr_args <- NULL
# uncorr_method <- PCA_method
# uncorr_args <- NULL

# Sparse decor
sparse_decor_method <- "Glasso"
sparse_uncorr_method <- dgpGLASSO_method
sparse_uncorr_args <- NULL
# sparse_decor_method <- NULL
# sparse_uncorr_method <- NULL
# sparse_uncorr_args <- NULL


# est
decor <- FALSE
if(decor == FALSE) {
  decor_method <- "None"
  uncorr_method <- NULL
  uncorr_args <- NULL
  sparse_decor_method <- "None"
  sparse_uncorr_method <- NULL
  sparse_uncorr_args <- NULL
}



kernel <- EigenPrism_kernel
kernel_args <- list(decor = decor)
kernel_name <- "EigenPrism_kernel"
kernel_result_col_names <- col_names_Eigen


# kernel_args <- list(interact = 0,decor = decor)
# kernel <- GCTA_kernel
# kernel_name <- "GCTA_kernel"
# kernel_result_col_names <- col_names_GCTA


# kernel <- least_square_kernel
# kernel_args <- list(decor = decor)
# kernel_name <- "least_square_kernel"
# kernel_result_col_names <- col_names_least_square


# est2
kernel_args_2 <- list(interact = 0,decor = decor)
kernel_2 <- GCTA_kernel
kernel_name <- append(kernel_name,"GCTA_kernel") %>% paste(.,collapse = "_")
kernel_result_col_names_2 <- col_names_GCTA
# kernel_args_2 <- NULL
# kernel_2 <- NULL
# kernel_name <- NULL
# kernel_result_col_names_2 <- NULL


# coef
main_fixed_var <- 0.5
main_random_var <- 0
inter_fixed_var <- 0
inter_random_var <- 0
# rho_e <- c(0.2, 0.5, 0.7)
rho_e <- 0.5
gene_coeff_args <- list(main_fixed_var = main_fixed_var,
                        main_random_var = main_random_var,
                        inter_fixed_var = inter_fixed_var,
                        inter_random_var = inter_random_var)

# generate args list
# args_all <- expand.grid(structure = structure, p = p, n = n_total, pre_cor = list(pre_cor),rho_e = rho_e, pro = pro)
args_all <- expand.grid(structure = structure, p = p, n = n_total, rho_e = rho_e, pro = pro)
gene_data_args_list <- args_all[,1:3] %>% split(x = ., f = seq(nrow(.))) # generate a list from each row of a dataframe
rho_e_list <- args_all[,4, drop = FALSE] %>% split(x = ., f = seq(nrow(.)))
pro_list <-  args_all[,5, drop = FALSE] %>% split(x = ., f = seq(nrow(.)))


# setup folders for results
result_name <- paste("decor_method",decor_method, "sparse_method", sparse_decor_method, 
                     "result_list_fixed_sub", dist, "structure", structure, "main", main_fixed_var, "inter",
                     inter_fixed_var, "n", paste(n_total, collapse = "_"), "p", p, "rho_e", paste(rho_e,collapse = "_"), 
                     "decor",decor,"subpro",paste(pro, collapse = "_"), "iter", n_iter, "nsub", n_sub,
                     kernel_name, "est", est, "c_betam", c_betam, "c_betai", c_betai, "Var", Var, sep = "_")
result_folder_path <- paste0(save_path, result_name, "/")
dir.create(result_folder_path)

# run simulation
result_list <- mapply(FUN = simulation_var_est_fn,
                      gene_data_args = gene_data_args_list,
                      rho_e = rho_e_list,
                      pro = pro_list,
                      MoreArgs = list(p = p,
                                      kernel = kernel,
                                      kernel_args = kernel_args,
                                      kernel_result_col_names = kernel_result_col_names,
                                      kernel_2 = kernel_2,
                                      kernel_args_2 = kernel_args_2,
                                      kernel_result_col_names_2 = kernel_result_col_names_2,
                                      bs = bs,
                                      c_betam = c_betam,
                                      c_betai = c_betai,
                                      emp_n = emp_n,
                                      combine = combine,
                                      gene_coeff_args = gene_coeff_args,
                                      uncorr_method = uncorr_method,
                                      uncorr_args = uncorr_args,
                                      sparse_uncorr_method = sparse_uncorr_method,
                                      sparse_uncorr_args = sparse_uncorr_args,
                                      generate_data = generate_data,
                                      brep = n_iter,
                                      n_sub = n_sub,
                                      seed_loop = seed_loop,
                                      seed_coef = seed_coef,
                                      cores = cores,
                                      inter_std = FALSE,
                                      inter_result_path = result_folder_path),
                      SIMPLIFY = FALSE)
