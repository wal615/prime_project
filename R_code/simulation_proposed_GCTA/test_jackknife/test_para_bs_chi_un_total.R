library(sas7bdat)
library(R.utils)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
options(warn = 1, error = bettertrace::stacktrace)
# options(warn = 1, error = recover)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
# source("./reports/proposed_GCTA_paper/est_var_analysis/est_combined_data/covaraites_summary_2005_2014.R")
source("./reports/proposed_GCTA_paper/est_var_analysis/est_combined_data/covaraites_summary_1999_2004.R")
c_betam <- 8
c_betai <- 2
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/"

cores <- 1
n_iter <- 1
delete_d <- FALSE

seed_loop <- 1234
seed_coef <- 1014
# steup parameters

# data generation
emp_n <- 10^5
# n_total <- c(50, 75, 100, 150, 200, 500, 750, 1000, 1500)
n_total <- c(250, 500)
dist <- "normal"
generate_data <- generate_chi
structure <- "un"

# sub_sampling
bs <- "para-bs"
n_sub <- 1000
d_fn <- function(n) {0}

# d <- 1012
# bs <- "leave-1-2"
# d <- 0
# bs <- "full"

# bs <- "leave-1"
# d_fn <- function(n) {1}

# bs <- "leave-d"
# # d_fn <- function(n) {round(0.75*n,0)}
# d_fn <- function(n) {round(25,0)}



set.seed(123)
index <- sample(1:nrow(PCB_1999_2004_common), 100, replace = F)
# # pre_cor <- cor(data.matrix(PCB_1999_2004_common[index, ..PCB_common]) %*% invsqrt(cov_1999_2004))
pre_cor <- cor(data.matrix(PCB_1999_2004_common[index, ..PCB_common]))
Var <- "PCB_1999_2004"
p <- length(PCB_common)
# p <- 1000

# combine <- TRUE
# est <- "total"

combine <- TRUE
est <- "total"

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
# sparse_decor_method <- "Glasso"
# sparse_uncorr_method <- dgpGLASSO_method
# sparse_uncorr_args <- NULL
sparse_decor_method <- "None"
sparse_uncorr_method <- NULL
sparse_uncorr_args <- NULL


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



# kernel <- Dicker_2013_kernel
# kernel_args <- list(decor = decor)
# kernel_name <- "Dicker_2013_kernel"
# kernel_result_col_names <- col_names_Dicker

# kernel <- EigenPrism_kernel
# kernel_args <- list(decor = decor)
# kernel_name <- "EigenPrism_kernel"
# kernel_result_col_names <- col_names_Eigen


kernel_args <- list(interact = 0,decor = decor)
kernel <- h_GCTA_kernel
kernel_name <- "h_GCTA_kernel"
kernel_result_col_names <- col_names_h_GCTA


# kernel <- least_square_kernel
# kernel_args <- list(decor = decor)
# kernel_name <- "least_square_kernel"
# kernel_result_col_names <- col_names_least_square


# est2
# kernel_args_2 <- list(decor = decor)
# kernel_2 <- GCTA_rr_kernel
# kernel_name <- append(kernel_name,"GCTA_rr_kernel") %>% paste(.,collapse = "_")
# kernel_result_col_names_2 <- col_names_GCTA_rr
# kernel_args_2 <- list(interact = 0,decor = decor)
# kernel_2 <- GCTA_kernel
# kernel_name <- append(kernel_name,"GCTA_kernel") %>% paste(.,collapse = "_")
# kernel_result_col_names_2 <- col_names_GCTA

kernel_args_2 <- NULL
kernel_2 <- NULL
kernel_result_col_names_2 <- NULL



# coef
main_fixed_var <- 0.5
main_random_var <- 0
inter_fixed_var <- 0.1
inter_random_var <- 0
# rho_e <- c(0.2, 0.5, 0.7)
rho_e <- 0.5
gene_coeff_args <- list(main_fixed_var = main_fixed_var,
                        main_random_var = main_random_var,
                        inter_fixed_var = inter_fixed_var,
                        inter_random_var = inter_random_var)

# generate args list
args_all <- expand.grid(structure = structure, p = p, n = n_total,pre_cor = list(pre_cor), rho_e = rho_e)
args_all$d <- d_fn(args_all$n)
gene_data_args_list <- args_all[,1:4] %>% split(x = ., f = seq(nrow(.))) # generate a list from each row of a dataframe
rho_e_list <- args_all[,"rho_e", drop = FALSE] %>% split(x = ., f = seq(nrow(.)))
d_list <- args_all[,"d", drop = FALSE] %>% data.matrix(.) %>% split(x = ., f = seq(nrow(.)))
if(delete_d == TRUE){
  n_sub_list <- (args_all$n)^1.5 %>% round(.)
} else if( bs != "para-bs") {
  n_sub_list <- rep(0, length(args_all$n))
} else {
  n_sub_list <- n_sub
}

# setup folders for results
result_name <- paste("decor",decor_method, "sparse", sparse_decor_method,
                     dist, "structure", structure, "main", main_fixed_var, "inter",
                     inter_fixed_var, "n", paste(n_total, collapse = "_"), "p", p, "rho_e", paste(rho_e,collapse = "_"),
                     "decor",decor,"subd",paste(unique(unlist(d_list)), collapse = "_"), "iter", n_iter, "nsub", max(unlist(n_sub_list)), "bs", bs,
                     kernel_name, "est", est, "c_betam", c_betam, "c_betai", c_betai, sep = "_")

# result_name <- paste("decor",decor_method, "sparse", sparse_decor_method, 
#                      dist, "structure", structure, "main", main_fixed_var, "inter",
#                      inter_fixed_var, "n", paste(n_total, collapse = "_"), "p", p, "rho_e", paste(rho_e,collapse = "_"), 
#                      "decor",decor, "iter", n_iter,"bs",bs, "nsub", max(unlist(n_sub_list)),
#                      kernel_name, "est", est, "c_betam", c_betam, "c_betai", c_betai, "Var", Var, sep = "_")

result_folder_path <- paste0(save_path, result_name, "/")
dir.create(result_folder_path)

# run simulation
result_list <- mapply(FUN = simulation_var_est_bootstrap_fn,
                      gene_data_args = gene_data_args_list,
                      rho_e = rho_e_list,
                      d = d_list,
                      n_sub = n_sub_list,
                      MoreArgs = list(p = p,
                                      kernel = kernel,
                                      kernel_args = kernel_args,
                                      kernel_result_col_names = kernel_result_col_names,
                                      kernel_2 = kernel_2,
                                      kernel_args_2 = kernel_args_2,
                                      kernel_result_col_names_2 = kernel_result_col_names_2,
                                      bs = bs,
                                      bs_summary = TRUE,
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
                                      seed_loop = seed_loop,
                                      seed_coef = seed_coef,
                                      cores = cores,
                                      inter_result_path = result_folder_path),
                      SIMPLIFY = FALSE)
