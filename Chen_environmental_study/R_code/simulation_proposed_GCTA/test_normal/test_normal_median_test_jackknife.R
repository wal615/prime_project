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

cores <- 20
n_iter <- 1000
delete_d <- TRUE

seed_loop <- 1234
seed_coef <- 1014
# steup parameters

# data generation
emp_n <- 10^5
n_total <- c(50,75,100,150,200)
dist <- "normal"
generate_data <- generate_normal_median
structure <- "I"

# sub_sampling
# d <- 102
# bs <- "bs"
# d <- 1012
# bs <- "leave-1-2"
# d <- 0
# bs <- "full"

# bs <- "leave-1"
# d_fn <- function(n) {1}
# 
bs <- "leave-d"
d_fn <- function(n) {round(0.5*n,0)}



# set.seed(123)
# index <- sample(1:nrow(PCB_1999_2004_common), 100, replace = F)
# # pre_cor <- cor(data.matrix(PCB_1999_2004_common[index, ..PCB_common]) %*% invsqrt(cov_1999_2004))
# pre_cor <- cor(data.matrix(PCB_1999_2004_common[index, ..PCB_common]))
Var <- "null"
# p <- length(PCB_common)
p <- 2

# combine <- TRUE
# est <- "total"

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



kernel <- single_median_kernel
kernel_args <- list(decor = decor)
kernel_name <- "median"
kernel_result_col_names <- col_names_single_median



# est2
kernel_args_2 <- NULL
kernel_2 <- NULL
kernel_name <- append(kernel_name,"null") %>% paste(.,collapse = "_")
kernel_result_col_names_2 <- NULL


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
args_all <- expand.grid(structure = structure, p = p, n = n_total,rho_e = rho_e)
args_all$d <- d_fn(args_all$n)
gene_data_args_list <- args_all[,1:3] %>% split(x = ., f = seq(nrow(.))) # generate a list from each row of a dataframe
rho_e_list <- args_all[,4, drop = FALSE] %>% split(x = ., f = seq(nrow(.)))
d_list <-  args_all[,5, drop = FALSE] %>% data.matrix(.) %>% split(x = ., f = seq(nrow(.)))
if(delete_d == TRUE){
  n_sub_list <- (args_all$n)^1.5 %>% round(.)
} else {
  n_sub_list <- rep(0, length(args_all$n))
}

# setup folders for results
result_name <- paste("decor",decor_method, "sparse", sparse_decor_method, 
                     dist, "structure", structure, "main", main_fixed_var, "inter",
                     inter_fixed_var, "n", paste(n_total, collapse = "_"), "p", p, "rho_e", paste(rho_e,collapse = "_"), 
                     "decor",decor,"subd",paste(unique(unlist(d_list)), collapse = "_"), "iter", n_iter, "nsub", max(unlist(n_sub_list)),
                     kernel_name, "est", est, "c_betam", c_betam, "c_betai", c_betai, "Var", Var, sep = "_")
result_folder_path <- paste0(save_path, result_name, "/")
dir.create(result_folder_path)

# run simulation
result_list <- mapply(FUN = simulation_var_est_fn,
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

