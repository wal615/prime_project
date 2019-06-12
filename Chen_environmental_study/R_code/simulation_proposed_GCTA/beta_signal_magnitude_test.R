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
pre_cor <- real_data_corr.mat(data_path)
p <- 33
x_final <- generate_chi(n = 10^7,p = 33,structure = "I")
gene_coeff_args <- list(main_fixed_var = 0.5,
                        main_random_var = 0,
                        inter_fixed_var = 0,
                        inter_random_var = 0)

betam <- generate_main(p, gene_coeff_args)
betai <- generate_inter(p, gene_coeff_args)
emp_theata <- t(betam)%*%var(x_final)%*%betam
true_theata <- t(betam)%*%diag(p)%*%betam

var(x_final[,1:p]%*%betam)
var(x_final[,-(1:p)]%*%betai)