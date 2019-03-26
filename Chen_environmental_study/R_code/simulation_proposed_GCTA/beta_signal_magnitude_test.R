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

x_final <- generate_PCB(data_path, pro = 0.3) %>% std_fn(.) %>% add_inter(.)
gene_coeff_args <- list(main_fixed_var = 1,
                        main_random_var = 0,
                        inter_fixed_var = 1,
                        inter_random_var = 0)
p <- 33

betam <- generate_main(33, gene_coeff_args)
betai <- generate_inter(33, gene_coeff_args)
var(x_final[,1:p]%*%betam)
var(x_final[,-(1:p)]%*%betai)