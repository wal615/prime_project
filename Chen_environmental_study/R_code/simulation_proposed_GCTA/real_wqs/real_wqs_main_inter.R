options(error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn", modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

cores <- 10
n_iter <- 100

combine <- FALSE
pro <- seq(0.6, 0.9, 0.1)
wqs <- "~/dev/projects/Chen_environmental_study/R_code/data/real_data/wqs/wqs_PCBs.csv"
args <- expand.grid(data_path = wqs, data_name = "wqs_PCBs", pro = pro, stringsAsFactors = FALSE)

gene_data_args <- args
gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe

uncorr_args <- list(p = 34)
result_list_real_wqs_main_inter <- mapply(FUN = fit_real_data_fn,
                               gene_data_args = gene_data_args,
                               MoreArgs = list(tran_fun = null_tran,
                                               combine = combine,
                                               uncorr_method = SVD_method,
                                               uncorr_args = uncorr_args,
                                               generate_data = generate_real,
                                               brep = n_iter,
                                               seed = 1234,
                                               cores = cores,
                                               inter_std = FALSE,
                                               interaction_m = 1),
                               SIMPLIFY = FALSE)

saveRDS(result_list_real_wqs_main_inter, file = "./result/simulation_proposed_GCTA_paper/result_list_real_wqs_main_inter")
