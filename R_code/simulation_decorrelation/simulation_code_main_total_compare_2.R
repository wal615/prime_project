options(error = recover)
setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_decorrelation/decorrelation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
cores <- 20

###############################################################################################################################
## total
###############################################################################################################################

combine <- FALSE
n_total <- c(800,900,1000)
# rho <- seq(0.1,0.9,0.1)
p <- 34
pre_cor <- unstr_corr.mat(p)
# p <- 33
# pro <- seq(0.1,0.9,0.1)

all_args <- expand.grid(structure = "un", p = p, n = n_total, pre_cor = list(pre_cor), main_pro = 1:10)
gene_args <- all_args[,c("structure", "p", "n", "pre_cor")]
# gene_args <- expand.grid(rho = rho, p = p, structure = "cs", n = n_total)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe
uncorr_args <- all_args[,c("main_pro","p")]
uncorr_args <- uncorr_args %>% split(x = ., f = seq(nrow(uncorr_args)))
# dim_red_args <- list(list(reduce_coef = 0.8))

result_list_fixed_fixed_main <- mapply(FUN = simulation_fn,
                                       gene_args = gene_args,
                                       combine = combine,
                                       uncorr_args = uncorr_args,
                                       MoreArgs = list(p = p,
                                                       tran_fun = null_tran,
                                                       main_fixed = TRUE,
                                                       inter_fixed = TRUE,
                                                       uncorr_method = SVD_method,
                                                       dim_red_method = NULL,
                                                       generate_data = generate_chi,
                                                       brep = 200,
                                                       nrep = 20,
                                                       seed = 1234,
                                                       cores = cores,
                                                       interaction = 1,
                                                       interaction_m = 0,
                                                       corrected_main = TRUE),
                                       SIMPLIFY = FALSE)

save(result_list_fixed_fixed_main, file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_0_inter_1_0_interaction_main_1_10")

###############################################################################################################################
## total
###############################################################################################################################

combine <- TRUE
# rho <- seq(0.1,0.9,0.1)
p <- 34
# pre_cor <- unstr_corr.mat(p)
# p <- 33
# pro <- seq(0.1,0.9,0.1)

gene_args <- expand.grid(structure = "un", p = p, n = n_total, pre_cor = list(pre_cor))
# gene_args <- expand.grid(rho = rho, p = p, structure = "cs", n = n_total)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe
uncorr_args <- list(p = p)
dim_red_args <- list(reduce_coef = 0.5)

result_list_fixed_fixed_total <- mapply(FUN = simulation_fn,
                                        gene_args = gene_args,
                                        combine = combine,
                                        MoreArgs = list(p = p,
                                                        tran_fun = null_tran,
                                                        main_fixed = TRUE,
                                                        inter_fixed = TRUE,
                                                        uncorr_method = SVD_method,
                                                        uncorr_args = uncorr_args,
                                                        dim_red_method = SVD_dim_reduction,
                                                        dim_red_args = dim_red_args,
                                                        generate_data = generate_chi,
                                                        brep = 200,
                                                        nrep = 20,
                                                        seed = 1234,
                                                        cores = cores,
                                                        interaction = 1,
                                                        interaction_m = 0),
                                        SIMPLIFY = FALSE)
save(result_list_fixed_fixed_total, file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_1_svd_0.5_0_interaction")