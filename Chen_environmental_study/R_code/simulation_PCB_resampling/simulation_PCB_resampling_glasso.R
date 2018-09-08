setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_PCB_resampling/PCB_resampling_simulation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)


cores <- 25
p <- 34
combine <- TRUE

pro <- seq(0.1,0.9,0.1)
n <- round(1000*pro, 0)
rho <- c(0.6, 0.4, 0.1, 0.05, 0.01)
args_table <- reshape::expand.grid.df(data.frame(n=n,pro=pro),data.frame(rho = rho))

gene_args <- args_table[,2,drop = FALSE] %>% 
             split(x = ., f = seq(nrow(.))) # generate a list from each row of a dataframe

uncorr_args <- args_table[,3,drop = FALSE] %>% 
               split(x = ., f = seq(nrow(.)))


# glasso
# result_list_fixed_fixed_total_glasso <- mapply(FUN = simulation_fn,
#                                                n = args_table$n,
#                                                gene_args = gene_args,
#                                                uncorr_args = uncorr_args,
#                                                MoreArgs = list(p = p,
#                                                                tran_fun = null_tran,
#                                                                uncorr_method = GLASSO_method,
#                                                                combine = combine,
#                                                                main_fixed = TRUE,
#                                                                inter_fixed = TRUE,
#                                                                generate_data = generate_PCB,
#                                                                brep = 200,
#                                                                nrep = 20,
#                                                                seed = 123,
#                                                                cores = cores,
#                                                                interaction = 1,
#                                                                interaction_m = 0),
#                                                SIMPLIFY = FALSE)
# save(result_list_fixed_fixed_total_glasso, 
#      file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_total_glasso")



result_list_fixed_random_total_glasso <- mapply(FUN = simulation_fn,
                                               n = args_table$n,
                                               gene_args = gene_args,
                                               uncorr_args = uncorr_args,
                                               MoreArgs = list(p = p,
                                                               tran_fun = null_tran,
                                                               uncorr_method = GLASSO_method,
                                                               combine = combine,
                                                               main_fixed = TRUE,
                                                               inter_fixed = FALSE,
                                                               generate_data = generate_PCB,
                                                               brep = 200,
                                                               nrep = 20,
                                                               seed = 123,
                                                               cores = cores,
                                                               interaction = 1,
                                                               interaction_m = 0),
                                               SIMPLIFY = FALSE)
save(result_list_fixed_random_total_glasso, 
     file = "./result/PCB_resampling/simulation_result_list_fixed_random_total_glasso")

result_list_random_random_total_glasso <- mapply(FUN = simulation_fn,
                                               n = args_table$n,
                                               gene_args = gene_args,
                                               uncorr_args = uncorr_args,
                                               MoreArgs = list(p = p,
                                                               tran_fun = null_tran,
                                                               uncorr_method = GLASSO_method,
                                                               combine = combine,
                                                               main_fixed = FALSE,
                                                               inter_fixed = FALSE,
                                                               generate_data = generate_PCB,
                                                               brep = 200,
                                                               nrep = 20,
                                                               seed = 123,
                                                               cores = cores,
                                                               interaction = 1,
                                                               interaction_m = 0),
                                               SIMPLIFY = FALSE)
save(result_list_random_random_total_glasso, 
     file = "./result/PCB_resampling/simulation_result_list_random_random_total_glasso")


# svd method

# result_list_fixed_fixed_total_svd <- mapply(FUN = simulation_fn,
#                                             n = n,
#                                             gene_args = gene_args[1:9],
#                                             MoreArgs = list(p = p,
#                                                             tran_fun = null_tran,
#                                                             combine = combine,
#                                                             main_fixed = TRUE,
#                                                             inter_fixed = TRUE,
#                                                             generate_data = generate_PCB,
#                                                             brep = 200,
#                                                             nrep = 20,
#                                                             seed = 123,
#                                                             cores = cores,
#                                                             interaction = 1,
#                                                             interaction_m = 0),
#                                             SIMPLIFY = FALSE)
# 
# save(result_list_fixed_fixed_total_svd, 
#      file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_total_svd")

result_list_fixed_random_total_svd <- mapply(FUN = simulation_fn,
                                            n = n,
                                            gene_args = gene_args[1:9],
                                            MoreArgs = list(p = p,
                                                            tran_fun = null_tran,
                                                            combine = combine,
                                                            main_fixed = TRUE,
                                                            inter_fixed = FALSE,
                                                            generate_data = generate_PCB,
                                                            brep = 200,
                                                            nrep = 20,
                                                            seed = 123,
                                                            cores = cores,
                                                            interaction = 1,
                                                            interaction_m = 0),
                                            SIMPLIFY = FALSE)

save(result_list_fixed_random_total_svd, 
     file = "./result/PCB_resampling/simulation_result_list_fixed_random_total_svd")

result_list_random_random_total_svd <- mapply(FUN = simulation_fn,
                                            n = n,
                                            gene_args = gene_args[1:9],
                                            MoreArgs = list(p = p,
                                                            tran_fun = null_tran,
                                                            combine = combine,
                                                            main_fixed = FALSE,
                                                            inter_fixed = FALSE,
                                                            generate_data = generate_PCB,
                                                            brep = 200,
                                                            nrep = 20,
                                                            seed = 123,
                                                            cores = cores,
                                                            interaction = 1,
                                                            interaction_m = 0),
                                            SIMPLIFY = FALSE)

save(result_list_random_random_total_svd, 
     file = "./result/PCB_resampling/simulation_result_list_random_random_total_svd")