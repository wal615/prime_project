options(error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn", modifiedOnly = FALSE)
data_folder <- "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/hemoglobin/"
result_folder <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/nhance/hemoglobin/"
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
library(mice)

data_name <- "imputed_hemoglobin_2"

cores <- 1
n_iter <- 1
combine <- TRUE
pro <- 1
tran_fn_y <- c(null_tran = null_tran,
               log_tran  = log_tran,
               norm_quantile_tran  = norm_quantile_tran,
               norm_score_tran = norm_score_tran)
# generate the parameters
data_path <- paste0(data_folder, data_name, ".csv")
args <- expand.grid(data_path = data_path, tran_fn_y = tran_fn_y, data_name = data_name, pro = pro, resp_name = "LBXGH", stringsAsFactors = FALSE)
gene_data_args <- args
gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe

uncorr_args <- list(p = 35)

result_list_real_hemolobin_total <- mapply(FUN = fit_real_data_fn,
                                           gene_data_args = gene_data_args,
                                           MoreArgs = list(combine = combine,
                                                           uncorr_method = SVD_method,
                                                           uncorr_args = uncorr_args,
                                                           generate_data = generate_real_test,
                                                           brep = n_iter,
                                                           seed = 1234,
                                                           cores = cores,
                                                           inter_std = TRUE,
                                                           interaction_m = 0),
                                           SIMPLIFY = FALSE)
save_path <- paste0(result_folder, "result_list_", data_name,"_total")
saveRDS(result_list_real_hemolobin_total, file = save_path)

# combine <- FALSE
# result_list_real_hemolobin_main <- mapply(FUN = fit_real_data_fn,
#                                            gene_data_args = gene_data_args,
#                                            MoreArgs = list(combine = combine,
#                                                            uncorr_method = SVD_method,
#                                                            uncorr_args = uncorr_args,
#                                                            generate_data = generate_real_test,
#                                                            brep = n_iter,
#                                                            seed = 1234,
#                                                            cores = cores,
#                                                            inter_std = TRUE,
#                                                            interaction_m = 0),
#                                            SIMPLIFY = FALSE)
# save_path <- paste0(result_folder, "result_list_", data_name,"_main")
# saveRDS(result_list_real_hemolobin_main, file = save_path)