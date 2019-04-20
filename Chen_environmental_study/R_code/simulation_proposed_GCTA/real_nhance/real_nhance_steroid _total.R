options(error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn", modifiedOnly = FALSE)
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/steroid/"
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
source("./R_code/data/real_data/NHANES/exposure_name_loading.R")
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
library(mice)

# data pre-process and generating 
# steroid 
PCB_and_LC <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/steroid/nhance_steroid_PCB_LC.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  data.table(.)
# remove all the missing value and only keep PCBs
PCB <- PCB_and_LC[, (PCB_LC):=NULL]

# check for all empty columns and removed
all_empty_col <- sapply(PCB, FUN = function(x) sum(is.na(x)) == length(x), simplify = TRUE)
PCB <- PCB[,!all_empty_col, with = FALSE]

# normalized response 
method = "pmm"
PCB_missing_imputed <- mice(PCB,m=3,maxit=3,method=method,seed=500) %>% complete(.) %>% data.frame(.)

PCB_missing_imputed$LBXGH <- norm_quantile_tran(PCB_missing_imputed$LBXGH)
data_path <- paste0(data_path, "steroid_", method ,".csv")
write.csv(x = PCB_missing_imputed, file = data_path, row.names = FALSE)


cores <- 30
n_iter <- 100
combine <- TRUE
pro <- seq(0.3, 0.6, 0.1)

# generate the parameters
args <- expand.grid(data_path = data_path, data_name = "emolobin_missing_imputed", pro = pro, resp_name = "LBXGH", stringsAsFactors = FALSE)
gene_data_args <- args
gene_data_args <- gene_data_args %>% split(x = ., f = seq(nrow(gene_data_args))) # generate a list from each row of a dataframe

uncorr_args <- list(p = 38)

result_list_real_hemolobin_total <- mapply(FUN = fit_real_data_fn,
                                           gene_data_args = gene_data_args,
                                           MoreArgs = list(tran_fun = null_tran,
                                                           combine = combine,
                                                           uncorr_method = SVD_method,
                                                           uncorr_args = uncorr_args,
                                                           generate_data = generate_real_test,
                                                           brep = n_iter,
                                                           seed = 1234,
                                                           cores = cores,
                                                           inter_std = TRUE,
                                                           interaction_m = 0),
                                           SIMPLIFY = FALSE)

saveRDS(result_list_real_hemolobin_total, file = "./result/simulation_proposed_GCTA_paper/result_list_real_hemolobin_missing_imputed_total")

combine <- FALSE
result_list_real_hemolobin_main <- mapply(FUN = fit_real_data_fn,
                                          gene_data_args = gene_data_args,
                                          MoreArgs = list(tran_fun = null_tran,
                                                          combine = combine,
                                                          uncorr_method = SVD_method,
                                                          uncorr_args = uncorr_args,
                                                          generate_data = generate_real_test,
                                                          brep = n_iter,
                                                          seed = 1234,
                                                          cores = cores,
                                                          inter_std = TRUE,
                                                          interaction_m = 0),
                                          SIMPLIFY = FALSE)

saveRDS(result_list_real_hemolobin_main, file = "./result/simulation_proposed_GCTA_paper/result_list_real_hemolobin_missing_imputed_main")