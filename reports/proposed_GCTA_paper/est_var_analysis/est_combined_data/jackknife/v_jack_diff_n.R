## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}


# n = n - 1 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_49_74_99_149_199_499_999_1499_1999_p_100_rho_e_0.5_decor_FALSE_subd_0_iter_1000_nsub_1_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# EigenPrism
summary_result_EigenPrism_n_1 <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_n_1[, match_n := n+1]
# GCTA_rr
summary_result_GCTA_rr_n_1 <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_n_1[, match_n := n+1]

# n = n
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_2000_p_100_rho_e_0.5_decor_FALSE_subd_0_iter_1000_nsub_1_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]



# EigenPrism
summary_result_EigenPrism_n <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

# GCTA_rr
summary_result_GCTA_rr_n <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

## merge n and n-1 together 
### GCTA
GCTA_p_100_n_50_2000 <- merge(x = summary_result_GCTA_rr_n_1, y = summary_result_GCTA_rr_n,
                              by.x = "match_n", by.y = "n")
GCTA_p_100_n_50_2000[,var_diff := (n/match_n)*est_var.x - est_var.y]
Eg_p_100_n_50_2000 <- merge(x = summary_result_EigenPrism_n_1, y = summary_result_EigenPrism_n,
                              by.x = "match_n", by.y = "n")
Eg_p_100_n_50_2000[,var_diff := (n/match_n)*est_var.x - est_var.y]

#
