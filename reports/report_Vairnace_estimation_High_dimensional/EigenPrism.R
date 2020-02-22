## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)


# reproduce the result of h_Eg 2013 paper 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/test_EigenPrism/")
result_path <- "./decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_p_100_rho_e_0.1_0.3_0.5_0.7_0.9_decor_FALSE_subd_0_iter_1000_nsub_1_bs_full_h_EigenPrism_kernel_est_main_c_betam_8_c_betai_2_Var_null" 
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


summary_result_h_Eg_I_normal_p_100_n_150_rho_e <- sub_result[, .(est_mean = mean(h_EigenPrism_main, na.rm = T),
                                                                 est_var_m = mean((h_EigenPrism_CI2 - h_EigenPrism_CI1)/(2 * qnorm(1-0.2/2)), na.rm = T), # Eg assume alpha level = 0.2
                                                                 NA_i = mean(is.na(h_EigenPrism_main)),
                                                                 rho_e = rho_e), 
                                                                 by = .(i,n,rho_e)][, .(MSE = mean((est_mean - rho_e)^2, na.rm = T),
                                                                                        est_mean = mean(est_mean, na.rm = T),
                                                                                        est_var_m = mean(est_var_m, na.rm = T),
                                                                                        est_var = var(est_mean, na.rm = T),
                                                                                        NA_main = sum(NA_i),
                                                                                        N = .N,
                                                                                        relative_ratio = mean((est_var_m - var(est_mean, na.rm = T))/var(est_mean, na.rm = T)),
                                                                                        relative_ratio_var = var((est_var_m - var(est_mean, na.rm = T))/var(est_mean, na.rm = T))), by = .(n,rho_e)] %>% 
  setorder(., by = n)

# reproduce the result of h_Eg 2013 paper p = 500
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/test_EigenPrism/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_250_500_p_500_rho_e_0.1_0.3_0.5_0.7_0.9_decor_FALSE_subd_0_iter_1000_nsub_1_bs_full_h_EigenPrism_kernel_est_main_c_betam_8_c_betai_2_Var_null" 
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


summary_result_h_Eg_I_normal_p_500_n_500_rho_e <- sub_result[, .(est_mean = mean(h_EigenPrism_main, na.rm = T),
                                                                 est_var_m = mean((h_EigenPrism_CI2 - h_EigenPrism_CI1)/(2 * qnorm(1-0.2/2)), na.rm = T), # Eg assume alpha level = 0.2
                                                                 NA_i = mean(is.na(h_EigenPrism_main)),
                                                                 rho_e = rho_e), 
                                                             by = .(i,n,rho_e)][, .(MSE = mean((est_mean - rho_e)^2, na.rm = T),
                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                    est_var_m = mean(est_var_m, na.rm = T),
                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                    NA_main = sum(NA_i),
                                                                                    N = .N,
                                                                                    relative_ratio = mean((est_var_m - var(est_mean, na.rm = T))/var(est_mean, na.rm = T)),
                                                                                    relative_ratio_var = var((est_var_m - var(est_mean, na.rm = T))/var(est_mean, na.rm = T))), by = .(n,rho_e)] %>% 
  setorder(., by = n)
