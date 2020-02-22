## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)


# delete d = d p = 2
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_2_rho_e_0.5_decor_FALSE_subd_1_iter_1000_nsub_0_median_null_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,var_main_effect := NULL] 
sub_result[,var_main_effect := 0] # the targe is the median of chi with df = 1

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 1), by = n]


summary_result_median <- sub_result[, .(est_mean = mean(median_main, na.rm = T),
                                        NA_i = mean(is.na(median_main)),
                                        var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                est_var = var(est_mean, na.rm = T),
                                                                                                est_mean = mean(est_mean, na.rm = T),
                                                                                                NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_median_jack <- sub_result[, .(median_main_jack = mean(S1_jack.median_main, na.rm = T),
                                             median_v_jack = mean(S1_v_jack_1, na.rm = T),
                                             relative_ratio = mean((S1_v_jack_1 - var(median_main, na.rm = T)))/var(median_main, na.rm = T),
                                             relative_ratio_var = var((S1_v_jack_1 - var(median_main, na.rm = T)))/var(median_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_result_median_jack <- merge(summary_result_median,summary_result_median_jack, by = "n")
summary_result_median_jack_d_1 <- merge(summary_result_median_jack, d, by = "n") %>% setorder(., n)

# delete d = 1 p = 2
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_2_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_iter_1000_nsub_2828_median_null_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,var_main_effect := NULL] 
sub_result[,var_main_effect := 0] # the targe is the median of chi with df = 1

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 0.5), by = n]


summary_result_median <- sub_result[, .(est_mean = mean(median_main, na.rm = T),
                                        NA_i = mean(is.na(median_main)),
                                        var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                est_var = var(est_mean, na.rm = T),
                                                                                                est_mean = mean(est_mean, na.rm = T),
                                                                                                NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_median_jack <- sub_result[, .(median_main_jack = mean(S1_jack.median_main, na.rm = T),
                                             median_v_jack = mean(S1_v_jack_1, na.rm = T),
                                             relative_ratio = mean((S1_v_jack_1 - var(median_main, na.rm = T)))/var(median_main, na.rm = T),
                                             relative_ratio_var = var((S1_v_jack_1 - var(median_main, na.rm = T)))/var(median_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_result_median_jack <- merge(summary_result_median,summary_result_median_jack, by = "n")
summary_result_median_jack_d_d <- merge(summary_result_median_jack, d, by = "n") %>% setorder(., n)



# delete d = 1  p = 100 n = 50 - 1500 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_2000_p_100_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d=1), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_1_p_100_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_1_p_100_n_50_1500 <- merge(summary_final_EigenPrism_8_d_1_p_100_n_50_1500,d, by = "n") %>% setorder(., n)

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_1_p_100_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_1_p_100_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_1_p_100_n_50_1500, d , by = "n") %>% setorder(., n)

# delete d = d p = 100 n = 50 - 1500
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_2000_p_100_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_250_500_750_1000_iter_100_nsub_89443_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 0.5), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_d_p_100_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_d_p_100_n_50_1500 <- merge(summary_final_EigenPrism_8_d_d_p_100_n_50_1500, d, by = "n") %>% setorder(., n)

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_d_p_100_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_d_p_100_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_d_p_100_n_50_1500, d, by = "n") %>% setorder(., n)

# delete d = 25 p = 100
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_p_100_rho_e_0.5_decor_FALSE_subd_25_iter_100_nsub_58095_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 25), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_25_p_100_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_25_p_100_n_50_1500 <- merge(summary_final_EigenPrism_8_d_25_p_100_n_50_1500, d, by = "n")

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_25_p_100_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_25_p_100_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_25_p_100_n_50_1500, d, by = "n")



# delete d = .75  p = 100
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_p_100_rho_e_0.5_decor_FALSE_subd_38_56_75_112_150_375_750_1125_iter_100_nsub_58095_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 0.75), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_.75_p_100_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_.75_p_100_n_50_1500 <- merge(summary_final_EigenPrism_8_d_.75_p_100_n_50_1500, d, by = "n")

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_.75_p_100_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_.75_p_100_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_.75_p_100_n_50_1500, d, by = "n")


# delete d = 1  p = 1000 n = 50 - 1500 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_2000_p_1000_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 1), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_1_p_1000_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_1_p_1000_n_50_1500 <- merge(summary_final_EigenPrism_8_d_1_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_1_p_1000_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_1_p_1000_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_1_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# delete d = 0.5 p = 1000 n = 50 - 1500
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_2000_p_1000_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_250_500_750_1000_iter_100_nsub_89443_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 0.5), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_d_p_1000_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_d_p_1000_n_50_1500 <- merge(summary_final_EigenPrism_8_d_d_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_d_p_1000_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_d_p_1000_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_d_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# delete d = 0.75 p = 1000 n = 50 - 1500
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_p_1000_rho_e_0.5_decor_FALSE_subd_38_56_75_112_150_375_750_1125_iter_100_nsub_58095_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 0.75), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_.75_p_1000_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_.75_p_1000_n_50_1500 <- merge(summary_final_EigenPrism_8_d_.75_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_.75_p_1000_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_.75_p_1000_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_.75_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# delete d = 25 p = 1000 n = 50 - 1500
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_p_1000_rho_e_0.5_decor_FALSE_subd_25_iter_100_nsub_58095_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 25), by = n]


# EigenPrism
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                            est_var_m = mean((EigenPrism_CI/(2*qnorm(1-0.1)))^2, na.rm = T),
                                            NA_i = mean(is.na(EigenPrism_main)),
                                            var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                    est_mean = mean(est_mean, na.rm = T),
                                                                                                    est_var_m = mean(est_var_m, na.rm = T), 
                                                                                                    est_var = var(est_mean, na.rm = T),
                                                                                                    NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_EigenPrism_jack <- sub_result[, .(EigenPrism_main_jack = mean(S1_jack.EigenPrism_main, na.rm = T),
                                                 EigenPrism_v_jack = mean(S1_v_jack_1, na.rm = T),
                                                 EigenPrism_v_jack_var = var(S1_v_jack_1, na.rm = T),
                                                 relative_ratio = mean((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 relative_ratio_var = var((S1_v_jack_1 - var(EigenPrism_main, na.rm = T)))/var(EigenPrism_main, na.rm = T),
                                                 .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_EigenPrism_8_d_25_p_1000_n_50_1500 <- merge(summary_result_EigenPrism,summary_result_EigenPrism_jack, by = "n")
summary_final_EigenPrism_8_d_25_p_1000_n_50_1500 <- merge(summary_final_EigenPrism_8_d_25_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T),
                                              relative_ratio = mean((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              relative_ratio_var = var((S2_v_jack_1 - var(GCTA_rr_main, na.rm = T)))/var(GCTA_rr_main, na.rm = T),
                                              .N),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_25_p_1000_n_50_1500 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_25_p_1000_n_50_1500 <- merge(summary_final_GCTA_rr_8_d_25_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)


# Dicker's method 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/Dicker_2013/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_p_1000_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_Dicker_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,.(d = 1), by = n]

summary_result_Dicker <- sub_result[, .(est_mean = mean(Dicker_main, na.rm = T),
                                                 est_var_m = mean(Dicker_var, na.rm = T),
                                                 NA_i = mean(is.na(Dicker_main)),
                                                 var_main_effect = var_main_effect[1]), 
                                             by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                              est_mean = mean(est_mean, na.rm = T),
                                                              est_var_m = mean(est_var_m, na.rm = T),
                                                              est_var = var(est_mean, na.rm = T),
                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_Dicker_jack <- sub_result[, .(Dicker_main_jack = mean(S1_jack.Dicker_main, na.rm = T),
                                             Dicker_v_jack = mean(S1_v_jack_1, na.rm = T),
                                             relative_ratio = mean((S1_v_jack_1 - var(Dicker_main, na.rm = T)))/var(Dicker_main, na.rm = T),
                                             relative_ratio_var = var((S1_v_jack_1 - var(Dicker_main, na.rm = T)))/var(Dicker_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)


summary_result_Dicker_jack_8_d_25_p_1000_n_50_1500 <- merge(summary_result_Dicker,summary_result_Dicker_jack, by = "n")
summary_result_Dicker_jack_8_d_25_p_1000_n_50_1500 <- merge(summary_result_Dicker_jack_8_d_25_p_1000_n_50_1500, d, by = "n") %>% setorder(., n)
