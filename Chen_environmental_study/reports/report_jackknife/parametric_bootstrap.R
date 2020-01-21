## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)


# p = 100
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_p_100_rho_e_0.5_decor_FALSE_subd_0_iter_100_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,true_h2 := 0.5] 


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             true_h2,
                             structure,
                             decor,
                             x_dist)]


summary_result <- sub_result[, .(est_h2 = mean(h_GCTA_main, na.rm = T),
                                 NA_i = mean(is.na(h_GCTA_main)),
                                 true_h2 = true_h2[1]), by = .(i,n)][, .(MSE = mean((est_h2 - true_h2)^2, na.rm = T),
                                                                                est_h2_var = var(est_h2, na.rm = T),
                                                                                est_h2 = mean(est_h2, na.rm = T),
                                                                                NA_main = sum(NA_i)), by = n] %>% 
                         setorder(., by = n)

summary_result_bs <- sub_result[, .(est_h2_bs = mean(S1_bs, na.rm = T),
                                    est_h2_var_bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T),
                                    relative_ratio = mean((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T),
                                    relative_ratio_var = var((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T)),by = .(n)] %>%
                     setorder(., by = n)

summary_result_GCTA_parametric_bs_p_100 <- merge(summary_result,summary_result_bs, by = "n")


# p = 100 niter = 1000
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_p_100_rho_e_0.5_decor_FALSE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,true_h2 := 0.5] 


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             true_h2,
                             structure,
                             decor,
                             x_dist)]


summary_result <- sub_result[, .(est_h2 = mean(h_GCTA_main, na.rm = T),
                                 NA_i = mean(is.na(h_GCTA_main)),
                                 true_h2 = true_h2[1]), by = .(i,n)][, .(MSE = mean((est_h2 - true_h2)^2, na.rm = T),
                                                                         est_h2_var = var(est_h2, na.rm = T),
                                                                         est_h2 = mean(est_h2, na.rm = T),
                                                                         NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

summary_result_bs <- sub_result[, .(est_h2_bs = mean(S1_bs, na.rm = T),
                                    est_h2_var_bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T),
                                    relative_ratio = mean((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T),
                                    relative_ratio_var = var((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_result_GCTA_parametric_bs_p_100_niter_1000 <- merge(summary_result,summary_result_bs, by = "n")

hist_plot_p_100_niter_1000 <- ggplot(sub_result, aes(x=h_GCTA_main, fill = as.character(n))) + 
  geom_histogram(alpha = 0.6, position = 'identity')

# p = 1000
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_p_1000_rho_e_0.5_decor_FALSE_subd_0_iter_100_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,true_h2 := 0.5] 

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             true_h2,
                             structure,
                             decor,
                             x_dist)]


summary_result <- sub_result[, .(est_h2 = mean(h_GCTA_main, na.rm = T),
                                 NA_i = mean(is.na(h_GCTA_main)),
                                 true_h2 = true_h2[1]), by = .(i,n)][, .(MSE = mean((est_h2 - true_h2)^2, na.rm = T),
                                                                         est_h2_var = var(est_h2, na.rm = T),
                                                                         est_h2 = mean(est_h2, na.rm = T),
                                                                         NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

summary_result_bs <- sub_result[, .(est_h2_bs = mean(S1_bs, na.rm = T),
                                    est_h2_var_bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T),
                                    relative_ratio = mean((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T),
                                    relative_ratio_var = var((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_result_GCTA_parametric_bs_p_1000_niter_100 <- merge(summary_result,summary_result_bs, by = "n")

hist_plot_p_1000_niter_100 <- ggplot(sub_result, aes(x=h_GCTA_main))

# p = 1000 iter = 1000
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_p_1000_rho_e_0.5_decor_FALSE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,true_h2 := 0.5] 

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             true_h2,
                             structure,
                             decor,
                             x_dist)]


summary_result <- sub_result[, .(est_h2 = mean(h_GCTA_main, na.rm = T),
                                 NA_i = mean(is.na(h_GCTA_main)),
                                 true_h2 = true_h2[1]), by = .(i,n)][, .(MSE = mean((est_h2 - true_h2)^2, na.rm = T),
                                                                         est_h2_var = var(est_h2, na.rm = T),
                                                                         est_h2 = mean(est_h2, na.rm = T),
                                                                         NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

summary_result_bs <- sub_result[, .(est_h2_bs = mean(S1_bs, na.rm = T),
                                    est_h2_var_bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T),
                                    relative_ratio = mean((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T),
                                    relative_ratio_var = var((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_result_GCTA_parametric_bs_p_1000_niter_1000 <- merge(summary_result,summary_result_bs, by = "n")

hist_plot_p_1000_niter_1000 <- ggplot(sub_result, aes(x=h_GCTA_main, fill = as.character(n))) + 
  geom_histogram(alpha = 0.6, position = 'identity')




# p = 21 niter = 1000 un
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_un_main_0.5_inter_0_n_50_75_100_150_p_21_rho_e_0.5_decor_FALSE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,true_h2 := 0.5] 


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             true_h2,
                             structure,
                             decor,
                             x_dist)]


summary_result <- sub_result[, .(est_h2 = mean(h_GCTA_main, na.rm = T),
                                 NA_i = mean(is.na(h_GCTA_main)),
                                 true_h2 = true_h2[1]), by = .(i,n)][, .(MSE = mean((est_h2 - true_h2)^2, na.rm = T),
                                                                         est_h2_var = var(est_h2, na.rm = T),
                                                                         est_h2 = mean(est_h2, na.rm = T),
                                                                         NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)

summary_result_bs <- sub_result[, .(est_h2_bs = mean(S1_bs, na.rm = T),
                                    est_h2_var_bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T),
                                    relative_ratio = mean((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T),
                                    relative_ratio_var = var((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_result_GCTA_parametric_bs_p_21_un_niter_1000 <- merge(summary_result,summary_result_bs, by = "n")

hist_plot_p_21_un_niter_1000 <- ggplot(sub_result, aes(x=h_GCTA_main, fill = as.character(n))) + 
  geom_histogram(alpha = 0.6, position = 'identity')

