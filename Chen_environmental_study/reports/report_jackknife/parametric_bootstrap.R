## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)

CI_helper_1 <- function(x, conf_level = 0.05) {
quantile(x, probs = c(conf_level/2, 1 - conf_level/2), na.rm = T, type = 4, names = FALSE)
}

CI_helper_2 <- function(x, L, R){
  fn <- function(x, L,R ){
    if(x <= R & x >= L){
      TRUE
      } else {
        FALSE
      }
    }
    mapply(fn, x = x, L = L, R = R) %>% mean(., na.rm = T)
}

z <- qnorm(1 - 0.05/2)

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
                                                                         N = .N,
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

# p = 100 niter = 1000 rho_e = (0.1 - 0.9) 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "./decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_p_100_rho_e_0.1_0.3_0.5_0.7_0.9_decor_FALSE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


summary_result <- sub_result[, .(est_h2 = mean(h_GCTA_main, na.rm = T),
                                 NA_i = mean(is.na(h_GCTA_main)),
                                 rho_e = rho_e), by = .(i,n,rho_e)][, .(MSE = mean((est_h2 - rho_e)^2, na.rm = T),
                                                                         est_h2_var = var(est_h2, na.rm = T),
                                                                         est_h2 = mean(est_h2, na.rm = T),
                                                                        N = .N,
                                                                         NA_main = sum(NA_i)), by = .(n, rho_e)] %>% 
  setorder(., by = n)

summary_result_bs <- sub_result[, .(est_h2_bs = mean(S1_bs, na.rm = T),
                                    est_h2_var_bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T),
                                    relative_ratio = mean((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T),
                                    relative_ratio_var = var((S1_v_bs.sub_h_GCTA_main - var(h_GCTA_main, na.rm = T)))/var(h_GCTA_main, na.rm = T)),by = .(n, rho_e)] %>%
  setorder(., by = n)

summary_result_GCTA_parametric_bs_p_100_niter_1000_rho_e_0.1_0.9 <- merge(summary_result,summary_result_bs, by = c("n","rho_e"))

hist_plot_p_100_niter_1000_rho_e_0.1_0.9 <- ggplot(sub_result[n == 150, ], aes(x=h_GCTA_main, fill = as.character(rho_e))) + 
  geom_histogram(alpha = 0.6, position = 'identity', bins = 50)


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
                                                                         N = .N,
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


# CI investigation

# p = 500 niter = 1000 rho_e = (0.1 - 0.9) GCTA
#############################################################################################################################

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_250_500_p_500_rho_e_0.1_0.3_0.5_0.7_0.9_decor_FALSE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[n == 250 | n == 500,]

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

summary_result <- sub_result[, .(CI.h2.emp.L = CI_helper_1(h_GCTA_main)[1],
                                 CI.h2.emp.R = CI_helper_1(h_GCTA_main)[2],
                                 CI.h2.emp.l = CI_helper_1(h_GCTA_main)[2] - CI_helper_1(h_GCTA_main)[1],
                                 var.h2.emp = var(h_GCTA_main, na.rm = T),
                                 h2 = mean(h_GCTA_main, na.rm = T),
                                 N = .N,
                                 NA_main = sum(is.na(h_GCTA_main))), by = .(n, rho_e)] 

summary_result_para_bs <- sub_result[, .(h2.bs = mean(S1_bs, na.rm = T),
                                         var.h2.bs = mean(S1_v_bs.sub_h_GCTA_main, na.rm = T)),
                                     by = .(n, rho_e)] %>%
  setorder(., by = n)

sub_result[,CI.h2.bs.L := S1_bs - sqrt(S1_v_bs.sub_h_GCTA_main) * z]
sub_result[,CI.h2.bs.R := S1_bs + sqrt(S1_v_bs.sub_h_GCTA_main) * z]
sub_result[,CI.h2.bs.l := 2 * sqrt(S1_v_bs.sub_h_GCTA_main) * z]
summary_CI_para_bs <- sub_result[, .(CI.h2.bs.c = CI_helper_2(rho_e, CI.h2.bs.L, CI.h2.bs.R),
                                     CI.h2.bs.L = mean(CI.h2.bs.L, na.rm = T),
                                     CI.h2.bs.R = mean(CI.h2.bs.R, na.rm = T),
                                     CI.h2.bs.l = mean(CI.h2.bs.l, na.rm = T)), by = .(n, rho_e)]
summary_result_para_bs <- merge(summary_result_para_bs, summary_CI_para_bs, by = c("n","rho_e"))
summary_result_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 <- merge(summary_result,summary_result_para_bs, by = c("n","rho_e"))

GCTA_CI_covarage <- summary_result_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0.95)+
  ggtitle("GCTA CI coverage")

GCTA_CI_length <- summary_result_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  ggtitle("GCTA CI lenght")

# p = 500 niter = 1000 rho_e = (0.1 - 0.9) Dicker
#############################################################################################################################

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_250_500_p_500_rho_e_0.1_0.3_0.5_0.7_0.9_decor_FALSE_subd_0_iter_1000_nsub_1000_bs_para-bs_Dicker_2013_kernel_est_main_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

summary_result <- sub_result[, .(CI.h2.emp.L = CI_helper_1(h_Dicker_main)[1],
                                 CI.h2.emp.R = CI_helper_1(h_Dicker_main)[2],
                                 CI.h2.emp.l = CI_helper_1(h_Dicker_main)[2] - CI_helper_1(h_Dicker_main)[1],
                                 var.h2.emp = var(h_Dicker_main, na.rm = T),
                                 h2 = mean(h_Dicker_main, na.rm = T),
                                 N = .N,
                                 NA_main = sum(is.na(h_Dicker_main))), by = .(n, rho_e)] 

################################################################
summary_result_para_bs <- sub_result[, .(h2.bs = mean(S1_bs, na.rm = T),
                                         var.h2.bs = mean(S1_v_bs.sub_h_Dicker_main, na.rm = T)),
                                     by = .(n, rho_e)]
sub_result[,CI.h2.bs.L := S1_bs - sqrt(S1_v_bs.sub_h_Dicker_main) * z]
sub_result[,CI.h2.bs.R := S1_bs + sqrt(S1_v_bs.sub_h_Dicker_main) * z]
sub_result[,CI.h2.bs.l := 2 * sqrt(S1_v_bs.sub_h_Dicker_main) * z]
summary_CI_para_bs <- sub_result[, .(CI.h2.bs.c = CI_helper_2(rho_e, CI.h2.bs.L, CI.h2.bs.R),
                                     CI.h2.bs.L = mean(CI.h2.bs.L, na.rm = T),
                                     CI.h2.bs.R = mean(CI.h2.bs.R, na.rm = T),
                                     CI.h2.bs.l = mean(CI.h2.bs.l, na.rm = T)), by = .(n, rho_e)]
summary_result_para_bs <- merge(summary_result_para_bs, summary_CI_para_bs, by = c("n","rho_e"))
###################################################################
summary_result_Dicker <- sub_result[, .(var.h2.Dicker = mean(h_Dicker_var, na.rm = T)),
                                     by = .(n, rho_e)]
sub_result[,CI.h2.Dicker.L := h_Dicker_main - sqrt(h_Dicker_var) * z]
sub_result[,CI.h2.Dicker.R := h_Dicker_main + sqrt(h_Dicker_var) * z]
sub_result[,CI.h2.Dicker.l := 2 * sqrt(h_Dicker_var) * z]
summary_CI_Dicker <- sub_result[, .(CI.h2.Dicker.c = CI_helper_2(rho_e, CI.h2.Dicker.L, CI.h2.Dicker.R),
                                     CI.h2.Dicker.L = mean(CI.h2.Dicker.L, na.rm = T),
                                     CI.h2.Dicker.R = mean(CI.h2.Dicker.R, na.rm = T),
                                     CI.h2.Dicker.l = mean(CI.h2.Dicker.l, na.rm = T)), by = .(n, rho_e)]
summary_result_Dicker <- merge(summary_result_Dicker, summary_CI_Dicker, by = c("n","rho_e"))
summary_result_Dicker_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 <- merge(summary_result,summary_result_para_bs,by = c("n","rho_e"))
summary_result_Dicker_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 <- merge(summary_result_Dicker_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9, summary_result_Dicker, by = c("n","rho_e"))

Dicker_CI_covarage <- summary_result_Dicker_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
                      tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
                      ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
                      geom_line() + 
                      geom_point() +
                      geom_hline(yintercept=0.95)+
                      ggtitle("Dicker CI coverage")

Dicker_CI_length <- summary_result_Dicker_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
                    tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
                    ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
                    geom_line() + 
                    geom_point() +
                    ggtitle("Dicker CI lenght")

# p = 500 niter = 1000 rho_e = (0.1 - 0.9) EigenPrism
#############################################################################################################################

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/test_EigenPrism/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_250_500_p_500_rho_e_0.1_0.3_0.5_0.7_0.9_decor_FALSE_subd_0_iter_1000_nsub_1_bs_full_h_EigenPrism_kernel_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

summary_result <- sub_result[, .(CI.h2.emp.L = CI_helper_1(h_EigenPrism_main)[1],
                                 CI.h2.emp.R = CI_helper_1(h_EigenPrism_main)[2],
                                 CI.h2.emp.l = CI_helper_1(h_EigenPrism_main)[2] - CI_helper_1(h_EigenPrism_main)[1],
                                 var.h2.emp = var(h_EigenPrism_main, na.rm = T),
                                 h2 = mean(h_EigenPrism_main, na.rm = T),
                                 N = .N,
                                 NA_main = sum(is.na(h_EigenPrism_main))), by = .(n, rho_e)] 

###################################################################
# Inference info based on Eigenprism
sub_result[,CI.h2.EigenPrism.L := h_EigenPrism_CI1]
sub_result[,CI.h2.EigenPrism.R := h_EigenPrism_CI2]
sub_result[,CI.h2.EigenPrism.l := h_EigenPrism_CI2 - h_EigenPrism_CI1]
summary_CI_EigenPrism <- sub_result[, .(CI.h2.EigenPrism.c = CI_helper_2(rho_e, CI.h2.EigenPrism.L, CI.h2.EigenPrism.R),
                                    CI.h2.EigenPrism.L = mean(CI.h2.EigenPrism.L, na.rm = T),
                                    CI.h2.EigenPrism.R = mean(CI.h2.EigenPrism.R, na.rm = T),
                                    CI.h2.EigenPrism.l = mean(CI.h2.EigenPrism.l, na.rm = T)), by = .(n, rho_e)]

summary_result_EigenPrism <- summary_CI_EigenPrism

summary_result_EigenPrism_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 <- merge(summary_result,summary_result_EigenPrism,by = c("n","rho_e"))




EigenPrism_CI_covarage <- summary_result_EigenPrism_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0.95)+
  ggtitle("EigenPrism CI coverage")

EigenPrism_CI_length <- summary_result_EigenPrism_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  ggtitle("EigenPrism CI lenght")

### CI coverage for all three methods 
CI_EigenPrism <- summary_result_EigenPrism_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9[, .(n = n,
                                                                                           rho_e = rho_e,
                                                                                           CI.h2.Eg.c = CI.h2.EigenPrism.c,
                                                                                           CI.h2.Eg.l = CI.h2.EigenPrism.l)]

CI_Dicker <- summary_result_Dicker_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9[, .(n = n,
                                                                                   rho_e = rho_e,
                                                                                   CI.h2.Dicker.c = CI.h2.Dicker.c,
                                                                                   CI.h2.Dicker.l = CI.h2.Dicker.l)]

CI_GCTA <- summary_result_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9[, .(n = n,
                                                                                rho_e = rho_e,
                                                                                CI.h2.bs.c = CI.h2.bs.c,
                                                                                CI.h2.bs.l = CI.h2.bs.l)]
CI_table <- merge(CI_EigenPrism, CI_Dicker, by = c("n", "rho_e"), all = T)
CI_table <- merge(CI_table, CI_GCTA, by = c("n", "rho_e"))

CI_covarage <- CI_table %>%
  tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0.95)+
  facet_wrap_paginate(facets = vars(n), ncol = 2, nrow = 1, page = 1)+
  ggtitle("GCTA, Dikcer, EigenPrsim CI coverage")

CI_length <- CI_table %>%
  tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=method)) +
  geom_line() + 
  geom_point() +
  facet_wrap_paginate(facets = vars(n), ncol = 2, nrow = 1, page = 1)+
  ggtitle("GCTA, Dikcer, EigenPrsim CI lenght")


# p = 500 niter = 1000 rho_e = (0.1 - 0.9) GCTA total
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse__normal_structure_I_main_0.5_inter_0.1_n_250_500_p_31_rho_e_0.1_0.3_0.5_0.7_0.9_decor_TRUE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_total_c_betam_8_c_betai_2"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

summary_result <- sub_result[, .(CI.h2.emp.L = CI_helper_1(h_GCTA_total)[1],
                                 CI.h2.emp.R = CI_helper_1(h_GCTA_total)[2],
                                 CI.h2.emp.l = CI_helper_1(h_GCTA_total)[2] - CI_helper_1(h_GCTA_total)[1],
                                 var.h2.emp = var(h_GCTA_total, na.rm = T),
                                 h2 = mean(h_GCTA_total, na.rm = T),
                                 N = .N,
                                 NA_total = sum(is.na(h_GCTA_total))), by = .(n, rho_e)] 

summary_result_para_bs <- sub_result[, .(h2.bs = mean(S1_bs, na.rm = T),
                                         var.h2.bs = mean(S1_v_bs.sub_h_GCTA_total, na.rm = T)),
                                     by = .(n, rho_e)] %>%
  setorder(., by = n)

sub_result[,CI.h2.bs.L := S1_bs - sqrt(S1_v_bs.sub_h_GCTA_total) * z]
sub_result[,CI.h2.bs.R := S1_bs + sqrt(S1_v_bs.sub_h_GCTA_total) * z]
sub_result[,CI.h2.bs.l := 2 * sqrt(S1_v_bs.sub_h_GCTA_total) * z]
summary_CI_para_bs <- sub_result[, .(CI.h2.bs.c = CI_helper_2(rho_e, CI.h2.bs.L, CI.h2.bs.R),
                                     CI.h2.bs.L = mean(CI.h2.bs.L, na.rm = T),
                                     CI.h2.bs.R = mean(CI.h2.bs.R, na.rm = T),
                                     CI.h2.bs.l = mean(CI.h2.bs.l, na.rm = T)), by = .(n, rho_e)]
summary_result_para_bs <- merge(summary_result_para_bs, summary_CI_para_bs, by = c("n","rho_e"))
summary_result_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 <- merge(summary_result,summary_result_para_bs, by = c("n","rho_e"))

GCTA_CI_covarage_normal_total <- summary_result_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0.95)+
  ggtitle("GCTA CI coverage")

GCTA_CI_length_normal_total <- summary_result_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  ggtitle("GCTA CI lenght")



# PCB p = 500 niter = 1000 rho_e = (0.1 - 0.9) GCTA total with decorr ####
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse__PCB_1999_structure_un_main_0.5_inter_0.1_n_100_150_231_p_21_rho_e_0.1_0.3_0.5_0.7_0.9_decor_TRUE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_total_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

summary_result <- sub_result[, .(CI.h2.emp.L = CI_helper_1(h_GCTA_total)[1],
                                 CI.h2.emp.R = CI_helper_1(h_GCTA_total)[2],
                                 CI.h2.emp.l = CI_helper_1(h_GCTA_total)[2] - CI_helper_1(h_GCTA_total)[1],
                                 var.h2.emp = var(h_GCTA_total, na.rm = T),
                                 h2 = mean(h_GCTA_total, na.rm = T),
                                 N = .N,
                                 NA_total = sum(is.na(h_GCTA_total))), by = .(n, rho_e)] 

summary_result_para_bs <- sub_result[, .(h2.bs = mean(S1_bs, na.rm = T),
                                         var.h2.bs = mean(S1_v_bs.sub_h_GCTA_total, na.rm = T)),
                                     by = .(n, rho_e)] %>%
  setorder(., by = n)

sub_result[,CI.h2.bs.L := S1_bs - sqrt(S1_v_bs.sub_h_GCTA_total) * z]
sub_result[,CI.h2.bs.R := S1_bs + sqrt(S1_v_bs.sub_h_GCTA_total) * z]
sub_result[,CI.h2.bs.l := 2 * sqrt(S1_v_bs.sub_h_GCTA_total) * z]
summary_CI_para_bs <- sub_result[, .(CI.h2.bs.c = CI_helper_2(rho_e, CI.h2.bs.L, CI.h2.bs.R),
                                     CI.h2.bs.L = mean(CI.h2.bs.L, na.rm = T),
                                     CI.h2.bs.R = mean(CI.h2.bs.R, na.rm = T),
                                     CI.h2.bs.l = mean(CI.h2.bs.l, na.rm = T)), by = .(n, rho_e)]
summary_result_para_bs <- merge(summary_result_para_bs, summary_CI_para_bs, by = c("n","rho_e"))
summary_result_PCB_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 <- merge(summary_result,summary_result_para_bs, by = c("n","rho_e"))

GCTA_CI_covarage_PCB_total <- summary_result_PCB_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0.95)+
  ggtitle("GCTA CI coverage")

GCTA_CI_length_PCB_total <- summary_result_PCB_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=method)) +
  geom_line() + 
  geom_point() +
  facet_wrap_paginate(facets = vars(n), ncol = 2, nrow = 2, page = 1)+
  ggtitle("PCB total CI lenght")

## 
# hist(sub_result[(n == 250)&(rho_e == 0.5),h_GCTA_total], nclass = 30)
# hist(sub_result[(n == 250)&(rho_e == 0.5),S1_bs], nclass = 30, add = T)

# PCB p = 21 niter = 1000 rho_e = (0.1, 0.5, 0.9) GCTA total with decorr ####
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse_Glasso_PCB_1999_structure_un_main_0.5_inter_0.1_n_250_500_p_21_rho_e_0.5_0.1_0.9_decor_TRUE_subd_0_iter_1000_nsub_1000_bs_para-bs_h_GCTA_kernel_est_total_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

summary_result <- sub_result[, .(CI.h2.emp.L = CI_helper_1(h_GCTA_total)[1],
                                 CI.h2.emp.R = CI_helper_1(h_GCTA_total)[2],
                                 CI.h2.emp.l = CI_helper_1(h_GCTA_total)[2] - CI_helper_1(h_GCTA_total)[1],
                                 var.h2.emp = var(h_GCTA_total, na.rm = T),
                                 h2 = mean(h_GCTA_total, na.rm = T),
                                 N = .N,
                                 NA_total = sum(is.na(h_GCTA_total))), by = .(n, rho_e)] 

summary_result_para_bs <- sub_result[, .(h2.bs = mean(S1_bs, na.rm = T),
                                         var.h2.bs = mean(S1_v_bs.sub_h_GCTA_total, na.rm = T)),
                                     by = .(n, rho_e)] %>%
  setorder(., by = n)

sub_result[,CI.h2.bs.L := S1_bs - sqrt(S1_v_bs.sub_h_GCTA_total) * z]
sub_result[,CI.h2.bs.R := S1_bs + sqrt(S1_v_bs.sub_h_GCTA_total) * z]
sub_result[,CI.h2.bs.l := 2 * sqrt(S1_v_bs.sub_h_GCTA_total) * z]
summary_CI_para_bs <- sub_result[, .(CI.h2.bs.c = CI_helper_2(rho_e, CI.h2.bs.L, CI.h2.bs.R),
                                     CI.h2.bs.L = mean(CI.h2.bs.L, na.rm = T),
                                     CI.h2.bs.R = mean(CI.h2.bs.R, na.rm = T),
                                     CI.h2.bs.l = mean(CI.h2.bs.l, na.rm = T)), by = .(n, rho_e)]
summary_result_para_bs <- merge(summary_result_para_bs, summary_CI_para_bs, by = c("n","rho_e"))
summary_result_PCB_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.5_0.9 <- merge(summary_result,summary_result_para_bs, by = c("n","rho_e"))

GCTA_CI_covarage_PCB_total <- summary_result_PCB_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".c"), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=as.character(n), shape = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept=0.95)+
  ggtitle("GCTA CI coverage")

GCTA_CI_length_PCB_total <- summary_result_PCB_total_GCTA_parametric_bs_p_500_niter_1000_rho_e_0.1_0.9 %>%
  tidyr::gather(., ends_with(".l", ignore.case = FALSE), key = "method", value = "value")  %>%
  ggplot(., aes(x=rho_e, y=value, color=method)) +
  geom_line() + 
  geom_point() +
  facet_wrap_paginate(facets = vars(n), ncol = 2, nrow = 2, page = 1)+
  ggtitle("PCB total CI lenght")

## 
# hist(sub_result[(n == 250)&(rho_e == 0.5),h_GCTA_total], nclass = 30)
# hist(sub_result[(n == 250)&(rho_e == 0.5),S1_bs], nclass = 30, add = T)


#############################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse__PCB_1999_structure_un_main_0.5_inter_0.1_n_100_150_231_p_21_rho_e_0.1_0.3_0.5_0.7_0.9_decor_TRUE_subd_0_iter_10_nsub_1000_bs_para-bs_h_GCTA_kernel_est_total_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result_PCB <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result_PCB[i == 1 & n == 150 & rho_e == 0.5, hist(sub_h_GCTA_total)]
sub_result_PCB[i == 1 & n == 150 & rho_e == 0.5, h_GCTA_total]

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse__normal_structure_I_main_0.5_inter_0.1_n_250_500_p_31_rho_e_0.1_0.3_0.5_0.7_0.9_decor_TRUE_subd_0_iter_10_nsub_1000_bs_para-bs_h_GCTA_kernel_est_total_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result_normal <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result_normal[i == 3 & n == 250 & rho_e == 0.5, hist(sub_h_GCTA_total)]
sub_result_normal[i == 3 & n == 250 & rho_e == 0.5, (h_GCTA_total)]

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse_None_normal_structure_un_main_0.5_inter_0.1_n_250_500_p_21_rho_e_0.5_decor_TRUE_subd_0_iter_10_nsub_1000_bs_para-bs_h_GCTA_kernel_est_total_c_betam_8_c_betai_2/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result_normal_un_total <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result_normal_un_total[, h_GCTA_total - S1_bs]

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_hist_sparse_None_normal_structure_un_main_0.5_inter_0_n_50_75_100_150_p_21_rho_e_0.5_decor_TRUE_subd_0_iter_10_nsub_1000_bs_para-bs_h_GCTA_kernel_est_main_c_betam_8_c_betai_2"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result_normal_un_main <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result_normal_un_main[, h_GCTA_main - S1_bs]
