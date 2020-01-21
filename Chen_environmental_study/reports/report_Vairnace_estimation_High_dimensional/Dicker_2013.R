## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)


# reproduce the result of dicker 2013 paper 
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/Dicker_2013/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_250_500_p_500_rho_e_0.5_decor_FALSE_subd_0_iter_1000_nsub_1_Dicker_est_main_c_betam_1_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


summary_result_dicker_I_normal <- sub_result[, .(est_mean = mean(Dicker_main, na.rm = T),
                                                 est_var_m = mean(Dicker_var, na.rm = T),
                                                 NA_i = mean(is.na(Dicker_main)),
                                                 var_main_effect = var_main_effect[1]), 
                                             by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                              est_mean = mean(est_mean, na.rm = T),
                                                              est_var_m = mean(est_var_m, na.rm = T),
                                                              est_var = var(est_mean, na.rm = T),
                                                              NA_main = sum(NA_i)), by = n] %>% 
                                   setorder(., by = n)
