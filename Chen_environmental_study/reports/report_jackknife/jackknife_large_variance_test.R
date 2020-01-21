## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr_Eg_jack_1_d/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_500_1000_1500_2000_p_100_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

# find the iteration with large bias jackknife variance estimation
sub_result[n == 50 & S2_v_jack_1 > 400, ]
# output
# var_main_effect var_inter_effect cov_main_inter_effect var_total_effect EigenPrism_main EigenPrism_CI
# 1:               8                0                     0                0        13.95175      17.54782
# 2:               8                0                     0                0        13.53180      18.48517
# sub_EigenPrism_CI GCTA_rr_main S1_jack.EigenPrism_main S1_v_jack_1 S2_jack.GCTA_rr_main S2_v_jack_1 x_dist
# 1:          17.42421     15.81163                20.51489    65.38263             27.50883    409.0963 normal
# 2:          17.24211     24.36293                22.48393    54.73569            126.44533    762.1583 normal
# structure   p  n main_fixed_var main_random_var inter_fixed_var inter_random_var decor combine d      bs n_sub
# 1:         I 100 50            0.5               0               0                0 FALSE   FALSE 1 leave-1    50
# 2:         I 100 50            0.5               0               0                0 FALSE   FALSE 1 leave-1    50
# rho_e emp_n  i
# 1:   0.5 1e+05 28
# 2:   0.5 1e+05 65

# find the iteration 
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/report_jackknife/")
result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_p_100_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/"
file_list <- list.files(path = result_path, full.names = TRUE)
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[i == 65,]
