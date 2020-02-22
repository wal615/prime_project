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

upper <- 0.9
lower <- 0.1
#############################################################################################################################
## Eg + GCTA main
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/non_decore/low_covariance/")
result_path <- "decor_method_None_result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_100_150_231_p_231_rho_e_0.5_dim_red_coeff__decor_FALSE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main_year_1999/"
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


# replace the 999 as NA 
sub_result[sub_EigenPrism_main == 999, c("sub_EigenPrism_main", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]

  
# EigenPrism
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    NA_i = mean(is.na(EigenPrism_main)),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T),
                                                                                            NA_total = sum(NA_i)), by = n]


# sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
#                                           var_jack = jack_var(sub_EigenPrism_main, pro = pro),
#                                           sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
#                                           sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                       by = .(n,rho_e, p,i,pro)]
# sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                    NA_i = mean(is.na(GCTA_main)),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T),
                                                                                            NA_total = sum(NA_i)), by = n]

# sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
#                                             var_jack = jack_var(sub_GCTA_main, pro = pro),
#                                             sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
#                                             sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                         by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

# sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)
summary_final[,target := "main"]
# summary_final[, var_diff_ratio := (var_jack - est_var)/est_var]
summary_final <- cbind(summary_final, additional)
summary_final_1 <- summary_final 

###########################################################
# normal
###########################################################

result_path <- "decor_method_None_result_list_fixed_sub_normal_structure_un_main_0.5_inter_0_n_100_150_231_p_231_rho_e_0.5_dim_red_coeff__decor_FALSE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main_year_1999"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# replace the 999 as NA 
sub_result[sub_EigenPrism_main == 999, c("sub_EigenPrism_main", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

# EigenPrism
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    NA_i = mean(is.na(EigenPrism_main)),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T),
                                                                                            NA_total = sum(NA_i)), by = n]


# sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
#                                           var_jack = jack_var(sub_EigenPrism_main, pro = pro),
#                                           sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
#                                           sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                       by = .(n,rho_e, p,i,pro)]
# sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_total = sum(NA_i)), by = n]

# sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
#                                             var_jack = jack_var(sub_GCTA_main, pro = pro),
#                                             sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
#                                             sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                         by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

# sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)
summary_final[,target := "main"]
# summary_final[, var_diff_ratio := (var_jack - est_var)/est_var]
summary_final <- cbind(summary_final, additional)
summary_final_2 <- summary_final 
summary_final_c_d <- rbind(summary_final_1, summary_final_2)




#############################################################################################################################
## Eg + GCTA main
#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/non_decore/low_covariance/original_cov/")
result_path <- "decor_method_None_result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_100_150_231_p_231_rho_e_0.5_dim_red_coeff__decor_FALSE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main_year_1999/"
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


# replace the 999 as NA 
sub_result[sub_EigenPrism_main == 999, c("sub_EigenPrism_main", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    NA_i = mean(is.na(EigenPrism_main)),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T),
                                                                                            NA_total = sum(NA_i)), by = n]


# sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
#                                           var_jack = jack_var(sub_EigenPrism_main, pro = pro),
#                                           sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
#                                           sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                       by = .(n,rho_e, p,i,pro)]
# sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_total = sum(NA_i)), by = n]

# sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
#                                             var_jack = jack_var(sub_GCTA_main, pro = pro),
#                                             sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
#                                             sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                         by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

# sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)
summary_final[,target := "main"]
# summary_final[, var_diff_ratio := (var_jack - est_var)/est_var]
summary_final <- cbind(summary_final, additional)
summary_final_1 <- summary_final 

###########################################################
# normal
###########################################################

result_path <- "decor_method_None_result_list_fixed_sub_normal_structure_un_main_0.5_inter_0_n_100_150_231_p_231_rho_e_0.5_dim_red_coeff__decor_FALSE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main_year_1999"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# replace the 999 as NA 
sub_result[sub_EigenPrism_main == 999, c("sub_EigenPrism_main", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

# EigenPrism
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    NA_i = mean(is.na(EigenPrism_main)),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T),
                                                                                            NA_total = sum(NA_i)), by = n]


# sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
#                                           var_jack = jack_var(sub_EigenPrism_main, pro = pro),
#                                           sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
#                                           sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                       by = .(n,rho_e, p,i,pro)]
# sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_total = sum(NA_i)), by = n]

# sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
#                                             var_jack = jack_var(sub_GCTA_main, pro = pro),
#                                             sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
#                                             sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
#                                         by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

# sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)
summary_final[,target := "main"]
# summary_final[, var_diff_ratio := (var_jack - est_var)/est_var]
summary_final <- cbind(summary_final, additional)
summary_final_2 <- summary_final 
summary_final_c_o <- rbind(summary_final_1, summary_final_2)