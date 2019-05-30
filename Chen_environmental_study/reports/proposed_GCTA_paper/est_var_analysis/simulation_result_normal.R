library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/")
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}
save_path <- "~/dev/projects/Chen_environmental_study/reports/proposed_GCTA_paper/est_var_analysis/"
#############################################################################################################################
## GCTA main
#############################################################################################################################
result_path <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_1500_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interact == 0, "main","main+inter"))]
summary_list <- list()
# summary tables
## true
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
var_table <- sub_result[, .(var_main_true = var(var_main_effect), 
                            var_inter_true = var(var_inter_effect), 
                            var_cov_true = var(cov_main_inter_effect),
                            var_total_true = var(var_total_effect))]
summary_list <- append(summary_list, list(mean_table, var_table))
## full data
mean_table <- sub_result[, .(mean_main_full = mean(prop_main), 
                             mean_inter_full = mean(prop_inter))]
var_table <- sub_result[, .(var_main_full = var(prop_main), 
                            var_inter_full = var(prop_main))]
summary_list <- append(summary_list, list(mean_table, var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_prop_main),
                      sub_inter_mean = mean(sub_prop_inter),
                      sub_main_var = var(sub_prop_main),
                      sub_inter_var = var(sub_prop_inter)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var),
                        var_inter_sub = mean(sub_inter_var))] 
summary_list <- append(summary_list, list(mean_table, var_table, sub_var_table))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()


#############################################################################################################################
## GCTA total
#############################################################################################################################
result_path <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_1500_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interact == 0, "total",""))]
summary_list <- list()
# summary tables
## true
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
var_table <- sub_result[, .(var_main_true = var(var_main_effect), 
                            var_inter_true = var(var_inter_effect), 
                            var_cov_true = var(cov_main_inter_effect),
                            var_total_true = var(var_total_effect))]
summary_list <- append(summary_list, list(mean_table, var_table))
## full data
mean_table <- sub_result[, .(mean_total_full = mean(prop_total), 
                             mean_inter_full = mean(prop_inter))]
var_table <- sub_result[, .(var_total_full = var(prop_total), 
                            var_inter_full = var(prop_total))]
summary_list <- append(summary_list, list(mean_table, var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_prop_total, na.rm = TRUE),
                      sub_inter_mean = mean(sub_prop_inter, na.rm = TRUE),
                      sub_total_var = var(sub_prop_total, na.rm = TRUE),
                      sub_inter_var = var(sub_prop_inter, na.rm = TRUE)), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var),
                        var_inter_sub = mean(sub_inter_var))] 
summary_list <- append(summary_list, list(mean_table, var_table, sub_var_table))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()



#############################################################################################################################
## Eg main
#############################################################################################################################
result_path <- "./"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
summary_list <- list()
# summary tables
## true
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
var_table <- sub_result[, .(var_main_true = var(var_main_effect), 
                            var_inter_true = var(var_inter_effect), 
                            var_cov_true = var(cov_main_inter_effect),
                            var_total_true = var(var_total_effect))]
summary_list <- append(summary_list, list(mean_table, var_table))
## full data
mean_table <- sub_result[, .(mean_main_full = mean(EigenPrism_main))]
var_table <- sub_result[, .(var_main_full = var(EigenPrism_main))]
CI_table <- sub_result[,.(mean_CI1_full = mean(EigenPrism_CI1),
                          mean_CI2_full = mean(EigenPrism_CI2))]
summary_list <- append(summary_list, list(mean_table, var_table, CI_table))

## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                      sub_main_var = var(sub_EigenPrism_main, na.rm = TRUE),
                      sub_CI1_mean = mean(sub_EigenPrism_CI1, na.rm = TRUE),
                      sub_CI2_mean = mean(sub_EigenPrism_CI2, na.rm = TRUE)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var))] 
sub_CI_table <- sub[,.(mean_CI1_sub = mean(sub_CI1_mean),
                       mean_CI2_sub = mean(sub_CI2_mean))]
summary_list <- append(summary_list, list(mean_table, var_table, sub_var_table, sub_CI_table))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()

#############################################################################################################################
## sigma_empirical
#############################################################################################################################
result_path <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_1500_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_signal_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interact == 0, "main","main+inter"))]
summary_list <- list()
# summary tables
## true
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
var_table <- sub_result[, .(var_main_true = var(var_main_effect), 
                            var_inter_true = var(var_inter_effect), 
                            var_cov_true = var(cov_main_inter_effect),
                            var_total_true = var(var_total_effect))]
summary_list <- append(summary_list, list(mean_table, var_table))
## full data
mean_table <- sub_result[, .(mean_main_full = mean(main), 
                             mean_inter_full = mean(inter),
                             mean_total_full = mean(total))]
var_table <- sub_result[, .(var_main_full = var(main), 
                            var_inter_full = var(inter),
                            var_total_full = var(total))]
summary_list <- append(summary_list, list(mean_table, var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_main),
                      sub_inter_mean = mean(sub_inter),
                      sub_total_mean = mean(sub_total),
                      sub_main_var = var(sub_main),
                      sub_inter_var = var(sub_inter),
                      sub_total_var = var(sub_total)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean),
                      mean_inter_submean = mean(sub_inter_mean),
                      mean_total_submean = mean(sub_total_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean),
                     var_inter_submean = var(sub_inter_mean),
                     var_total_submean = var(sub_total_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var),
                        var_inter_sub = mean(sub_inter_var),
                        var_total_sub = mean(sub_total_var))] 
summary_list <- append(summary_list, list(mean_table, var_table, sub_var_table))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()

