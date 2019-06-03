library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/decor/")
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}
save_path <- "~/dev/projects/Chen_environmental_study/reports/proposed_GCTA_paper/est_var_analysis/decor/"
upper <- 0.9
lower <- 0.1
#############################################################################################################################
## Eg + GCTA main
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_264_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# replace the 999 as NA 
sub_result[sub_EigenPrism_main == 999, c("sub_EigenPrism_main", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]

# summary tables
## true
summary_list <- list()
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
summary_list <- append(summary_list, list(true_value = mean_table))

summary_list <- append(summary_list, list(method = "============EgenPrism==========="))

## full data
mean_table <- sub_result[, .(mean_main_full = mean(EigenPrism_main))]
var_table <- sub_result[, .(var_main_full = var(EigenPrism_main))]
summary_list <- append(summary_list, list(full_data = cbind(mean_table,var_table)))


## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                      sub_main_var = var(sub_EigenPrism_main, na.rm = TRUE),
                      sub_NA = sum(is.na(sub_EigenPrism_main))), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var))]
sub_NA <- sub[,.(sub_NA = mean(sub_NA))]
summary_list <- append(summary_list, list(sub_data = cbind(mean_table, var_table, sub_var_table, sub_NA)))

# add eg CI and covarage rate
Eg_CI_result <- sub_result[,.(EigenPrism_CI_length = (EigenPrism_CI2 - EigenPrism_CI1), 
                              EigenPrism_CI_coverage = (var_main_effect >= EigenPrism_CI1) & (var_main_effect <= EigenPrism_CI2))]
EigenPrism_CI_length_mean <- Eg_CI_result[,mean(EigenPrism_CI_length)]
EigenPrism_CI_coverage_rate <- Eg_CI_result[,mean(EigenPrism_CI_coverage)]
summary_list <- append(summary_list, list(Eigen_CI = cbind(EigenPrism_CI_length_mean, EigenPrism_CI_coverage_rate)))

# add CI and covarage rate
sub_CI_result <- sub_result[,.(var_main_effect =mean(var_main_effect, na.rm = TRUE), sub_CI1 = quantile(sub_EigenPrism_main, lower, na.rm = TRUE), sub_CI2 = quantile(sub_EigenPrism_main, upper,na.rm = TRUE)), by = i]
sub_CI_result[, sub_CI_coverage := (var_main_effect >= sub_CI1) & (var_main_effect <= sub_CI2)]
sub_coverage_rate <- sub_CI_result[,mean(sub_CI_coverage)]
sub_CI_length_mean <- sub_CI_result[, sub_CI2-sub_CI1] %>% mean(.)
full_CI_length <- sub_result[, (quantile(EigenPrism_main, upper, na.rm = TRUE)-quantile(EigenPrism_main, lower, na.rm = TRUE))]
summary_list <- append(summary_list, list(sub_CI = cbind(sub_CI_length_mean, sub_coverage_rate,full_CI_length)))
summary_list <- append(summary_list, list(method = "============GCTA==========="))
## GCTA
## full data
mean_table <- sub_result[, .(mean_main_full = mean(prop_main), 
                             mean_inter_full = mean(prop_inter))]
var_table <- sub_result[, .(var_main_full = var(prop_main), 
                            var_inter_full = var(prop_inter))]
summary_list <- append(summary_list, list(full_data = cbind(mean_table,var_table)))

## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_prop_main, na.rm = T),
                      sub_inter_mean = mean(sub_prop_inter, na.rm = T),
                      sub_main_var = var(sub_prop_main, na.rm = T),
                      sub_inter_var = var(sub_prop_inter, na.rm = T),
                      sub_NA = sum(is.na(sub_prop_main))), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var),
                        var_inter_sub = mean(sub_inter_var))]
sub_NA <- sub[,.(sub_NA = mean(sub_NA))]

summary_list <- append(summary_list, list(sub_data = cbind(mean_table, var_table, sub_var_table, sub_NA)))



# add CI 
sub_CI_result <- sub_result[,.(var_main_effect = mean(var_main_effect), sub_CI1 = quantile(sub_prop_main, lower, na.rm = TRUE), sub_CI2 = quantile(sub_prop_main, upper, na.rm = TRUE)), by = i]
sub_CI_result[,sub_CI_length := sub_CI2 - sub_CI1]
sub_CI_result[, coverage := (var_main_effect >= sub_CI1) & (var_main_effect <= sub_CI2)]
sub_CI_rate <- sub_CI_result[,mean(coverage)]
sub_CI_length_mean <- sub_CI_result[, mean(sub_CI_length)]
full_CI_length <- sub_result[, (quantile(prop_main, upper)-quantile(prop_main, lower))]
summary_list <- append(summary_list, list(sub_CI = cbind(sub_CI_length_mean, sub_CI_rate,full_CI_length)))


# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()




#############################################################################################################################
## Eg + GCTA total
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0.1_n_528_p_33_dim_red_coeff_0.9_subpro_0.5_iter_100_nsub_200_EigenPrism_kernel_GCTA_kernel_est_total/"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# replace the 999 as NA 
sub_result[sub_EigenPrism_total == 999, c("sub_EigenPrism_total", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]

# summary tables
## true
summary_list <- list()
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
summary_list <- append(summary_list, list(true_value = mean_table))

summary_list <- append(summary_list, list(method = "============EgenPrism==========="))

## full data
mean_table <- sub_result[, .(mean_total_full = mean(EigenPrism_total))]
var_table <- sub_result[, .(var_total_full = var(EigenPrism_total))]
summary_list <- append(summary_list, list(full_data = cbind(mean_table,var_table)))


## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_EigenPrism_total, na.rm = TRUE),
                      sub_total_var = var(sub_EigenPrism_total, na.rm = TRUE),
                      sub_NA = sum(is.na(sub_EigenPrism_total))), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var))]
sub_NA <- sub[,.(sub_NA = mean(sub_NA))]
summary_list <- append(summary_list, list(sub_data = cbind(mean_table, var_table, sub_var_table, sub_NA)))

# add eg CI and covarage rate
Eg_CI_result <- sub_result[,.(EigenPrism_CI_length = (EigenPrism_CI2 - EigenPrism_CI1), 
                              EigenPrism_CI_coverage = (var_total_effect >= EigenPrism_CI1) & (var_total_effect <= EigenPrism_CI2))]
EigenPrism_CI_length_mean <- Eg_CI_result[,mean(EigenPrism_CI_length)]
EigenPrism_CI_coverage_rate <- Eg_CI_result[,mean(EigenPrism_CI_coverage)]
summary_list <- append(summary_list, list(Eigen_CI = cbind(EigenPrism_CI_length_mean, EigenPrism_CI_coverage_rate)))

# add CI and covarage rate
sub_CI_result <- sub_result[,.(var_total_effect =mean(var_total_effect, na.rm = TRUE), sub_CI1 = quantile(sub_EigenPrism_total, lower, na.rm = TRUE), sub_CI2 = quantile(sub_EigenPrism_total, upper,na.rm = TRUE)), by = i]
sub_CI_result[, sub_CI_coverage := (var_total_effect >= sub_CI1) & (var_total_effect <= sub_CI2)]
sub_coverage_rate <- sub_CI_result[,mean(sub_CI_coverage)]
sub_CI_length_mean <- sub_CI_result[, sub_CI2-sub_CI1] %>% mean(.)
full_CI_length <- sub_result[, (quantile(EigenPrism_total, upper, na.rm = TRUE)-quantile(EigenPrism_total, lower, na.rm = TRUE))]
summary_list <- append(summary_list, list(sub_CI = cbind(sub_CI_length_mean, sub_coverage_rate,full_CI_length)))
summary_list <- append(summary_list, list(method = "============GCTA==========="))
## GCTA
## full data
mean_table <- sub_result[, .(mean_total_full = mean(prop_total), 
                             mean_inter_full = mean(prop_inter))]
var_table <- sub_result[, .(var_total_full = var(prop_total), 
                            var_inter_full = var(prop_inter))]
summary_list <- append(summary_list, list(full_data = cbind(mean_table,var_table)))

## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_prop_total, na.rm = T),
                      sub_inter_mean = mean(sub_prop_inter, na.rm = T),
                      sub_total_var = var(sub_prop_total, na.rm = T),
                      sub_inter_var = var(sub_prop_inter, na.rm = T),
                      sub_NA = sum(is.na(sub_prop_total))), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var),
                        var_inter_sub = mean(sub_inter_var))]
sub_NA <- sub[,.(sub_NA = mean(sub_NA))]

summary_list <- append(summary_list, list(sub_data = cbind(mean_table, var_table, sub_var_table, sub_NA)))



# add CI 
sub_CI_result <- sub_result[,.(var_total_effect = mean(var_total_effect), sub_CI1 = quantile(sub_prop_total, lower, na.rm = TRUE), sub_CI2 = quantile(sub_prop_total, upper, na.rm = TRUE)), by = i]
sub_CI_result[,sub_CI_length := sub_CI2 - sub_CI1]
sub_CI_result[, coverage := (var_total_effect >= sub_CI1) & (var_total_effect <= sub_CI2)]
sub_CI_rate <- sub_CI_result[,mean(coverage)]
sub_CI_length_mean <- sub_CI_result[, mean(sub_CI_length)]
full_CI_length <- sub_result[, (quantile(prop_total, upper)-quantile(prop_total, lower))]
summary_list <- append(summary_list, list(sub_CI = cbind(sub_CI_length_mean, sub_CI_rate,full_CI_length)))


# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()


#############################################################################################################################
## sigma_empirical
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1500_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_signal_kernel_est_main"
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