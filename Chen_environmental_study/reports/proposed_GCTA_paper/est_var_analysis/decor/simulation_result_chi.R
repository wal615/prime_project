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
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_528_p_33_dim_red_coeff_0.9_subpro_0.5_iter_100_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
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
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))

## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                      sub_main_var = var(sub_EigenPrism_main, na.rm = TRUE)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table,
                                          sub_data_mean_var = sub_var_table))

# add eg CI and covarage rate
Eg_CI_result <- sub_result[,.(EigenPrism_CI_full_length = (EigenPrism_CI2 - EigenPrism_CI1), 
                              EigenPrism_CI_coverage = (var_main_effect >= EigenPrism_CI1) & (var_main_effect <= EigenPrism_CI2))]
EigenPrism_CI_full_length_mean <- Eg_CI_result[,mean(EigenPrism_CI_full_length)]
EigenPrism_CI_full_coverage_rate <- Eg_CI_result[,mean(EigenPrism_CI_coverage)]
summary_list <- append(summary_list, list(EigenPrism_CI_full_coverage_rate = EigenPrism_CI_full_coverage_rate, 
                                          EigenPrism_CI_full_length_mean = EigenPrism_CI_full_length_mean))

# add CI and covarage rate
sub_CI_result <- sub_result[,.(var_main_effect =mean(var_main_effect, na.rm = TRUE), sub_CI1 = quantile(sub_EigenPrism_main, lower, na.rm = TRUE), sub_CI2 = quantile(sub_EigenPrism_main, upper,na.rm = TRUE)), by = i]
sub_CI_result[, sub_CI_coverage := (var_main_effect >= sub_CI1) & (var_main_effect <= sub_CI2)]
sub_coverage_rate <- sub_CI_result[,mean(sub_CI_coverage)]
sub_CI_length_mean <- sub_CI_result[, sub_CI2-sub_CI1] %>% mean(.)
summary_list <- append(summary_list, list(sub_coverage_rate=sub_coverage_rate,
                                          sub_CI_length_mean = sub_CI_length_mean))
summary_list <- append(summary_list, list(method = "============GCTA==========="))
## GCTA
## full data
mean_table <- sub_result[, .(mean_main_full = mean(prop_main), 
                             mean_inter_full = mean(GCTA_inter))]
var_table <- sub_result[, .(var_main_full = var(prop_main), 
                            var_inter_full = var(GCTA_inter))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_prop_main, na.rm = T),
                      sub_inter_mean = mean(sub_GCTA_inter, na.rm = T),
                      sub_main_var = var(sub_prop_main, na.rm = T),
                      sub_inter_var = var(sub_GCTA_inter, na.rm = T)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var),
                        var_inter_sub = mean(sub_inter_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table, 
                                          sub_data_mean_var = sub_var_table))


# add CI 
sub_CI_result <- sub_result[,.(var_main_effect = mean(var_main_effect), sub_CI1 = quantile(sub_prop_main, lower, na.rm = TRUE), sub_CI2 = quantile(sub_prop_main, upper, na.rm = TRUE)), by = i]
sub_CI_result[,sub_CI_length := sub_CI2 - sub_CI1]
sub_CI_result[, coverage := (var_main_effect >= sub_CI1) & (var_main_effect <= sub_CI2)]
sub_CI_coverage_rate <- sub_CI_result[,mean(coverage)]
sub_CI_length_mean <- sub_CI_result[, mean(sub_CI_length)]
full_CI_length <- sub_result[, (quantile(prop_main, upper)-quantile(prop_main, lower))]
summary_list <- append(summary_list, list(sub_CI_length_mean=sub_CI_length_mean, 
                                          sub_CI_coverage_rate = sub_CI_coverage_rate, 
                                          full_CI_length = full_CI_length))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()


#############################################################################################################################
## least + GCTA main
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_528_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_least_square_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# summary tables

## true
summary_list <- list()
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
summary_list <- append(summary_list, list(true_value = mean_table))

summary_list <- append(summary_list, list(method = "============Least_square==========="))
## full data
mean_table <- sub_result[, .(mean_main_full = mean(least_square_main))]
var_table <- sub_result[, .(var_main_full = var(least_square_main))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))

## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_least_square_main, na.rm = TRUE),
                      sub_main_var = var(sub_least_square_main, na.rm = TRUE)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table,
                                          sub_data_mean_var = sub_var_table))

# add CI and covarage rate
sub_CI_result <- sub_result[,.(var_main_effect =mean(var_main_effect, na.rm = TRUE), sub_CI1 = quantile(sub_least_square_main, lower, na.rm = TRUE), sub_CI2 = quantile(sub_least_square_main, upper,na.rm = TRUE)), by = i]
sub_CI_result[, sub_CI_coverage := (var_main_effect >= sub_CI1) & (var_main_effect <= sub_CI2)]
sub_coverage_rate <- sub_CI_result[,mean(sub_CI_coverage)]
sub_CI_length_mean <- sub_CI_result[, sub_CI2-sub_CI1] %>% mean(.)
summary_list <- append(summary_list, list(sub_coverage_rate=sub_coverage_rate,
                                          sub_CI_length_mean = sub_CI_length_mean))
summary_list <- append(summary_list, list(method = "============GCTA==========="))

## GCTA
## full data
mean_table <- sub_result[, .(mean_main_full = mean(prop_main), 
                             mean_inter_full = mean(GCTA_inter))]
var_table <- sub_result[, .(var_main_full = var(prop_main), 
                            var_inter_full = var(GCTA_inter))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_main_mean = mean(sub_prop_main, na.rm = T),
                      sub_inter_mean = mean(sub_GCTA_inter, na.rm = T),
                      sub_main_var = var(sub_prop_main, na.rm = T),
                      sub_inter_var = var(sub_GCTA_inter, na.rm = T)), by = i]
mean_table <- sub[, .(mean_main_submean = mean(sub_main_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_main_submean = var(sub_main_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_main_sub = mean(sub_main_var),
                        var_inter_sub = mean(sub_inter_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table, 
                                          sub_data_mean_var = sub_var_table))


# add CI 
sub_CI_result <- sub_result[,.(var_main_effect = mean(var_main_effect), sub_CI1 = quantile(sub_prop_main, lower, na.rm = TRUE), sub_CI2 = quantile(sub_prop_main, upper, na.rm = TRUE)), by = i]
sub_CI_result[,sub_CI_length := sub_CI2 - sub_CI1]
sub_CI_result[, coverage := (var_main_effect >= sub_CI1) & (var_main_effect <= sub_CI2)]
sub_CI_coverage_rate <- sub_CI_result[,mean(coverage)]
sub_CI_length_mean <- sub_CI_result[, mean(sub_CI_length)]
full_CI_length <- sub_result[, (quantile(prop_main, upper)-quantile(prop_main, lower))]
summary_list <- append(summary_list, list(sub_CI_length_mean=sub_CI_length_mean, 
                                          sub_CI_coverage_rate = sub_CI_coverage_rate, 
                                          full_CI_length = full_CI_length))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()


#############################################################################################################################
## Eg + GCTA total
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0.1_n_1000_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_EigenPrism_kernel_GCTA_kernel_est_total"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# summary tables

## true
summary_list <- list()
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
summary_list <- append(summary_list, list(true_value = mean_table))

## full data
summary_list <- append(summary_list, list(method = "============EgenPrism==========="))
mean_table <- sub_result[, .(mean_total_full = mean(EigenPrism_total))]
var_table <- sub_result[, .(var_total_full = var(EigenPrism_total))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))

## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_EigenPrism_total, na.rm = TRUE),
                      sub_total_var = var(sub_EigenPrism_total, na.rm = TRUE)), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table,
                                          sub_data_mean_var = sub_var_table))

# add eg CI and covarage rate
Eg_CI_result <- sub_result[,.(EigenPrism_CI_full_length = (EigenPrism_CI2 - EigenPrism_CI1), 
                              EigenPrism_CI_coverage = (var_total_effect >= EigenPrism_CI1) & (var_total_effect <= EigenPrism_CI2))]
EigenPrism_CI_full_length_mean <- Eg_CI_result[,mean(EigenPrism_CI_full_length)]
EigenPrism_CI_full_coverage_rate <- Eg_CI_result[,mean(EigenPrism_CI_coverage)]
summary_list <- append(summary_list, list(EigenPrism_CI_full_coverage_rate = EigenPrism_CI_full_coverage_rate, 
                                          EigenPrism_CI_full_length_mean = EigenPrism_CI_full_length_mean))

# add CI and covarage rate
sub_CI_result <- sub_result[,.(var_total_effect =mean(var_total_effect, na.rm = TRUE), sub_CI1 = quantile(sub_EigenPrism_total, lower, na.rm = TRUE), sub_CI2 = quantile(sub_EigenPrism_total, upper,na.rm = TRUE)), by = i]
sub_CI_result[, sub_CI_coverage := (var_total_effect >= sub_CI1) & (var_total_effect <= sub_CI2)]
sub_coverage_rate <- sub_CI_result[,mean(sub_CI_coverage)]
sub_CI_length_mean <- sub_CI_result[, sub_CI2-sub_CI1] %>% mean(.)
summary_list <- append(summary_list, list(sub_coverage_rate=sub_coverage_rate,
                                          sub_CI_length_mean = sub_CI_length_mean))

## GCTA
## full data
summary_list <- append(summary_list, list(method = "============GCTA==========="))
mean_table <- sub_result[, .(mean_total_full = mean(prop_total), 
                             mean_inter_full = mean(GCTA_inter))]
var_table <- sub_result[, .(var_total_full = var(prop_total), 
                            var_inter_full = var(GCTA_inter))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_prop_total, na.rm = T),
                      sub_inter_mean = mean(sub_GCTA_inter, na.rm = T),
                      sub_total_var = var(sub_prop_total, na.rm = T),
                      sub_inter_var = var(sub_GCTA_inter, na.rm = T)), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var),
                        var_inter_sub = mean(sub_inter_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table, 
                                          sub_data_mean_var = sub_var_table))


# add CI 
sub_CI_result <- sub_result[,.(var_total_effect = mean(var_total_effect), sub_CI1 = quantile(sub_prop_total, lower, na.rm = TRUE), sub_CI2 = quantile(sub_prop_total, upper, na.rm = TRUE)), by = i]
sub_CI_result[,sub_CI_length := sub_CI2 - sub_CI1]
sub_CI_result[, coverage := (var_total_effect >= sub_CI1) & (var_total_effect <= sub_CI2)]
sub_CI_coverage_rate <- sub_CI_result[,mean(coverage)]
sub_CI_length_mean <- sub_CI_result[, mean(sub_CI_length)]
full_CI_length <- sub_result[, (quantile(prop_total, upper,na.rm = TRUE)-quantile(prop_total, lower,na.rm = TRUE))]
summary_list <- append(summary_list, list(sub_CI_length_mean=sub_CI_length_mean, 
                                          sub_CI_coverage_rate = sub_CI_coverage_rate, 
                                          full_CI_length = full_CI_length))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()

#############################################################################################################################
## least + GCTA total
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0.1_n_2000_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_least_square_kernel_GCTA_kernel_est_total"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# summary tables

## true
summary_list <- list()
summary_list <- append(summary_list, list(result_path))
mean_table <- sub_result[, .(mean_main_true = mean(var_main_effect), 
                             mean_inter_true = mean(var_inter_effect), 
                             mean_cov_true = mean(cov_main_inter_effect),
                             mean_total_true = mean(var_total_effect))]
summary_list <- append(summary_list, list(true_value = mean_table))

summary_list <- append(summary_list, list(method = "============Least_square==========="))
## full data
mean_table <- sub_result[, .(mean_total_full = mean(least_square_total))]
var_table <- sub_result[, .(var_total_full = var(least_square_total))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))

## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_least_square_total, na.rm = TRUE),
                      sub_total_var = var(sub_least_square_total, na.rm = TRUE)), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table,
                                          sub_data_mean_var = sub_var_table))

# add CI and covarage rate
sub_CI_result <- sub_result[,.(var_total_effect =mean(var_total_effect, na.rm = TRUE), sub_CI1 = quantile(sub_least_square_total, lower, na.rm = TRUE), sub_CI2 = quantile(sub_least_square_total, upper,na.rm = TRUE)), by = i]
sub_CI_result[, sub_CI_coverage := (var_total_effect >= sub_CI1) & (var_total_effect <= sub_CI2)]
sub_coverage_rate <- sub_CI_result[,mean(sub_CI_coverage)]
sub_CI_length_mean <- sub_CI_result[, sub_CI2-sub_CI1] %>% mean(.)
summary_list <- append(summary_list, list(sub_coverage_rate=sub_coverage_rate,
                                          sub_CI_length_mean = sub_CI_length_mean))
summary_list <- append(summary_list, list(method = "============GCTA==========="))

## GCTA
## full data
mean_table <- sub_result[, .(mean_total_full = mean(prop_total), 
                             mean_inter_full = mean(GCTA_inter))]
var_table <- sub_result[, .(var_total_full = var(prop_total), 
                            var_inter_full = var(GCTA_inter))]
summary_list <- append(summary_list, list(full_data_mean = mean_table, 
                                          full_data_var = var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_prop_total, na.rm = T),
                      sub_inter_mean = mean(sub_GCTA_inter, na.rm = T),
                      sub_total_var = var(sub_prop_total, na.rm = T),
                      sub_inter_var = var(sub_GCTA_inter, na.rm = T)), by = i]
mean_table <- sub[, .(mean_total_submean = mean(sub_total_mean),
                      mean_inter_submean = mean(sub_inter_mean))]
var_table <- sub[, .(var_total_submean = var(sub_total_mean),
                     var_inter_submean = var(sub_inter_mean))]
sub_var_table <- sub[,.(var_total_sub = mean(sub_total_var),
                        var_inter_sub = mean(sub_inter_var))] 
summary_list <- append(summary_list, list(sub_data_mean = mean_table, 
                                          sub_data_var = var_table, 
                                          sub_data_mean_var = sub_var_table))


# add CI 
sub_CI_result <- sub_result[,.(var_total_effect = mean(var_total_effect), sub_CI1 = quantile(sub_prop_total, lower, na.rm = TRUE), sub_CI2 = quantile(sub_prop_total, upper, na.rm = TRUE)), by = i]
sub_CI_result[,sub_CI_length := sub_CI2 - sub_CI1]
sub_CI_result[, coverage := (var_total_effect >= sub_CI1) & (var_total_effect <= sub_CI2)]
sub_CI_coverage_rate <- sub_CI_result[,mean(coverage)]
sub_CI_length_mean <- sub_CI_result[, mean(sub_CI_length)]
full_CI_length <- sub_result[, (quantile(prop_total, upper)-quantile(prop_total, lower))]
summary_list <- append(summary_list, list(sub_CI_length_mean=sub_CI_length_mean, 
                                          sub_CI_coverage_rate = sub_CI_coverage_rate, 
                                          full_CI_length = full_CI_length))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()

#############################################################################################################################
## GCTA total
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0.1_n_700_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_EigenPrism_kernel_GCTA_kernel_est_total"
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
                             mean_inter_full = mean(GCTA_inter))]
var_table <- sub_result[, .(var_total_full = var(prop_total), 
                            var_inter_full = var(prop_total))]
summary_list <- append(summary_list, list(mean_table, var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_total_mean = mean(sub_prop_total, na.rm = TRUE),
                      sub_inter_mean = mean(sub_GCTA_inter, na.rm = TRUE),
                      sub_total_var = var(sub_prop_total, na.rm = TRUE),
                      sub_inter_var = var(sub_GCTA_inter, na.rm = TRUE)), by = i]
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

#############################################################################################################################
## single_var_est
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1500_p_2_dim_red_coeff__subpro_0.5_iter_100_nsub_200_single_var_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
summary_list <- list()
# summary tables

## full data
mean_table <- sub_result[, .(mean_var1_full = mean(singal_var1), 
                             mean_var2_full = mean(singal_var2),
                             mean_cov_full = mean(cov))]
var_table <- sub_result[, .(var_var1_full = var(singal_var1), 
                            var_var2_full = var(singal_var2),
                            var_cov_full = var(cov))]
summary_list <- append(summary_list, list(mean_table, var_table))
## sub-sampling_mean
sub <- sub_result[, .(sub_var1_mean = mean(sub_singal_var1),
                      sub_var2_mean = mean(sub_singal_var2),
                      sub_cov_mean = mean(sub_cov),
                      sub_var1_var = var(sub_singal_var1),
                      sub_var2_var = var(sub_singal_var2),
                      sub_cov_var = var(sub_cov)), by = i]
mean_table <- sub[, .(mean_var1_submean = mean(sub_var1_mean),
                      mean_var2_submean = mean(sub_var2_mean),
                      mean_cov_submean = mean(sub_cov_mean))]
var_table <- sub[, .(var_var1_submean = var(sub_var1_mean),
                     var_var2_submean = var(sub_var2_mean),
                     var_cov_submean = var(sub_cov_mean))]
sub_var_table <- sub[,.(var_var1_sub = mean(sub_var1_var),
                        var_var2_sub = mean(sub_var2_var),
                        var_cov_sub = mean(sub_cov_var))] 
summary_list <- append(summary_list, list(mean_table, var_table, sub_var_table))

# save the result
sink(paste0(save_path,result_path,".txt"))
print(summary_list)
sink()
