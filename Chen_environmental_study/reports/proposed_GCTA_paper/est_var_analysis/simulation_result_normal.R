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
## I main
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
## un main
#############################################################################################################################
result_path <- ""
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
