library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/")


#############################################################################################################################
## I main
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_1500_p_33_dim_red_coeff__subpro_0.5_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interact == 0, "main","main+inter"))]

# tables
prop_main <- sub_result[, .(prop_main = mean(prop_main)), by = i]
prop_main$i <- 0
sub_prop_main <- sub_result[, .(prop_main = sub_prop_main,i)]
hist_table <- rbindlist(l = list(prop_main, sub_prop_main), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(prop_main, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]

#GCTA
GCTA_main <- sub_result[, .(GCTA_main = mean(GCTA_main)), by = i]
GCTA_main$i <- 0
sub_GCTA_main <- sub_result[, .(GCTA_main = sub_GCTA_main,i)]
hist_table <- rbindlist(l = list(GCTA_main, sub_GCTA_main), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(GCTA_main, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]

# save the result 
write.csv(sub_result, file = paste0("./", result_path, ".csv"), row.names = FALSE)


#############################################################################################################################
## un main
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1500_p_33_dim_red_coeff__pro_0.5_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

# tables
prop_main <- sub_result[, .(prop_main = mean(prop_main)), by = i]
prop_main$i <- 0
sub_prop_main <- sub_result[, .(prop_main = sub_prop_main,i)]
hist_table <- rbindlist(l = list(prop_main, sub_prop_main), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(prop_main, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]
# save the result 
write.csv(sub_result, file = paste0("./", result_path, ".csv"), row.names = FALSE)

#############################################################################################################################
## I total
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0.1_n_1500_p_33_dim_red_coeff__pro_0.5_total"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

# tables
prop_total <- sub_result[, .(prop_total = mean(prop_total)), by = i]
prop_total$i <- 0
sub_prop_total <- sub_result[, .(prop_total = sub_prop_total,i)]
hist_table <- rbindlist(l = list(prop_total, sub_prop_total), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(prop_total, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]
# save the result 
write.csv(sub_result, file = paste0("./", result_path, ".csv"), row.names = FALSE)



#############################################################################################################################
## un main dim reduction
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1500_p_33_dim_red_coeff_0.8_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

# tables
prop_main <- sub_result[, .(prop_main = mean(prop_main)), by = i]
prop_main$i <- 0
sub_prop_main <- sub_result[, .(prop_main = sub_prop_main,i)]
hist_table <- rbindlist(l = list(prop_main, sub_prop_main), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(prop_main, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]
# save the result 
write.csv(sub_result, file = paste0("./", result_path, ".csv"), row.names = FALSE)



#############################################################################################################################
## un main dim reduction 75%
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1500_p_20_dim_red_coeff__pro_0.75_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

# tables
prop_main <- sub_result[, .(prop_main = mean(prop_main)), by = i]
prop_main$i <- 0
sub_prop_main <- sub_result[, .(prop_main = sub_prop_main,i)]
hist_table <- rbindlist(l = list(prop_main, sub_prop_main), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(prop_main, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]
# save the result 
write.csv(sub_result, file = paste0("./", result_path, ".csv"), row.names = FALSE)

#############################################################################################################################
## un main dim reduction by rm PCB directly
#############################################################################################################################
result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1500_p_20_dim_red_coeff__main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

# tables
prop_main <- sub_result[, .(prop_main = mean(prop_main)), by = i]
prop_main$i <- 0
sub_prop_main <- sub_result[, .(prop_main = sub_prop_main,i)]
hist_table <- rbindlist(l = list(prop_main, sub_prop_main), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,.(est_var = var(prop_main, na.rm = TRUE)), by = i]
var_table[i !=0, est_var] %>% mean(.)
var_table[i ==0, est_var]
# save the result 
write.csv(sub_result, file = paste0("./", result_path, ".csv"), row.names = FALSE)