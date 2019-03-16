library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")
main_col_name <- c("true_main", "GCTA_main", "prop_main")
inter_col_name <- c("true_interaction", "GCTA_interaction", "prop_interaction")


##############################
## test the reproduceable 
##############################
file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper", include.dirs = FALSE, full.names = TRUE, recursive = FALSE)[-1] # remove the subfolder 
file_PCB <- file_list_0[grep(x = file_list_0, pattern = "PCB")]
file_PCB_no_total <- file_PCB[-grep(x = file_PCB, pattern = "total")]
file_PCB_total <- file_PCB[grep(x = file_PCB, pattern = "total")]

# no pcb no total
result_PCB_no_total <- lapply(file_PCB_no_total, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_PCB_no_total <- result_PCB_no_total[true_main !=0,]
result_PCB_no_total[,.(ave_true_main = mean(true_main), ave_true_inter = mean(true_interaction)), by= .(x_dist, inter_fixed_var, pro, interaction_m)]
result_PCB_no_total[,.N, by= .(x_dist, inter_fixed_var,pro, interaction_m)]

# no pcb total
result_PCB_total <- lapply(file_PCB_total, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)
result_PCB_total <- result_PCB_total[true_total !=0,]
result_PCB_total[,.(ave_true_total = mean(true_total), ave_true_inter = mean(true_interaction)), by= .(x_dist, inter_fixed_var,pro, interaction_m)]
result_PCB_total[,.N, by= .(x_dist, inter_fixed_var,pro, interaction_m)]



file_no_PCB <- file_list_0[-grep(x = file_list_0, pattern = "PCB")]
file_no_PCB_no_total <- file_no_PCB[-grep(x = file_no_PCB, pattern = "total")]
file_no_PCB_total <- file_no_PCB[grep(x = file_no_PCB, pattern = "total")]

# no pcb no total
result_no_PCB_no_total <- lapply(file_no_PCB_no_total, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_no_PCB_no_total <- result_no_PCB_no_total[true_main !=0,]
result_no_PCB_no_total[,.(ave_true_main = mean(true_main), ave_true_inter = mean(true_interaction)), by= .(x_dist,structure, inter_fixed_var,interaction_m)]
result_no_PCB_no_total[,.N, by= .(x_dist,structure, inter_fixed_var,interaction_m)]

# no pcb total
result_no_PCB_total <- lapply(file_no_PCB_total, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_no_PCB_total <- result_no_PCB_total[true_total !=0,]
result_no_PCB_total[,.(ave_true_total = mean(true_total), ave_true_inter = mean(true_interaction)), by= .(x_dist,structure, inter_fixed_var,interaction_m)]
result_no_PCB_total[,.N, by= .(x_dist,structure, inter_fixed_var,interaction_m)]

