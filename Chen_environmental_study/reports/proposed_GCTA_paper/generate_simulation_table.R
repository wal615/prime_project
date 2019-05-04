library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")

file_list_all <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)

##################
# PCB
##################
# total
file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*PCB.*total",perl = TRUE)]

PCB_total_combined <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)
PCB_total_combined[,data_path:= NULL]

PCB_total_combined_mean <- PCB_total_combined[true_total != 0, ]

by = c("main_fixed_var",
       "inter_fixed_var",
       "reduce_coef", 
       "x_dist" ,
       "combine", 
       "interaction_m",
       "pro",
       "n",
       "inter_std")
PCB_total_combined_mean_summary <- PCB_total_combined_mean[,  list(true_total = mean(true_total),
                                                                    true_main = mean(var_main_effect),
                                                                    true_inter = mean(var_inter_effect),
                                                                    true_cov_main_inter = mean(cov_main_inter_effect),
                                                                    GCTA_total = mean(GCTA_total),
                                                                    proposed_GCTA_total = mean(prop_total),
                                                                    N_iteration = .N), by = by][,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "total"," "))]
PCB_total_combined_mean_summary[,interaction_m:=NULL] 

# main and inter

file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*PCB.*(main|main_inter)$",perl = TRUE)]

PCB_main_inter_combined <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)
PCB_main_inter_combined[,data_path:= NULL]

PCB_main_inter_combined_mean <- PCB_main_inter_combined[true_main != 0, ]

by = c("main_fixed_var",
       "inter_fixed_var",
       "x_dist" ,
       "combine", 
       "interaction_m",
       "pro",
       "n")

PCB_main_inter_combined_mean_summary <- PCB_main_inter_combined_mean[,  list(true_total = sum(mean(var_main_effect) +  mean(var_inter_effect) + mean(cov_main_inter_effect)),
                                                                             true_main = mean(var_main_effect),
                                                                             true_inter = mean(var_inter_effect),
                                                                             true_cov_main_inter = mean(cov_main_inter_effect),
                                                                             GCTA_main = mean(GCTA_main),
                                                                             GCTA_inter = mean(GCTA_interaction),
                                                                             proposed_GCTA_main = mean(prop_main),
                                                                             proposed_GCTA_inter = mean(prop_interaction),
                                                                             N_iteration = .N), by = by][,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]
PCB_main_inter_combined_mean_summary[,interaction_m:=NULL] 

# generate the summary for PCB simulation 
# subset pro == 0.6
PCB_total_final <- PCB_total_combined_mean_summary[pro == 0.6 & inter_std == TRUE,][,c("main_fixed_var", "inter_fixed_var", "combine","reduce_coef","inter_std") := NULL]
PCB_main_inter_final <- PCB_main_inter_combined_mean_summary[pro == 0.6,][,c("main_fixed_var", "inter_fixed_var", "combine") := NULL]
PCB_combined_final <- rbindlist(l = list(PCB_total_final, PCB_main_inter_final), fill = TRUE)
col_order <- c("x_dist", "n", "data_gen_model", "est_model",
               "true_total","GCTA_total",  "proposed_GCTA_total",
               "true_main","GCTA_main",  "proposed_GCTA_main",
               "true_inter","GCTA_inter",  "proposed_GCTA_inter")
PCB_combined_final <- PCB_combined_final[, ..col_order]


#############################################
# Chi
############################################

file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*chi_un.*total",perl = TRUE)]

chi_total_combined <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)

chi_total_combined_mean <- chi_total_combined[true_total != 0, ]

by = c("main_fixed_var",
       "inter_fixed_var",
       "x_dist" ,
       "combine", 
       "interaction_m",
       "n",
       "inter_std")
chi_total_combined_mean_summary <- chi_total_combined_mean[,  list(true_total = mean(true_total),
                                                                   true_main = mean(var_main_effect),
                                                                   true_inter = mean(var_inter_effect),
                                                                   true_cov_main_inter = mean(cov_main_inter_effect),
                                                                   GCTA_total = mean(GCTA_total),
                                                                   proposed_GCTA_total = mean(prop_total),
                                                                   N_iteration = .N), by = by][,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "total"," "))]
chi_total_combined_mean_summary[,interaction_m:=NULL] 

# main and inter

# total
file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*chi_un.*(main|main_inter)$",perl = TRUE)]

chi_main_inter_combined <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)

chi_main_inter_combined_mean <- chi_main_inter_combined[true_main != 0, ]

by = c("main_fixed_var",
       "inter_fixed_var",
       "x_dist" ,
       "combine", 
       "interaction_m",
       "n")

chi_main_inter_combined_mean_summary <- chi_main_inter_combined_mean[,  list(true_total = sum(mean(var_main_effect) +  mean(var_inter_effect) + mean(cov_main_inter_effect)),
                                                                             true_main = mean(var_main_effect),
                                                                             true_inter = mean(var_inter_effect),
                                                                             true_cov_main_inter = mean(cov_main_inter_effect),
                                                                             GCTA_main = mean(GCTA_main),
                                                                             GCTA_inter = mean(GCTA_interaction),
                                                                             proposed_GCTA_main = mean(prop_main),
                                                                             proposed_GCTA_inter = mean(prop_interaction),
                                                                             N_iteration = .N), by = by][,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]
chi_main_inter_combined_mean_summary[,interaction_m:=NULL] 

# generate the summary for chi simulation 
# subset pro == 0.6
chi_total_final <- chi_total_combined_mean_summary[,c("main_fixed_var", "inter_fixed_var", "combine","inter_std") := NULL]
chi_main_inter_final <- chi_main_inter_combined_mean_summary[,c("main_fixed_var", "inter_fixed_var", "combine") := NULL]
chi_combined_final <- rbindlist(l = list(chi_total_final, chi_main_inter_final), fill = TRUE)
col_order <- c("x_dist", "n", "data_gen_model", "est_model",
               "true_total","GCTA_total",  "proposed_GCTA_total",
               "true_main","GCTA_main",  "proposed_GCTA_main",
               "true_inter","GCTA_inter",  "proposed_GCTA_inter")
chi_combined_final <- chi_combined_final[, ..col_order]

#############################################
# normal
############################################

file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*normal_un.*total",perl = TRUE)]

normal_total_combined <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)

normal_total_combined_mean <- normal_total_combined[true_total != 0, ]

by = c("main_fixed_var",
       "inter_fixed_var",
       "x_dist" ,
       "combine", 
       "interaction_m",
       "n",
       "inter_std")
normal_total_combined_mean_summary <- normal_total_combined_mean[,  list(true_total = mean(true_total),
                                                                   true_main = mean(var_main_effect),
                                                                   true_inter = mean(var_inter_effect),
                                                                   true_cov_main_inter = mean(cov_main_inter_effect),
                                                                   GCTA_total = mean(GCTA_total),
                                                                   proposed_GCTA_total = mean(prop_total),
                                                                   N_iteration = .N), by = by][,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "total"," "))]
normal_total_combined_mean_summary[,interaction_m:=NULL] 

# main and inter

# total
file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*normal_un.*(main|main_inter)$",perl = TRUE)]

normal_main_inter_combined <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)

normal_main_inter_combined_mean <- normal_main_inter_combined[true_main != 0, ]

by = c("main_fixed_var",
       "inter_fixed_var",
       "x_dist" ,
       "combine", 
       "interaction_m",
       "n")

normal_main_inter_combined_mean_summary <- normal_main_inter_combined_mean[,  list(true_total = sum(mean(var_main_effect) +  mean(var_inter_effect) + mean(cov_main_inter_effect)),
                                                                             true_main = mean(var_main_effect),
                                                                             true_inter = mean(var_inter_effect),
                                                                             true_cov_main_inter = mean(cov_main_inter_effect),
                                                                             GCTA_main = mean(GCTA_main),
                                                                             GCTA_inter = mean(GCTA_interaction),
                                                                             proposed_GCTA_main = mean(prop_main),
                                                                             proposed_GCTA_inter = mean(prop_interaction),
                                                                             N_iteration = .N), by = by][,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]
normal_main_inter_combined_mean_summary[,interaction_m:=NULL] 

# generate the summary for normal simulation 
# subset pro == 0.6
normal_total_final <- normal_total_combined_mean_summary[,c("main_fixed_var", "inter_fixed_var", "combine","inter_std") := NULL]
normal_main_inter_final <- normal_main_inter_combined_mean_summary[,c("main_fixed_var", "inter_fixed_var", "combine") := NULL]
normal_combined_final <- rbindlist(l = list(normal_total_final, normal_main_inter_final), fill = TRUE)
col_order <- c("x_dist", "n", "data_gen_model", "est_model",
               "true_total","GCTA_total",  "proposed_GCTA_total",
               "true_main","GCTA_main",  "proposed_GCTA_main",
               "true_inter","GCTA_inter",  "proposed_GCTA_inter")
normal_combined_final <- normal_combined_final[, ..col_order]


## combine all the table to final output for prime meeing 
simulation_summary_table <- rbindlist(list(normal_combined_final, chi_combined_final, PCB_combined_final)) %>% setorder(., x_dist, n, data_gen_model, est_model) %>% data.frame(.)

## round to digital 3
col_names <- colnames(simulation_summary_table)[-(1:4)]
simulation_summary_table[, -(1:4)] <- simulation_summary_table[, -(1:4)] %>% round(.,2)
simulation_summary_table[is.na(simulation_summary_table)] <- "."
colnames(simulation_summary_table) <- gsub("_", " ", colnames(simulation_summary_table))
write.csv(simulation_summary_table, file = "./reports/proposed_GCTA_paper/prime_simulation_table.csv", row.names = FALSE)




#############################################################################################################################
## Variance estimation of GCTA
#############################################################################################################################
file_list_all <- list.files("./result/simulation_proposed_GCTA_paper/var_est/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/var_est/",.)

file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*sub_chi.*total",perl = TRUE)]
sub_chi_total <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)
sub_chi_total[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "total"," "))]
rm_col <- c( "combine", "n_sub", "inter_std", "GCTA_interaction", "sub_GCTA_interaction", 
             "prop_interaction","sub_prop_interaction","structure", "interaction_m","n",
             "main_fixed_var","main_random_var","inter_fixed_var","inter_random_var")
sub_chi_total[,(rm_col) := NULL]
sub_chi_total_est <- sub_chi_total[var_main_effect != 0,]
sub_chi_total_var_est <- sub_chi_total[var_main_effect == 0,][, c("sub_GCTA_total", "sub_prop_total")]
names(sub_chi_total_var_est) <- c("sub_GCTA_total_var", "sub_prop_total_var")
sub_chi_total_est_final <- cbind(sub_chi_total_est, sub_chi_total_var_est)

final_table <- sub_chi_total_est_final[, .(true_total = mean(var_total_effect), 
                                            GCTA_total = mean(GCTA_total),
                                            prop_total = mean(prop_total, na.rm = TRUE),
                                            sub_GCTA_total = mean(sub_GCTA_total),
                                            sub_prop_total = mean(sub_prop_total),
                                            var_GCTA = var(GCTA_total),
                                            var_sub_GCTA = mean(sub_GCTA_total_var),
                                            var_prop = var(prop_total, na.rm = TRUE),
                                            var_sub_prop = mean(sub_prop_total_var))]

write.csv(final_table, file = "./reports/proposed_GCTA_paper/sub_chi_total_est_final.csv", row.names = FALSE)



##################################################################################
file_list <- file_list_all[grep(x = file_list_all, pattern = "result_list_.*sub_bs_chi.*total",perl = TRUE)]
sub_chi_total <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(., fill = TRUE)
sub_chi_total[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "total"," "))]
rm_col <- c( "combine", "n_sub", "inter_std", "GCTA_interaction", "sub_GCTA_interaction", 
             "prop_interaction","sub_prop_interaction","structure", "interaction_m","n",
             "main_fixed_var","main_random_var","inter_fixed_var","inter_random_var")
sub_chi_total[,(rm_col) := NULL]
sub_chi_total_est <- sub_chi_total[var_main_effect != 0,]
sub_chi_total_var_est <- sub_chi_total[var_main_effect == 0,][, c("sub_GCTA_total", "sub_prop_total")]
names(sub_chi_total_var_est) <- c("sub_GCTA_total_var", "sub_prop_total_var")
sub_chi_total_est_final <- cbind(sub_chi_total_est, sub_chi_total_var_est)

sub_chi_total_est_final[, mean(GCTA_total)]
sub_chi_total_est_final[, mean(sub_GCTA_total)]
sub_chi_total_est_final[, var(GCTA_total)]
sub_chi_total_est_final[, mean(sub_GCTA_total_var)]

sub_chi_total_est_final[, mean(prop_total, na.rm = TRUE)]
sub_chi_total_est_final[, mean(sub_prop_total)]
sub_chi_total_est_final[, var(prop_total, na.rm = TRUE)]
sub_chi_total_est_final[, mean(sub_prop_total_var)]

write.csv(sub_chi_total_est_final, file = "./reports/proposed_GCTA_paper/sub_bs_chi_total_est_final.csv", row.names = FALSE)
