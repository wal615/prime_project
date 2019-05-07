library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
setwd("~/dev/projects/Chen_environmental_study/")

file_list_all <- list.files("./result/simulation_proposed_GCTA_paper/var_est/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/var_est/",.)

#############################################################################################################################
## Variance estimation of GCTA
#############################################################################################################################
file_list_all <- list.files("./result/simulation_proposed_GCTA_paper/var_est/result_list_fixed_sub_chi_un_main_0.5_inter_0.1_total/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/var_est/result_list_fixed_sub_chi_un_main_0.5_inter_0.1_total/",.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]

sub_total <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_total[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "total"," "))]

# histogram
prop_total <- sub_total[, .(prop_total = mean(prop_total)), by = i]
prop_total$i <- 0
sub_prop_total <- sub_total[i %in% sample(1:100, 4), .(prop_total = sub_prop_total,i)]
hist_table <- rbindlist(l = list(sub_prop_total,prop_total), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,var(prop_total, na.rm = TRUE), by = i]
hist_plot_total_original_scale <- ggplot(data = hist_table, aes(x=prop_total, fill=i)) + 
                                  geom_histogram(alpha=0.5, position="identity", bins = 35) +
                                  annotation_custom(tableGrob(var_table, rows=NULL), 
                                                  xmin=100, xmax=125, ymin=100, ymax=300) +
                                  ggtitle("hist of total effect of original scale") +
                                  theme_bw() +
  
                
ggsave("./reports/proposed_GCTA_paper/est_var_analysis/original_total.pdf", 
       plot = hist_plot_total_original_scale, 
       dpi = 1200,width = 10)

## after log_transformation
log_sub_total <- sub_total[,.(log_prop_total = log(prop_total), sub_log_prop_total = log(sub_prop_total), i = i)]
log_prop_total <- log_sub_total[, .(log_prop_total = mean(log_prop_total)), by = i]
log_prop_total$i <- 0
sub_log_prop_total <- log_sub_total[i %in% sample(1:100, 4), .(log_prop_total = sub_log_prop_total,i)]
hist_table <- rbindlist(l = list(sub_log_prop_total,log_prop_total), use.names = TRUE)
hist_table[, i := as.character(i)]
var_table <- hist_table[,var(log_prop_total, na.rm = TRUE), by = i]
hist_plot_total_log_scale <- ggplot(data = hist_table, aes(x=log_prop_total, fill=i)) + 
  geom_histogram(alpha=0.5, position="identity", bins = 35) +
  annotation_custom(tableGrob(var_table, rows=NULL), 
                    xmin=4.5, xmax=4.5, ymin=100, ymax=150) +
  ggtitle("hist of total effect of log scale")
  theme_bw()
  
ggsave("./reports/proposed_GCTA_paper/est_var_analysis/log_total.pdf", 
         plot = hist_plot_total_log_scale, 
         dpi = 1200,width = 10)

# save the table 
write.csv(sub_total, file = "./result/simulation_proposed_GCTA_paper/var_est/result_list_fixed_sub_chi_un_main_0.5_inter_0.1_total/sub_sampling_chi_total.csv", row.names = FALSE)
