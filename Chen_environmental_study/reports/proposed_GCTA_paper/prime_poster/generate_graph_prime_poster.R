library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")
main_col_name <- c("true_main", "GCTA_main", "prop_main")
inter_col_name <- c("true_interaction", "GCTA_interaction", "prop_interaction")
file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/prime_slides/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/prime_slides/",.)
#####################################
## chi_un
#####################################

file_list <- file_list_0[grep(x = file_list_0, pattern = "chi_cs.*$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

result_list_fixed_main <- result_list[true_main != 0, !inter_col_name, with = FALSE] # remove inter

fixed_chi_cs_main_inter_main <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "variance_est_method", value = "est_variance") %>%
  ggplot(., aes(x = variance_est_method, y = est_variance, fill = variance_est_method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model, rho), ncol = 3 ,nrow = 3, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Main effect estimation of Dependent Chi-square with compound symmetry") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))





###################################################################################################################
## PCB Total
###################################################################################################################



file_list <- file_list_0[grep(x = file_list_0, pattern = "PCB.*inter_1_total",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_total <- result_list_fixed[true_total != 0, -c(2,4,6)] # remove inter
result_list_fixed_total[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), "total")]

fixed_PCB_total_dim_inter_std <- tidyr::gather(result_list_fixed_total, ends_with("total"), key = "variance_est_method", value = "est_variance") %>%
  ggplot(., aes(x = variance_est_method, y = est_variance, fill = variance_est_method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model, pro), 
                      ncol = 3,
                      nrow = 3, 
                      scales = "free", 
                      labeller = label_both, 
                      page = 1) +
  ggtitle("Total effect estimation of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))




# PCB

ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/total_dim_dependent_PCB_pro_0.1_0.9.pdf", plot = fixed_PCB_total_dim_inter_std, device = NULL, path = NULL,
       scale = 1, dpi = 1200, width = 8, height = 9)


# chi-square 
ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/main_dependent_chi.pdf", plot = fixed_chi_cs_main_inter_main, device = NULL, path = NULL,
       scale = 1, dpi = 1200, width = 8, height = 9)

