library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
# file_list_I <- file_list[grepl(pattern = "chi_I_inter", x = file_list, fixed = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)

# remove outliners based on sub-group
result_list_fixed <- result_list_fixed[, .SD[(prop_interaction < prop_interaction[order(prop_interaction, decreasing = TRUE)[10]]) | (prop_interaction ==0),], 
                                                             by = c("structure", "inter_fixed_var", "interaction_m")]

result_list_fixed_main <- result_list_fixed[true_main != 0, -(c(2,4,6)+3)] # remove inter
result_list_fixed_inter <- result_list_fixed[true_main != 0, -(c(1,3,5)+3)]

fixed_chi_main_I <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(structure, inter_fixed_var, interaction_m), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Main effect of Independent Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))


fixed_chi_inter_I <- tidyr::gather(result_list_fixed_inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Interaction effect of Independent Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  facet_wrap_paginate(facets = vars(structure, inter_fixed_var, interaction_m), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1)+
  theme(plot.title = element_text(hjust = 0.5))

fixed_chi_main_un <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  facet_wrap_paginate(facets = vars(structure, inter_fixed_var, interaction_m), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 2) +
  ggtitle("Main effect of correlated Chi") +
  theme(plot.title = element_text(hjust = 0.5))

fixed_chi_inter_un <- tidyr::gather(result_list_fixed_inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Interaction effect of correlated Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  facet_wrap_paginate(facets = vars(structure, inter_fixed_var, interaction_m), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 2, shrink = TRUE)+
  theme(plot.title = element_text(hjust = 0.5))


pdf(file = "./reports/proposed_GCTA_paper/test_chi_I_ui.pdf",
    width = 8,
    height = 6)

print(fixed_chi_main_I)
print(fixed_chi_inter_I)

print(fixed_chi_main_un)
print(fixed_chi_inter_un)
dev.off()

###################################################################################################################
## combine
###################################################################################################################

### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "chi.*combine")]
# file_list_I <- file_list[grepl(pattern = "chi_I_inter", x = file_list, fixed = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)

# remove outliners based on sub-group
# result_list_fixed <- result_list_fixed[, .SD[(prop_interaction < prop_interaction[order(prop_interaction, decreasing = TRUE)[5]]) | (prop_interaction ==0),], 
#                                        by = c("structure", "inter_fixed_var", "interaction_m")]

result_list_fixed_total <- result_list_fixed[true_total != 0, -c(2,4,6)] # remove inter


fixed_chi_total_I_un <- tidyr::gather(result_list_fixed_total, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(structure, inter_fixed_var), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("total effect of Independent and correlated Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))


### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "chi.*main")]
# file_list_I <- file_list[grepl(pattern = "chi_I_inter", x = file_list, fixed = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)

# remove outliners based on sub-group
# result_list_fixed <- result_list_fixed[, .SD[(prop_interaction < prop_interaction[order(prop_interaction, decreasing = TRUE)[5]]) | (prop_interaction ==0),], 
#                                        by = c("structure", "inter_fixed_var", "interaction_m")]

result_list_fixed_main <- result_list_fixed[true_main != 0, -c(2,4,6)] # remove inter


fixed_chi_main_I_un <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(structure, inter_fixed_var), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("main effect of Independent and correlated Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))


pdf(file = "./reports/proposed_GCTA_paper/test_chi_I_ui_combine_main.pdf",
    width = 8,
    height = 6)

print(fixed_chi_total_I_un)
print(fixed_chi_main_I_un)
dev.off()

###################################################################################################################
## PCB Total
###################################################################################################################

### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*total$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_total <- result_list_fixed[true_total != 0, -c(2,4,6)] # remove inter
result_list_fixed_total[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), "total")]

fixed_PCB_total <- tidyr::gather(result_list_fixed_total, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model,pro), 
                      ncol = 3,
                      nrow = 2, 
                      scales = "free", 
                      labeller  = label_both, 
                      page = 1) +
  ggtitle("Total effect of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

###################################################################################################################
##  PCB main
###################################################################################################################

### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*main$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_main <- result_list_fixed[true_main != 0, -c(2,4,6)] # remove inter
result_list_fixed_main[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

fixed_PCB_main <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model, pro), ncol = 3 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("main effect of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

###################################################################################################################
##  PCB maininter
###################################################################################################################

file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*main_inter",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

result_list_fixed_main <- result_list_fixed[true_main != 0, -c(2,4,6)] # remove inter
fixed_PCB_main_inter_main <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model, pro), ncol = 3 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Main of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

result_list_fixed_inter <- result_list_fixed[true_main != 0, -c(1,3,5)] # remove inter
fixed_PCB_main_inter_inter <- tidyr::gather(result_list_fixed_inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model, pro), ncol = 3 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Interaction of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

###################################################################################################################
##  PCB combine dim
###################################################################################################################

file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*dim$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var ==0, "main","main+inter"), "total")]
result_list_fixed <- result_list_fixed[true_total != 0, -c(2,4,6)]

fixed_PCB_total_dim <- tidyr::gather(result_list_fixed, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model, pro, reduce_coef), ncol = 3 ,nrow = 3, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("dim total of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))


pdf(file = "./reports/proposed_GCTA_paper/test_fixed_PCB.pdf",
    width = 8,
    height = 9)

print(fixed_PCB_main_inter_main)
print(fixed_PCB_main_inter_inter)
print(fixed_PCB_main)
print(fixed_PCB_total)
print(fixed_PCB_total_dim)

dev.off()

