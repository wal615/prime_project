library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list_0[grep(x = file_list_0, pattern = "chi_I.*main_inter$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

result_list_fixed_main <- result_list[true_main != 0, -c(2,4,6)] # remove inter
result_list_fixed_inter <- result_list[true_main != 0, -c(1,3,5)]

fixed_chi_I_main_inter_main <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Main effect of Independent Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))


fixed_chi_I_main_inter_inter <- tidyr::gather(result_list_fixed_inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Interaction effect of Independent Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

file_list <- file_list_0[grep(x = file_list_0, pattern = "chi_I.*main$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]
result_list_fixed_main <- result_list[true_main != 0, -c(2,4,6)] # remove inter

fixed_chi_I_main <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Main effect of Independent Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

file_list <- file_list_0[grep(x = file_list_0, pattern = "chi_I.*total$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), "total")]
result_list_fixed_main <- result_list[true_total != 0, -c(2,4,6)] # remove inter

fixed_chi_I_total <- tidyr::gather(result_list_fixed_main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(data_gen_model, est_model), ncol = 2 ,nrow = 2, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Total effect of Independent Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))



pdf(file = "./reports/proposed_GCTA_paper/test_chi.pdf",
    width = 8,
    height = 10)

print(fixed_chi_main_I)
print(fixed_chi_inter_I)

print(fixed_chi_main_un)
print(fixed_chi_inter_un)
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

