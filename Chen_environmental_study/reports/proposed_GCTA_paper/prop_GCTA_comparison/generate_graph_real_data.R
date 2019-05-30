library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")
main_col_name <- c("true_main", "GCTA_main", "prop_main")
inter_col_name <- c("true_interaction", "GCTA_interaction", "prop_interaction")


##############################
## hemolobin
##############################
file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list_0[grep(x = file_list_0, pattern = "hemolobin.*total$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list <- result_list %>% na.omit(.)

real_nhance_hemolobin_total <- tidyr::gather(result_list, matches("total|var"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(pro), ncol = 3 ,nrow =1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Total effect estimation of hemoglobin") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

pdf(file = "./reports/proposed_GCTA_paper/nhance/hemolobin/total.pdf",
    width = 10,
    height = 6)


print(real_nhance_hemolobin_total)

dev.off()


## main

file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list_0[grep(x = file_list_0, pattern = "hemolobin.*main$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list <- result_list %>% na.omit(.)

real_nhance_hemolobin_main <- tidyr::gather(result_list, matches("main|var"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(pro), ncol = 3 ,nrow =1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("main effect estimation of hemoglobin") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

pdf(file = "./reports/proposed_GCTA_paper/nhance/hemolobin/main.pdf",
    width = 10,
    height = 6)


print(real_nhance_hemolobin_main)

dev.off()

##############################
## Wqs
##############################
file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list_0[grep(x = file_list_0, pattern = ".*reduction$")]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list <- result_list %>% na.omit(.)

real_wqs_total_dim_reduction <- tidyr::gather(result_list, matches("total|var"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(pro, reduce_coef), ncol = 3 ,nrow = 3, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Total effect estimation of wqs data") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

pdf(file = "./reports/proposed_GCTA_paper/wqs/total_dim_reduction.pdf",
    width = 8,
    height = 11)


print(real_wqs_total_dim_reduction)

dev.off()