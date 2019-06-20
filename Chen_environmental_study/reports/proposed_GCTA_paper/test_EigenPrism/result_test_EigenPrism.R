library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)

# modified mean and var so that they ignores na
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}

upper = 0.9
lower = 0.1
z_p <-qnorm(lower, lower.tail = F)
CI_length <- function(x, upper, lower){
  CI_precentile <- quantile(x, c(lower, upper),na.rm = T)
  CI_precentile[2] - CI_precentile[1]
}
test_fn <- function(...){browser()}
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/test_EigenPrism/")
result_list <- read.csv(file = "./result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_100_500_1000_2000_p_500_1000_2000_rho_e_0.2_0.5_0.7_dim_red_coeff__iter_1000_EigenPrism_kernel_est_main/result.csv", stringsAsFactors = FALSE) %>% data.table(.)
result_list[,rho_e := as.character(rho_e)]
summary_result <- result_list[, .(Eg_mean = mean(EigenPrism_main, na.rm = TRUE),
                                  Eg_var = var(EigenPrism_main, na.rm = TRUE),
                                  Eg_mean_CI_length = CI_length(EigenPrism_main, upper = upper, lower = lower),
                                  Eg_mean_CI_z_length = 2 * sd(EigenPrism_main, na.rm = TRUE)*z_p,
                                  Eg_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE)), by = .(n,rho_e, p)] %>% setorder(., rho_e,n,p)
# result_list[, test_fn(EigenPrism_main,EigenPrism_CI1,EigenPrism_CI2), by = .(n, rho_e, p)]
CI_summary_result <- result_list[, .(coverage = (var_main_effect >= EigenPrism_CI1) & (var_main_effect <= EigenPrism_CI2),
                                     n =n,
                                     rho_e = rho_e,
                                     p =p)]
CI_coverage_rate <- CI_summary_result[, .(coverage_rate = mean(coverage, na.rm = TRUE)), by = .(rho_e, n,p)] %>% setorder(., rho_e,n,p)

mean_plot <- summary_result %>%
  ggplot(., aes(x = n, y = Eg_mean, group = rho_e)) +
  geom_line(aes(color = rho_e)) +
  geom_point(aes(color = rho_e)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Mean estimation of EigenPrism with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

coverage_rate_plot <- CI_coverage_rate %>%
  ggplot(., aes(x = n, y = coverage_rate, group = rho_e)) +
  geom_line(aes(color = rho_e)) +
  geom_point(aes(color = rho_e)) +
  geom_hline(yintercept = 0.8)+
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Coverage rate of EigenPrism CI with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()

CI_plot <- summary_result %>%
  tidyr::gather(., c("Eg_mean_CI_z_length","Eg_CI_length"), key = "method", value = "value") %>%
  ggplot(., aes(x = n, y = value, group = interaction(rho_e, method))) +
  geom_line(aes(color = rho_e)) +
  geom_point(aes(color = rho_e, shape = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("CI widths of EigenPrism with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
