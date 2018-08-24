library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")

## generating graph for the fixed_fixed simuation
load(file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_main")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)

main <- table_fixed_fixed[true_main != 0, -c(2,4,6)]
plot_chi_fixed_fixed_main <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")+
  ggtitle("Main effect with fixed main and fixed interactive") +
  theme(plot.title = element_text(hjust = 0.5))

## generating graph for the fixed_fixed simuation
load(file = "./result/PCB_resampling/simulation_result_list_fixed_fixed")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_chi_fixed_fixed_total <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with fixed main and fixed interactive") +
  theme(plot.title = element_text(hjust = 0.5))

## generating graph for the fixed_random simuation
load(file = "./result/PCB_resampling/simulation_result_list_fixed_random_main")
table_fixed_random <- rbindlist(result_list_fixed_random_main)

main <- table_fixed_random[true_main != 0, -c(2,4,6)]
plot_chi_fixed_random_main <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")

## generating graph for the fixed_random simuation
load(file = "./result/PCB_resampling/simulation_result_list_fixed_random")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
plot_chi_fixed_random_total <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")



## generating graph for the random_random simuation
load(file = "./result/PCB_resampling/simulation_result_list_random_random_main")
table_random_random <- rbindlist(result_list_random_random_main)

main <- table_random_random[true_main != 0, -c(2,4,6)]
plot_chi_random_random_main <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")

## generating graph for the random_random simuation
load(file = "./result/PCB_resampling/simulation_result_list_random_random")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_total != 0, -c(2,4,6)]
plot_chi_random_random_total <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")