library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")

## generating graph for the fixed_fixed simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_fixed")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_main != 0, -c(2,4,6)]
plot_chi_fixed_fixed_main <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "fixed", labeller  = "label_both")

inter <- table_fixed_fixed[true_main != 0, -c(1,3,5)]
plot_chi_fixed_fixed_inter <- tidyr::gather(inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "fixed", labeller  = "label_both")


## generating graph for the fixed_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_random")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_main != 0, -c(2,4,6)]
plot_chi_fixed_random_main <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "fixed", labeller  = "label_both")

inter <- table_fixed_random[true_main != 0, -c(1,3,5)]
plot_chi_fixed_random_inter <- tidyr::gather(inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "fixed", labeller  = "label_both")


## generating graph for the random_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_random_random")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_main != 0, -c(2,4,6)]
plot_chi_random_random_main <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "fixed", labeller  = "label_both")

inter <- table_random_random[true_main != 0, -c(1,3,5)]
plot_chi_random_random_inter <- tidyr::gather(inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "fixed", labeller  = "label_both")

