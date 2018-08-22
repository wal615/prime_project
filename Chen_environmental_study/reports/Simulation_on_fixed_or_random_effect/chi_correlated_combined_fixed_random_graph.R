library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")

# df = 10
## generating graph for the fixed_fixed simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_fixed_combine_df_10")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_main != 0, -c(2,4,6)]
plot_chi_fixed_fixed_main_combine_df_10 <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")



## generating graph for the fixed_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_random_combine_df_10")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_main != 0, -c(2,4,6)]
plot_chi_fixed_random_main_combine_df_10 <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")


## generating graph for the random_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_random_random_combine_df_10")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_main != 0, -c(2,4,6)]
plot_chi_random_random_main_combine_df_10 <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")


# df = 1
## generating graph for the fixed_fixed simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_fixed_combine_df_1")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_main != 0, -c(2,4,6)]
plot_chi_fixed_fixed_main_combine_df_1 <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")



## generating graph for the fixed_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_random_combine_df_1")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_main != 0, -c(2,4,6)]
plot_chi_fixed_random_main_combine_df_1 <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")


## generating graph for the random_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_random_random_combine_df_1")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_main != 0, -c(2,4,6)]
plot_chi_random_random_main_combine_df_1 <- tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")
