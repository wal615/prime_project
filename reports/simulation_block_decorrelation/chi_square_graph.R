library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")

# only decorrelated main effect
## generating graph for the fixed_fixed simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_fixed_combine_df_1_only_decorr_main")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_chi_fixed_fixed_main_combine_df_1_only_decorr_main <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, rho), ncol =3 , scales = "free", labeller  = "label_both")


## generating graph for the fixed_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_random_combine_df_1_only_decorr_main")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
plot_chi_fixed_random_main_combine_df_1_only_decorr_main <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, rho), ncol =3 , scales = "free", labeller  = "label_both")


## generating graph for the random_random simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_random_random_combine_df_1_only_decorr_main")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_total != 0, -c(2,4,6)]
plot_chi_random_random_main_combine_df_1_only_decorr_main <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, rho), ncol =3 , scales = "free", labeller  = "label_both")


# both decorrelated main effect and inter
## generating graph for the fixed_fixed simuation
load(file = "./result/simulation_fixed_random/chi_square/simulation_result_list_fixed_fixed_combine_df_1_only_decorr_main_inter")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_chi_fixed_fixed_main_combine_df_1_only_decorr_main_inter <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, rho), ncol =3 , scales = "free", labeller  = "label_both")
