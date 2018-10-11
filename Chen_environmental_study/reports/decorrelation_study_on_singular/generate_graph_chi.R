library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
## generating graph for the fixed_fixed simuation chi n 100-500 p_34
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_n_100_500_p_34")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_chi_fixed_fixed_total_p_34_n_100_600 <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, n), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with fixed main and fixed interactive") +
  theme(plot.title = element_text(hjust = 0.5))

## generating graph for the fixed_fixed simuation chi n 100-500 p_34 SVD dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_cho_0.5_n_100_500_p_34_svd_reduction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_chi_fixed_fixed_total_p_34_n_100_700_svd_dim_red <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, n), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with fixed main and fixed interactive") +
  theme(plot.title = element_text(hjust = 0.5))

#####################################################################################################

## fixed random
## generating graph for the fixed_random simuation chi n 100-500 p_34
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_n_100_500_p_34")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
plot_chi_fixed_random_total_p_34_n_100_600 <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, n), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with fixed main and random interactive") +
  theme(plot.title = element_text(hjust = 0.5))

## generating graph for the fixed_fixed simuation chi n 100-500 p_34 SVD dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_cho_0.5_n_100_500_p_34_svd_reduction")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
plot_chi_fixed_random_total_p_34_n_100_700_svd_dim_red <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, n), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with fixed main and fixed interactive") +
  theme(plot.title = element_text(hjust = 0.5))
#################################################################################################################################

### random random
## generating graph for the random_random simuation chi n 100-500 p_34
load(file = "./result/simulation_decorrelation/simulation_result_list_random_random_ind_chi_n_100_500_p_34")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_total != 0, -c(2,4,6)]
plot_chi_random_random_total_p_34_n_100_600 <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, n), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with random main and random interactive") +
  theme(plot.title = element_text(hjust = 0.5))

## generating graph for the random_random simuation chi n 100-500 p_34 SVD dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_random_random_ind_chi_cho_0.5_n_100_500_p_34_svd_reduction")
table_random_random <- rbindlist(result_list_random_random)

main <- table_random_random[true_total != 0, -c(2,4,6)]
plot_chi_random_random_total_p_34_n_100_700_svd_dim_red <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(main_fixed, inter_fixed, n), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with fixed main and fixed interactive") +
  theme(plot.title = element_text(hjust = 0.5))