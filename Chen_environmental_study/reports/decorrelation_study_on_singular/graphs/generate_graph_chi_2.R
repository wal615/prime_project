library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")

#################################################################################################################
# fixed fixed
## generating graph for the fixed_fixed simuation chi n 100-500 p_34 SVD dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_500_p_34_svd_reduction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/graphs/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_700_svd_dim_red.pdf",
    width = 8,
    height = 7)
for(i in 1:length(uni_value)){
  
  # subset the values
  main_tmp <- main[rho == uni_value[i]]
  
  print(tidyr::gather(main_tmp, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(rho, n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Total effect with fixed main and fixed interactive with SVD method") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

## generating graph for the fixed_fixed simuation chi n 100-500 p_34 PCA dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_500_p_34_pca_reduction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages pca
pdf(file = "./reports/decorrelation_study_on_singular/graphs/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_700_pca_dim_red.pdf",
    width = 8,
    height = 7)
for(i in 1:length(uni_value)){
  
  # subset the values
  main_tmp <- main[rho == uni_value[i]]
  
  print(tidyr::gather(main_tmp, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(rho, n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Total effect with fixed main and fixed interactive with PCA method") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

#####################################################################################################

## fixed random
## generating graph for the fixed_random simuation chi n 100-500 p_34
## generating graph for the fixed_fixed simuation chi n 100-500 p_34 SVD dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_rho_0.1_0.9_n_100_500_p_34_svd_reduction")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/graphs/plot_chi_fixed_random_total_p_34_rho_0.1_0.9_n_100_700_svd_dim_red.pdf",
    width = 8,
    height = 7)
for(i in 1:length(uni_value)){
  
  # subset the values
  main_tmp <- main[rho == uni_value[i]]
  
  print(tidyr::gather(main_tmp, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(rho, n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Total effect with fixed main and fixed interactive with SVD method") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

## generating graph for the fixed_random simuation chi n 100-500 p_34 pca dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_rho_0.1_0.9_n_100_500_p_34_pca_reduction")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages
pdf(file = "./reports/decorrelation_study_on_singular/graphs/plot_chi_fixed_random_total_p_34_rho_0.1_0.9_n_100_700_pca_dim_red.pdf",
    width = 8,
    height = 7)
for(i in 1:length(uni_value)){
  
  # subset the values
  main_tmp <- main[rho == uni_value[i]]
  
  print(tidyr::gather(main_tmp, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(rho, n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Total effect with fixed main and random interactive PCA") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

#################################################################################################################################
# random random