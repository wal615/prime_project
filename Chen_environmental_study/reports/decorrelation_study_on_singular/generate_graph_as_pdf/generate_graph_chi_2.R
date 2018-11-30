library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")

#####################################################################################################
## fixed fixed
#####################################################################################################


## generating graph for the fixed_fixed simuation chi
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_PCB_pro_0.1_0.9_red_0.5")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_PCB_fixed_fixed_total_p_34_pro_0.1_0.9_svd_red_0.5.pdf",
    width = 8,
    height = 7)

  print(tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(pro), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Total effect of PCB with fixed main and fixed interactive \n with SVD method with 50% covariate sub-sampling") +
          theme(plot.title = element_text(hjust = 0.5)))
dev.off()

## generating graph for the fixed_fixed simuation chi
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34_svd_dim_100")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800_svd_dim_100.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive with SVD method with 100 covariate") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################

## generating graph for the fixed_fixed simuation chi with dimension reduction
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34_svd_red_0.5")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800_svd_red_0.5.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive with SVD method with 50% covariate") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################

## generating graph for the fixed_fixed simuation ar chi n 100-800 p_34 with svd 0.5
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ar_chi_rho_0.1_0.9_n_100_800_p_34")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]
## generate a pdf file with multiple pages pca
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_ar_rho_0.1_0.9_n_100_800_svd_red_0.5.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive no dimension reduction") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################

## generating graph for the fixed_fixed simuation chi with dimension reduction
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34_svd_red_0.2")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800_svd_red_0.2.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive with SVD method with 20% covariate") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################

## generating graph for the fixed_fixed simuation chi with dimension reduction
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34_svd_red_0.9")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800_svd_red_0.9.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive with SVD method with 90% covariate") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################

## generating graph for the fixed_fixed simuation PCB with dimension reduction
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_PCB_pro_0.1_0.9_red_0.5")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_PCB_pro_0.1_0.9_svd_red_0.5.pdf",
    width = 8,
    height = 7)
  
  print(tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(pro), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Total effect with fixed main and fixed interactive with SVD method with 50% covariate") +
          theme(plot.title = element_text(hjust = 0.5)))
dev.off()

################################################

## generating graph for the fixed_fixed simuation chi n 100-500 p_34 PCA dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34_pca")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages pca
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800_p_34_pca.pdf",
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

################################################

## generating graph for the fixed_fixed simuation chi n 100-500 p_34 with no dimension reduction
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]
## generate a pdf file with multiple pages pca
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive no dimension reduction") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################################################################

## generating graph for the fixed_fixed simuation chi n 100-800 p_34 with ar structure SVD dim_reduction 0.5
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ar_chi_rho_0.1_0.9_n_100_800_p_34")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]
## generate a pdf file with multiple pages
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_ar_chi_rho_0.1_0.9_n_100_800_p_34_svd_0.5.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive ar structure svd 0.5") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################################################################

## generating graph for the fixed_fixed simuation chi n 100-800 p_34 with ar structure SVD dim_reduction 0.8
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ar_chi_rho_0.1_0.9_n_100_800_p_34_svd_0.8")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]
## generate a pdf file with multiple pages
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_ar_chi_rho_0.1_0.9_n_100_800_p_34_svd_0.8.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive ar structure svd 0.8") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()


################################################################################################

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ar_chi_rho_0.1_0.9_n_100_800_p_34_svd_0.5_un")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]

uni_value <- main[,unique(rho)][1]
## generate a pdf file with multiple pages
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_un_chi_rho_0.1_0.9_n_100_800_p_34_svd_0.5.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive un structure svd 0.5") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################################################################

## generating graph for the fixed_fixed simuation chi n 100-800 p_34 with PCB 0.8
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_PCB_p_33_svd_0.8")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
## generate a pdf file with multiple pages
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_PCB_fixed_fixed_pro_0.1_0.9_p_33_svd_0.8.pdf",
    width = 8,
    height = 7)
  
  # subset the values
  main_tmp <- main
  
  print(tidyr::gather(main_tmp, ends_with("total"), key = "method", value = "value") %>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          scale_y_continuous(trans='log10') +
          facet_wrap_paginate(facets = vars(pro), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("PCB Total effect with fixed main and fixed interactive svd 0.8") +
          theme(plot.title = element_text(hjust = 0.5)))
dev.off()

#####################################################################################################
## fixed random
#####################################################################################################

## generating graph for the fixed_random simuation chi 
## generating graph for the fixed_fixed simuation chi SVD dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_rho_0.1_0.9_n_100_800_p_34_svd_dim_100")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages svd
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_random_total_p_34_rho_0.1_0.9_n_100_800_svd_dim_100.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive with SVD method with 100 covariates") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

################################################

## generating graph for the fixed_random simuation chi n pca dim_re
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_random_ind_chi_rho_0.1_0.9_n_100_800_p_34_pca")
table_fixed_random <- rbindlist(result_list_fixed_random)

main <- table_fixed_random[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]

## generate a pdf file with multiple pages
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_random_total_p_34_rho_0.1_0.9_n_100_800_p_34_pca.pdf",
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

################################################

## generating graph for the fixed_random simuation chi n 100-500 p_34 with no dimension reduction
load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_ind_chi_rho_0.1_0.9_n_100_800_p_34")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
uni_value <- main[,unique(rho)]
## generate a pdf file with multiple pages pca
pdf(file = "./reports/decorrelation_study_on_singular/generate_graph_as_pdf/plot_chi_fixed_fixed_total_p_34_rho_0.1_0.9_n_100_800.pdf",
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
          ggtitle("Total effect with fixed main and fixed interactive no dimension reduction") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()
