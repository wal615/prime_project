library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")




#####################################################################################################
## fixed fixed
#####################################################################################################


## no interaction
## generating graph for the fixed_fixed simuation chi main
y_limit <- c(-1,1)
pdf(file = "./reports/compare_difference_of_main_total/cov_pdf_graph/fixed_fixed_un_chi_main_and_total_inter_0_cov.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_0")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_0_svd_0.5")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]

merged_main_total <- merge(x = main, y = total[,c("true_total","prop_total")], all =FALSE, by.x = "true_main", by.y = "true_total")
merged_main_total[,true_cov:= 0]
merged_main_total[,est_cov:= prop_total - prop_main -prop_interaction]

print(tidyr::gather(merged_main_total, ends_with("cov"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Covariance of Chi-square with fixed main and fixed interactive \n without interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()


## interaction
## generating graph for the fixed_fixed simuation chi main

pdf(file = "./reports/compare_difference_of_main_total/cov_pdf_graph/fixed_fixed_un_chi_main_and_total_inter_1_0_interaction_cov.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_1")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_1_svd_0.5")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]

merged_main_total <- merge(x = main, y = total[,c("true_total","true_interaction","prop_total")], all =FALSE, by = "true_interaction")
merged_main_total[,true_cov:= true_total - true_main - true_interaction]
merged_main_total[,est_cov:= prop_total - prop_main -prop_interaction]

print(tidyr::gather(merged_main_total, ends_with("cov"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Covariance of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

## strong interaction
## generating graph for the fixed_fixed simuation chi main

y_limit <- c(5,-30)
pdf(file = "./reports/compare_difference_of_main_total/cov_pdf_graph/fixed_fixed_un_chi_main_and_total_inter_1_m_0.015_interaction_cov.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_1_m_0.015_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_1_svd_0.5_m_0.015_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]

merged_main_total <- merge(x = main, y = total[,c("true_total","true_interaction","prop_total")], all =FALSE, by = "true_interaction")
merged_main_total[,true_cov:= true_total - true_main - true_interaction]
merged_main_total[,est_cov:= prop_total - prop_main -prop_interaction]

print(tidyr::gather(merged_main_total, ends_with("cov"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Covariance of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

## strong interaction
## generating graph for the fixed_fixed simuation chi main

y_limit <- c(30,-60)
pdf(file = "./reports/compare_difference_of_main_total/cov_pdf_graph/fixed_fixed_un_chi_main_and_total_inter_1_v_0.5_interaction_cov.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_1_v_0.5_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_1_svd_0.5_v_0.5_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]

merged_main_total <- merge(x = main, y = total[,c("true_total","true_interaction","prop_total")], all =FALSE, by = "true_interaction")
merged_main_total[,true_cov:= true_total - true_main - true_interaction]
merged_main_total[,est_cov:= prop_total - prop_main -prop_interaction]

print(tidyr::gather(merged_main_total, ends_with("cov"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Covariance of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()