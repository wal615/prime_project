library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")


y_limit <- c(0.01,16)

#####################################################################################################
## fixed fixed
#####################################################################################################

## no interaction
## generating graph for the fixed_fixed simuation chi main

pdf(file = "./reports/compare_difference_of_main_total/pdf_graph/fixed_fixed_un_chi_main_and_total_no_interaction.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_0")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]
main <- main[, true_interaction := ifelse(true_interaction>0,true_interaction, 0.01)]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_0_svd_0.5")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]
total <- total[, true_interaction := ifelse(true_interaction>0,true_interaction, 0.01)]

print(tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Main effect of Chi-square with fixed main and fixed interactive \n without interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = y_limit))
       

print(tidyr::gather(main, ends_with("interaction"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("interaction effect of Chi-square with fixed main and fixed interactive \n without interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = y_limit))



print(tidyr::gather(total, ends_with("total"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("total effect of Chi-square with fixed total and fixed interactive \n without interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_cartesian(ylim = y_limit))

print(tidyr::gather(total, ends_with("interaction"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("interaction effect of Chi-square with fixed total and fixed interactive \n without interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_cartesian(ylim = y_limit))
dev.off()

## with interaction
pdf(file = "./reports/compare_difference_of_main_total/pdf_graph/fixed_fixed_un_chi_main_and_total_interaction.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_1_0_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]
main <- main[, true_interaction := ifelse(true_interaction>0,true_interaction, 0.01)]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_1_svd_0.5_0_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]
total <- total[, true_interaction := ifelse(true_interaction>0,true_interaction, 0.01)]



print(tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Main effect of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = y_limit))

print(tidyr::gather(main, ends_with("interaction"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("interaction effect of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = y_limit))


print(tidyr::gather(total, ends_with("total"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("total effect of Chi-square with fixed total and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_cartesian(ylim = y_limit))

print(tidyr::gather(total, ends_with("interaction"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("interaction effect of Chi-square with fixed total and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_cartesian(ylim = y_limit))

dev.off()



################################################################################
## with stronger interaction effect
################################################################################

pdf(file = "./reports/compare_difference_of_main_total/pdf_graph/fixed_fixed_un_chi_main_and_total_v_0.5_interaction.pdf",
    width = 8,
    height = 7)

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_main_interm_1_inter_1_v_0.5_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_main)
main <- table_fixed_fixed[true_main != 0]
main <- main[, true_interaction := ifelse(true_interaction>0,true_interaction, 0.01)]

load(file = "./result/simulation_decorrelation/simulation_result_list_fixed_fixed_un_chi_total_inter_1_svd_0.5_v_0.5_interaction")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total)
total <- table_fixed_fixed[true_total != 0]
total <- total[, true_interaction := ifelse(true_interaction>0,true_interaction, 0.01)]

y_limit <- c(0.01,190)


print(tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("Main effect of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = y_limit))

print(tidyr::gather(main, ends_with("interaction"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("interaction effect of Chi-square with fixed main and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = y_limit))


print(tidyr::gather(total, ends_with("total"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("total effect of Chi-square with fixed total and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_cartesian(ylim = y_limit))

print(tidyr::gather(total, ends_with("interaction"), key = "method", value = "value") %>%
        ggplot(., aes(x = method, y = value, fill = method)) +
        geom_violin(alpha = 0.2) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap_paginate(facets = vars(n), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
        ggtitle("interaction effect of Chi-square with fixed total and fixed interactive \n with interaction effect") +
        theme(plot.title = element_text(hjust = 0.5)) + 
        coord_cartesian(ylim = y_limit))

dev.off()