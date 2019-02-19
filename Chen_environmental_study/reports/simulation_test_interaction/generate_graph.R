library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")

#####################################################################################################
## no interaction
#####################################################################################################


## generating graph for the fixed_fixed simuation chi
load(file = "./result/simulation_test_inter/simulation_result_list_fixed_fixed_cs_chi_main_interm_0_inter_0_mean_0")
main <- rbindlist(result_list_fixed_fixed_main)
main <- main[true_main != 0, ]

load(file = "./result/simulation_test_inter/simulation_result_list_fixed_fixed_cs_chi_total_inter_0_svd_0.5_mean_0")
total <- rbindlist(result_list_fixed_fixed_total)
total <- total[true_total != 0]

main_total <- merge(x = main, y = total, by.x = "true_main", by.y = "true_total")
main_total_inter0 <- main_total[,.(true_main, rho.y, n.y, interaction = true_interaction.x), 
                          .(GCTA_diff = abs(GCTA_main - GCTA_total), proposed_diff = abs(prop_main - prop_total))]

## generate a pdf 

pdf(file = "./reports/simulation_test_interaction/pdf/fixed_fixed_cs_chi_main_total_diffc_inter_0_mean_0.pdf",
    width = 8,
    height = 7)
for(i in sort(unique(main_total_inter0$n.y))){
  main_total_tem <- main_total_inter0[n.y == i]
  final_table <- tidyr::gather(main_total_tem, c(GCTA_diff, proposed_diff, interaction), key = "method", value = "value")
  final_table$method <- final_table$method %>% factor(., levels = c("GCTA_diff", "proposed_diff", "interaction") ) 
  print( final_table%>%
          ggplot(., aes(x = method, y = value, fill = method)) +
          geom_violin(alpha = 0.2) +
          geom_boxplot(alpha = 0.7) +
          facet_wrap_paginate(facets = vars(n.y, rho.y), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
          ggtitle("Difference of Total and main effect estimation when no interaction Chi CS") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()


#####################################################################################################
## with interaction
#####################################################################################################


## generating graph for the fixed_fixed simuation chi
load(file = "./result/simulation_test_inter/simulation_result_list_fixed_fixed_cs_chi_main_interm_0_inter_1_mean_0")
main <- rbindlist(result_list_fixed_fixed_main)
main <- main[true_main != 0, ]

load(file = "./result/simulation_test_inter/simulation_result_list_fixed_fixed_cs_chi_total_inter_1_svd_0.5_mean_0")
total <- rbindlist(result_list_fixed_fixed_total)
total <- total[true_total != 0]

main_total <- merge(x = main, y = total, by.x = "true_interaction", by.y = "true_interaction")
main_total_inter1 <- main_total[,.(true_main, rho.y, n.y, interaction = true_interaction), 
                         .(GCTA_diff = abs(GCTA_main - GCTA_total), proposed_diff = abs(prop_main - prop_total))]

## generate a pdf 

pdf(file = "./reports/simulation_test_interaction/pdf/fixed_fixed_cs_chi_main_total_diffc_inter_1_mean_0.pdf",
    width = 8,
    height = 7)
for(i in sort(unique(main_total_inter1$n.y))){
  main_total_tem <- main_total_inter1[n.y == i]
  final_table <- tidyr::gather(main_total_tem, c(GCTA_diff, proposed_diff, interaction), key = "method", value = "value")
  final_table$method <- final_table$method %>% factor(., levels = c("GCTA_diff", "proposed_diff", "interaction") ) 
  print( final_table%>%
           ggplot(., aes(x = method, y = value, fill = method)) +
           geom_violin(alpha = 0.2) +
           geom_boxplot(alpha = 0.7) +
           facet_wrap_paginate(facets = vars(n.y, rho.y), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
           ggtitle("Difference of Total and main effect estimation when with interaction Chi CS") +
           theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()


#####################################################################################################
## proposed method with and without interaction term
#####################################################################################################

main_total_combined <- rbindlist(list(main_total_inter0[,inter := "0"], main_total_inter1[,inter := "1"]), use.names = TRUE)

## generate a pdf 

pdf(file = "./reports/simulation_test_interaction/pdf/fixed_fixed_cs_chi_main_total_diffc_proposed_mean_0.pdf",
    width = 8,
    height = 7)
for(i in sort(unique(main_total_combined$n.y))){
  main_total_tem <- main_total_combined[n.y == i]
  print( main_total_tem %>%
           ggplot(., aes(x = inter, y = proposed_diff, fill = inter)) +
           geom_violin(alpha = 0.2) +
           geom_boxplot(alpha = 0.7) +
           facet_wrap_paginate(facets = vars(n.y, rho.y), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
           ggtitle("Compare with proposed method") +
           theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()

pdf(file = "./reports/simulation_test_interaction/pdf/fixed_fixed_cs_chi_main_total_diffc_GCTA_mean_0.pdf",
    width = 8,
    height = 7)
for(i in sort(unique(main_total_combined$n.y))){
  main_total_tem <- main_total_combined[n.y == i]
  print( main_total_tem %>%
           ggplot(., aes(x = inter, y = GCTA_diff, fill = inter)) +
           geom_violin(alpha = 0.2) +
           geom_boxplot(alpha = 0.7) +
           facet_wrap_paginate(facets = vars(n.y, rho.y), ncol = 3 ,nrow = 3, scales = "fixed", labeller  = "label_both", page = 1)+
           ggtitle("Compare with GCTA method") +
           theme(plot.title = element_text(hjust = 0.5)))
}
dev.off()
