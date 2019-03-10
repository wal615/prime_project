library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)

file_list <- file_list[-grep(x = file_list, pattern = "combine|\\main\\b",perl = TRUE)]
result_list <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.,fill = TRUE)

result_list_chi <- result_list[x_dist == "chi"]
# remove outliners based on sub-group
result_list_chi <- result_list_chi[, .SD[(prop_interaction < prop_interaction[order(prop_interaction, decreasing = TRUE)[20]]) | (prop_interaction ==0),],
                                       by = c("structure", "inter_fixed_var", "interaction_m")]

result_list_chi_inter <- result_list_chi[true_main != 0 & interaction_m ==1, -(c(6,8))]

chi_interm_1_un_0 <- result_list_chi_inter[interaction_m ==1 & structure == "un" & inter_fixed_var ==0 & inter_random_var == 0,]  %>% 
                    setkey(., "true_main") %>% 
                    unique(., by = "true_main")
chi_interm_1_un_1 <- result_list_chi_inter[interaction_m ==1 & structure == "un" & inter_fixed_var ==0.1 & inter_random_var == 0,] %>% 
                    setkey(., "true_main") %>% 
                    unique(., by = "true_main")
chi_interm_1_un_merge <- merge(chi_interm_1_un_0, chi_interm_1_un_1, by = "true_main")
chi_interm_1_un_merge[, c("diff_interaction","diff_GCTA_interaction", "diff_prop_interaction") := list(true_interaction.y - true_interaction.x, GCTA_interaction.y - GCTA_interaction.x, prop_interaction.y - prop_interaction.x)]

tmp_table <- tidyr::gather(chi_interm_1_un_merge, starts_with("diff"), key = "method", value = "value")
tmp_table$method <-  factor(tmp_table$method, levels = c("diff_GCTA_interaction", "diff_prop_interaction","diff_interaction"))

fixed_chi_interm_1_un_merge <-  tmp_table %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(structure.x, x_dist.x), ncol = 1 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("interaction effect of un Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))
#####################################################################
## I
#####################################################################
chi_interm_1_I_0 <- result_list_chi_inter[interaction_m ==1 & structure == "I" & inter_fixed_var ==0 & inter_random_var == 0,]  %>% 
  setkey(., "true_main") %>% 
  unique(., by = "true_main")
chi_interm_1_I_1 <- result_list_chi_inter[interaction_m ==1 & structure == "I" & inter_fixed_var ==0.1 & inter_random_var == 0,] %>% 
  setkey(., "true_main") %>% 
  unique(., by = "true_main")
chi_interm_1_I_merge <- merge(chi_interm_1_I_0, chi_interm_1_I_1, by = "true_main")
chi_interm_1_I_merge[, c("diff_interaction","diff_GCTA_interaction", "diff_prop_interaction") := list(true_interaction.y - true_interaction.x, GCTA_interaction.y - GCTA_interaction.x, prop_interaction.y - prop_interaction.x)]

tmp_table <- tidyr::gather(chi_interm_1_I_merge, starts_with("diff"), key = "method", value = "value")
tmp_table$method <-  factor(tmp_table$method, levels = c("diff_GCTA_interaction", "diff_prop_interaction","diff_interaction"))

fixed_chi_interm_1_I_merge <-  tmp_table %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(structure.x, x_dist.x), ncol = 1 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("interaction effect of I Chi") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))

#####################################################################
## PCB
#####################################################################


result_list_PCB <- result_list[x_dist == "PCB" & pro == 0.8]
# remove outliners based on sub-group
result_list_PCB <- result_list_PCB[, .SD[(prop_interaction < prop_interaction[order(prop_interaction, decreasing = TRUE)[10]]) | (prop_interaction ==0),],
                                   by = c("structure", "inter_fixed_var", "interaction_m")]

result_list_PCB_inter <- result_list_PCB[true_main != 0 & interaction_m ==1, -(c(6,8))]

PCB_interm_1_un_0 <- result_list_PCB_inter[interaction_m ==1 & inter_fixed_var ==0 & inter_random_var == 0,]  %>% 
  setkey(., "true_main") %>% 
  unique(., by = "true_main")
PCB_interm_1_un_1 <- result_list_PCB_inter[interaction_m ==1 &  inter_fixed_var ==0.1 & inter_random_var == 0,] %>% 
  setkey(., "true_main") %>% 
  unique(., by = "true_main")
PCB_interm_1_un_merge <- merge(PCB_interm_1_un_0, PCB_interm_1_un_1, by = "true_main")
PCB_interm_1_un_merge[, c("diff_interaction","diff_GCTA_interaction", "diff_prop_interaction") := list(true_interaction.y - true_interaction.x, GCTA_interaction.y - GCTA_interaction.x, prop_interaction.y - prop_interaction.x)]

tmp_table <- tidyr::gather(PCB_interm_1_un_merge, starts_with("diff"), key = "method", value = "value")
tmp_table$method <-  factor(tmp_table$method, levels = c("diff_GCTA_interaction", "diff_prop_interaction","diff_interaction"))

fixed_PCB_interm_1_un_merge <-  tmp_table %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap_paginate(facets = vars(x_dist.x, pro.x), ncol = 1 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("interaction effect of un PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5))


pdf(file = "./reports/proposed_GCTA_paper/diff_interaction_h0_h1.pdf",
    width = 8,
    height = 6)

print(fixed_chi_interm_1_I_merge)
print(fixed_chi_interm_1_un_merge)

print(fixed_PCB_interm_1_un_merge)
dev.off()
