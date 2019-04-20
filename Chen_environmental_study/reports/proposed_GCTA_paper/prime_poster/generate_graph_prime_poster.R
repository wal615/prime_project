library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
library(officer)
library(rvg)


setwd("~/dev/projects/Chen_environmental_study/")
main_col_name <- c("true_main", "GCTA_main", "prop_main")
inter_col_name <- c("true_interaction", "GCTA_interaction", "prop_interaction")


##############################
## main depdent PCB
##############################

file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*main_1_inter_0_main$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_total <- result_list_fixed[true_main != 0, -c(2,4,6)] # remove inter
main_depdent_PCB <- result_list_fixed_total[pro == 0.6 & inter_fixed_var == 0, ]
main_depdent_PCB[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

notation_table <- data.frame(data_gen_model = main_depdent_PCB$data_gen_model[1],
                             est_model = main_depdent_PCB$est_model[1],
                             true_main_ave = mean(main_depdent_PCB$var_main_effect) %>% round(.,3),
                             true_inter_ave = mean(main_depdent_PCB$var_inter_effect))%>% t(.)

main_depdent_PCB_plot <- tidyr::gather(main_depdent_PCB, ends_with("main"), key = "method", value = "est_main") %>%
  ggplot(., aes(x = method, y = est_main, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Main effect estimation of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_custom(tableGrob(notation_table), xmin = 2.5, ymax = 80)


ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/main_depdent_PCB_plot.png", plot = main_depdent_PCB_plot, device = NULL, path = NULL,
       scale = 1, dpi = 1200, limitsize = TRUE)
#####################################################################
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*main_1_inter_1_main$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_total <- result_list_fixed[true_main != 0, -c(2,4,6)] # remove inter
main_depdent_PCB <- result_list_fixed_total[pro == 0.6 & inter_fixed_var == 1, ]
main_depdent_PCB[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

notation_table <- data.frame(data_gen_model = main_depdent_PCB$data_gen_model[1],
                             est_model = main_depdent_PCB$est_model[1],
                             true_main_ave = mean(main_depdent_PCB$var_main_effect) %>% round(.,3),
                             true_inter_ave = mean(main_depdent_PCB$var_inter_effect))%>% t(.)

main_depdent_PCB_plot <- tidyr::gather(main_depdent_PCB, ends_with("main"), key = "method", value = "est_main") %>%
  ggplot(., aes(x = method, y = est_main, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Main effect estimation of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_custom(tableGrob(notation_table), xmin = 2.5, xmax = 2.5, ymax =200, ymin = 200)


ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/main_depdent_PCB_inter_plot.png", plot = main_depdent_PCB_plot, device = NULL, path = NULL,
       scale = 1, dpi = 1200, limitsize = TRUE)

##############################
## total depdent PCB
##############################

file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*total_inter_std$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_total <- result_list_fixed[true_total != 0, -c(2,4,6)] # remove inter
total_depdent_PCB <- result_list_fixed_total[pro == 0.6 & inter_fixed_var == 1, ]
total_depdent_PCB[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), "total")]


notation_table <- data.frame(data_gen_model = total_depdent_PCB$data_gen_model[1],
                             est_model = total_depdent_PCB$est_model[1],
                             true_main_ave = mean(total_depdent_PCB$var_main_effect) %>% round(.,3),
                             true_inter_ave = mean(total_depdent_PCB$var_inter_effect))%>% t(.)

total_depdent_PCB_plot <- tidyr::gather(total_depdent_PCB, ends_with("total"), key = "method", value = "est_total") %>%
  ggplot(., aes(x = method, y = est_total, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Total effect estimation of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_custom(tableGrob(notation_table))

ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/total_depdent_PCB_plot.png", plot = total_depdent_PCB_plot, device = NULL, path = NULL,
       scale = 1, dpi = 1200, limitsize = TRUE)

#############################################################################################################

file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "PCB.*total_inter_std$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed_total <- result_list_fixed[true_total != 0, -c(2,4,6)] # remove inter
total_depdent_PCB <- result_list_fixed_total[pro == 0.6 & inter_fixed_var == 0, ]
total_depdent_PCB[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), "total")]


notation_table <- data.frame(data_gen_model = total_depdent_PCB$data_gen_model[1],
                             est_model = total_depdent_PCB$est_model[1],
                             true_main_ave = mean(total_depdent_PCB$var_main_effect) %>% round(.,3),
                             true_inter_ave = mean(total_depdent_PCB$var_inter_effect))%>% t(.)

total_depdent_PCB_plot <- tidyr::gather(total_depdent_PCB, ends_with("total"), key = "method", value = "est_total") %>%
  ggplot(., aes(x = method, y = est_total, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Total effect estimation of PCB") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_custom(tableGrob(notation_table))

ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/total_depdent_PCB_inter_plot.png", plot = total_depdent_PCB_plot, device = NULL, path = NULL,
       scale = 1, dpi = 1200, limitsize = TRUE)
##############################
## Normal depdent estimation
##############################

file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list <- file_list[grep(x = file_list, pattern = "normal.*un.*main_inter$",perl = TRUE)]
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)
result_list_fixed <- result_list_fixed[inter_fixed_var == 0.1, ]
result_list_fixed[,c("data_gen_model","est_model") := list(ifelse(inter_fixed_var == 0, "main","main+inter"), ifelse(interaction_m == 0, "main","main+inter"))]

result_list_fixed_main <- result_list_fixed[true_main != 0, -c(2,4,6)] # remove inter
result_list_fixed_inter <- result_list_fixed[true_main != 0, -c(1,3,5)] # remove main

notation_table <- data.frame(data_gen_model = result_list_fixed_main$data_gen_model[1],
                             est_model = result_list_fixed_main$est_model[1],
                             true_main_ave = mean(result_list_fixed_main$var_main_effect) %>% round(.,3),
                             true_inter_ave = mean(result_list_fixed_main$var_inter_effect)%>% round(.,3)) %>% t(.)

main_depdent_normal_plot <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "est_main") %>%
  ggplot(., aes(x = method, y = est_main, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Main effect estimation of normal") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_custom(tableGrob(notation_table), ymax = 16, ymin = 16, xmin =2.8, xmax = 2.8)

ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/main_normal.png", plot = main_depdent_normal_plot, device = NULL, path = NULL,
       scale = 1, dpi = 1200, limitsize = TRUE)

inter_depdent_normal_plot <- tidyr::gather(result_list_fixed_inter, ends_with("interaction"), key = "method", value = "est_inter") %>%
  ggplot(., aes(x = method, y = est_inter, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  ggtitle("Interaction effect estimation of normal") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(hjust = 0.5)) +
  annotation_custom(tableGrob(notation_table), ymax = 1.3, ymin = 1.3, xmin =2.8, xmax = 2.8)

ggsave(filename = "./reports/proposed_GCTA_paper/prime_poster/inter_depdent_normal_plot.png", plot = inter_depdent_normal_plot, device = NULL, path = NULL,
       scale = 1, dpi = 1200, limitsize = TRUE)

# read_pptx() %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(main_depdent_normal_plot), type = "body") %>% 
#   print(target = "./reports/proposed_GCTA_paper/prime_poster/main_normal.pptx")

