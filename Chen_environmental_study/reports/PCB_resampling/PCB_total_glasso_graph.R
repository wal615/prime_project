library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
load(file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_total_glasso")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed_total_glasso)

total_glasso <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_PCB_fixed_fixed_total_glasso <- as.list(numeric(3))
for (i in (1:3)){
  plot_PCB_fixed_fixed_total_glasso[[i]] <- tidyr::gather(total_glasso, ends_with("total"), key = "method", value = "value") %>%
               ggplot(., aes(x = method, y = value, fill = method)) +
               geom_violin(alpha = 0.2) +
               geom_boxplot(alpha = 0.7) +
               facet_wrap_paginate(facets = vars(main_fixed, inter_fixed, pro, rho), 
                          ncol = 5 , nrow = 3, scales = "free", labeller  = "label_both", page = i)+
               ggtitle("Total effect with glasso under main fixed and inter fixed") +
               theme(plot.title = element_text(hjust = 0.5))
}

## generating graph for the fixed_fixed simuation
load(file = "./result/PCB_resampling/simulation_result_list_fixed_fixed_total_svd")
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_PCB_fixed_fixed_total_svd <- tidyr::gather(main, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, pro), ncol =3 , scales = "free", labeller  = "label_both")+
  ggtitle("Total effect with svd fixed main fixed and inter fixed") +
  theme(plot.title = element_text(hjust = 0.5))

# ### fixed random 
# load(file = "./result/PCB_resampling/simulation_result_list_fixed_random_total_glasso")
# table_fixed_random <- rbindlist(result_list_fixed_random_total_glasso)
# 
# total_glasso <- table_fixed_random[true_total != 0, -c(2,4,6)]
# plot_PCB_fixed_random_total_glasso <- as.list(numeric(3))
# for (i in (1:3)){
#   plot_PCB_fixed_random_total_glasso[[i]] <- tidyr::gather(total_glasso, ends_with("total"), key = "method", value = "value") %>%
#     ggplot(., aes(x = method, y = value, fill = method)) +
#     geom_violin(alpha = 0.2) +
#     geom_boxplot(alpha = 0.7) +
#     facet_wrap_paginate(facets = vars(main_fixed, inter_fixed, pro, rho), 
#                         ncol = 5 , nrow = 3, scales = "free", labeller  = "label_both", page = i)+
#     ggtitle("Total effect with glasso under main fixed and inter random") +
#     theme(plot.title = element_text(hjust = 0.5))
# }
# 
# ### random random 
# load(file = "./result/PCB_resampling/simulation_result_list_random_random_total_glasso")
# table_random_random <- rbindlist(result_list_random_random_total_glasso)
# 
# total_glasso <- table_random_random[true_total != 0, -c(2,4,6)]
# plot_PCB_random_random_total_glasso <- as.list(numeric(3))
# for (i in (1:3)){
#   plot_PCB_random_random_total_glasso[[i]] <- tidyr::gather(total_glasso, ends_with("total"), key = "method", value = "value") %>%
#     ggplot(., aes(x = method, y = value, fill = method)) +
#     geom_violin(alpha = 0.2) +
#     geom_boxplot(alpha = 0.7) +
#     facet_wrap_paginate(facets = vars(main_fixed, inter_fixed, pro, rho), 
#                         ncol = 5 , nrow = 3, scales = "free", labeller  = "label_both", page = i)+
#     ggtitle("Total effect with glasso under main fixed and inter fixed") +
#     theme(plot.title = element_text(hjust = 0.5))
# }
# 
# 
