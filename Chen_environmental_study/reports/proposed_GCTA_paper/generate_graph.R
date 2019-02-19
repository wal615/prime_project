library(data.table)
library(tidyverse)
library(ggforce)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
## generating graph for the fixed_fixed simuation chi n 100-500 p_34
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>% 
  paste0("./result/simulation_proposed_GCTA_paper/",.)
result_list_fixed <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.) 

result_list_fixed_main <- result_list_fixed[true_main != 0, -c(2,4,6)]
result_list_fixed_inter <- result_list_fixed[true_main != 0, -c(1,3,5)]

fixed_main <- tidyr::gather(result_list_fixed_main[structure == "I" & inter_fixed_var != 0 & x_dist == "Normal"], ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(n, structure, inter_fixed_var, x_dist), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Fixed_main_test") +
  theme(plot.title = element_text(hjust = 0.5))


fixed_inter <- tidyr::gather(result_list_fixed_inter[structure == "I" & inter_fixed_var != 0 & x_dist == "Normal"], ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(n, structure, inter_fixed_var, x_dist), ncol = 3 , scales = "free", labeller  = "label_both")+
  ggtitle("Fixed_inter_test") +
  theme(plot.title = element_text(hjust = 0.5))
