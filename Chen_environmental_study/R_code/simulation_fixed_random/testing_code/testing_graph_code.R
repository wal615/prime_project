library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")
load(file = "./result/simulation_result_list_fixed_fixed")

table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

main <- table_fixed_fixed[true_main != 0, -c(2,4,6)]

tidyr::gather(main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free", labeller  = "label_both")

inter <- table_fixed_fixed[true_main != 0, -c(1,3,5)]
tidyr::gather(inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, corr), ncol =3 , scales = "free")
