library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
setwd("~/dev/projects/Chen_environmental_study/")

### fixed fixed 
file_list <- list.files("./result/simulation_proposed_GCTA_paper/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/",.)
file_list_I <- file_list[grepl(pattern = "chi_I_inter", x = file_list, fixed = TRUE)]
result_list_fixed_I <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)



















result_list_fixed <- readRDS("./result/simulation_proposed_GCTA_paper/result_list_fixed_chi_inter_1_interm_1") %>% rbindlist(.)

result_list_fixed_main <- result_list_fixed[true_main != 0, -c(2,4,6)]
result_list_fixed_inter <- result_list_fixed[true_main != 0, -c(1,3,5)]

fixed_main_inter_1_interm_1 <- tidyr::gather(result_list_fixed_main, ends_with("main"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  ggtitle("fixed_main_inter_1_interm_1") +
  theme(plot.title = element_text(hjust = 0.5))


fixed_inter_inter_1_interm_1 <- tidyr::gather(result_list_fixed_inter, ends_with("interaction"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(trans='log10') +
  ggtitle("fixed_inter_inter_1_interm_1") +
  theme(plot.title = element_text(hjust = 0.5))


pdf(file = "./reports/proposed_GCTA_paper/test_chi_indepenent.pdf",
    width = 8,
    height = 3)

print(fixed_main)
print(fixed_inter)
print(fixed_total)
dev.off()
