setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")
source("./R_code/simulation_fixed_random/chi_square_fixed_random_simulation_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)


n <- 1000
p <- 34
gene_args <- data.frame(n =n, p = p, rho = seq(0.1,0.9,0.1), combine = TRUE, chi_coef = 1)
gene_args <- gene_args %>% split(x = ., f = seq(nrow(gene_args))) # generate a list from each row of a dataframe

result_list_fixed_fixed <- mapply(FUN = simulation_fn,
                                  gene_args = gene_args,
                                  combine = TRUE,
                                  MoreArgs = list(n = n,
                                                  uncorr_method = SVD_method,
                                                  tran_fun = null_tran,
                                                  p = p,
                                                  main_fixed = TRUE,
                                                  inter_fixed = TRUE,
                                                  generate_data = generate_chi,
                                                  brep = 200,
                                                  nrep = 20,
                                                  seed = 123,
                                                  cores = 10,
                                                  interaction = 1,
                                                  interaction_m = 0),
                                  SIMPLIFY = FALSE)
table_fixed_fixed <- rbindlist(result_list_fixed_fixed)

total <- table_fixed_fixed[true_total != 0, -c(2,4,6)]
plot_chi_fixed_fixed_total_combine_df_10 <- tidyr::gather(total, ends_with("total"), key = "method", value = "value") %>%
  ggplot(., aes(x = method, y = value, fill = method)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(facets = vars(main_fixed, inter_fixed, rho), ncol =3 , scales = "free", labeller  = "label_both")
