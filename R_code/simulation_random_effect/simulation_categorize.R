setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

cut_fn <- function(x, by) {
  breaks <- c(quantile(x, probs = seq(0, 1, by = by))) %>% unique(.)
  cut(x = x, 
      breaks = breaks,
      labels = 1:(length(breaks)-1), 
      include.lowest = TRUE) %>% 
  as.character(.) %>%
  as.numeric(.)
}

a=read.sas7bdat("~/dev/projects/Chen_environmental_study/R_code/pcbs1000nomiss.sas7bdat")
a=data.matrix(a[,2:35], rownames.force = NA)
b <- a 
### cut with 2 interval
#b_cut_2 <- apply(b,2,FUN = function(x) cut(x,2, 1:2) %>% as.character(.) %>% as.numeric)
#b_cut_2 <- std_fn(b_cut_2, ncol(b_cut_2), tran_FUN = null_tran)
#
### cut with 5 interval
#b_cut_5 <- apply(b,2,FUN = function(x) cut(x,5, 1:5) %>% as.character(.) %>% as.numeric)
#b_cut_5 <- std_fn(b_cut_5, ncol(b_cut_5), tran_FUN = null_tran)
#
#data_list_cut <- list(b_cut_2 = b_cut_2,
#                  b_cut_5 = b_cut_5)
#
#interaction_list <- list(1,1)
#interaction_m_list <- list(1,1)
#
#
#result_list <- mapply(FUN = compare_corr_GCTA,
#                      b = data_list_cut,
#                      interaction = interaction_list,
#                      interaction_m = interaction_m_list,
#                      MoreArgs = list(brep = 80, nrep = 20, seed = 123, cores = 2),
#                      SIMPLIFY = FALSE)
#
#save(result_list, file = "./result/simulation_cut_2_5")




## cut with quantile method

b_quantile_2 <- apply(b,2, FUN = cut_fn, by = 0.5)
b_quantile_2 <- std_fn(b_quantile_2, ncol(b_quantile_2), tran_FUN = null_tran)

b_quantile_5 <- apply(b,2, FUN = cut_fn, by = 0.2)
b_quantile_5 <- std_fn(b_quantile_5, ncol(b_quantile_5), tran_FUN = null_tran)

b_quantile_10 <- apply(b,2, FUN = cut_fn, by = 0.1)
b_quantile_10 <- std_fn(b_quantile_10, ncol(b_quantile_10), tran_FUN = null_tran)

data_list_quantile <- list(b_quantile_2 = b_quantile_2,
                      b_quantile_5 = b_quantile_5,
                      b_quantile_10 = b_quantile_10)

interaction_list <- list(1,1,1)
interaction_m_list <- list(1,1,1)



result_list <- mapply(FUN = compare_corr_GCTA,
                      b = data_list_quantile,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 80, nrep = 20, seed = 123, cores = 3),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_quantile_2_5_10")
