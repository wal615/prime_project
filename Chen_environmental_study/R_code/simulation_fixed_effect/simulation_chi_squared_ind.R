# Following simulation is to test if under normal distribution, we can estimate the interaction effect unbaisly by proposed method

setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)

mu <- rep(0,34)
n <- 1000
Sigma <- diag(34)
b_norm_ind <- mvrnorm(n = n,
                      Sigma = Sigma,
                      mu = mu)
b_chi_ind <- b_norm_ind^2

additional <- list(dist = "Chi_ind", tran = "null")
b_chi_ind_null <- std_fn(b = b_chi_ind,
                         p = 34,
                         additional = additional) %>% list(.)

names(b_chi_ind_null) <- paste0(names(b_chi_ind_null), "_null")

interaction_list <- as.list(rep(1,length(b_chi_ind_null)))
interaction_m_list <- as.list(rep(1,length(b_chi_ind_null)))

result_list <- mapply(FUN = compare_corr_GCTA,
                      b = b_chi_ind_null,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 3, nrep = 100, seed = 123, cores = 3, 
                                      interm_result = TRUE, 
                                      interm_result_path = "~/dev/projects/Chen_environmental_study/result/inter_result/chi_ind/"),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_chi_ind_null")

