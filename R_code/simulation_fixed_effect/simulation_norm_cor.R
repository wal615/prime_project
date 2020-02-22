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
p <- seq(0.1,0.9,0.1)
Sigma_str <- as.list(numeric(9))
names(Sigma_str) <- paste0("correlation_", p)

for(i in (1:length(Sigma_str))){
  cor_str <- matrix(rep(p[i],34^2), ncol = 34)
  diag(cor_str) <- 1
  Sigma_str[[i]] <- cor_str
}

b_norm_corr <- mapply(FUN = mvrnorm,
                      Sigma = Sigma_str,
                      MoreArgs = list(n = n, mu = mu),
                      SIMPLIFY = FALSE)


addtional <- lapply(p, FUN = function(x) list(cor = x, tran = "null"))

b_norm_corr_null <- mapply(FUN = std_fn,
                           b = b_norm_corr,
                           additional = addtional,
                           MoreArgs = list(p = 34, 
                                           tran_FUN = null_tran),
                           SIMPLIFY = FALSE)

names(b_norm_corr_null) <- paste0(names(b_norm_corr_null), "_null")

interaction_list <- as.list(rep(1,length(b_norm_corr_null)))
interaction_m_list <- as.list(rep(1,length(b_norm_corr_null)))

result_list <- mapply(FUN = compare_corr_GCTA,
                      b = b_norm_corr_null,
                      interaction = interaction_list,
                      interaction_m = interaction_m_list,
                      MoreArgs = list(brep = 3, nrep = 100, seed = 123, cores = 3, 
                                      interm_result = TRUE, 
                                      interm_result_path = "~/dev/projects/Chen_environmental_study/result/inter_result/norm_cor/"),
                      SIMPLIFY = FALSE)

save(result_list, file = "./result/simulation_norm_core_null")

