## select the lambda for covariance estimation
library(sas7bdat)
library(R.utils)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(glasso)
library(dpglasso)
library(QUIC)
library(gtools) # for rbind based on columns
options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/pcb_99_13_no_missing.csv"
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/decor/"
p <- 800
n <- 500
pre_cov <- unstr_corr.mat(p,k=5)
pre_cor <- cov2cor(pre_cov)
icor <- inv(pre_cor)
insqrtcor <- invsqrt(pre_cor)
tol <- 0.05

for(j in c(1,0.1,0.05,0.01)){
  for(i in 1:10){
    x <- generate_normal(n = n, p = p, structure = "un",pre_cor = pre_cor) %>% std_fn(.)
    r_1 <- r_1 + mdist(inv(cov(x)), icor)
    r_2 <- r_2 + mdist(inv(glasso(cov(x), rho = j, thr = tol)$w), icor)
    r_3 <- r_3 + mdist(dpglasso(cov(x),rho = j, outer.tol = tol)$X, icor)
    r_4 <- r_4 + mdist(QUIC(cov(x),rho = j, tol = tol)$X, icor)
    cat(i,j)
  }
}

result <- cbind(r_1,r_2,r_3,r_4, n = n)
write.csv(result, file = "./R_code/simulation_proposed_GCTA/test_decor_method/result_normal.csv")
