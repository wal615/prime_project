# testing the linearity of EigenPrism 
options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn",modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/test_EigenPrism/test_EigenPrism_helper.R")
save_path <- "~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/test_EigenPrism/"
library(data.table)
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns

p <- 1000
n <- 500
set.seed(1234)
X_1 <- generate_normal(n = n, p = p, structure = "I")
X_2 <- generate_normal(n = n, p = p, structure = "I")
beta <- rep(c(1,0), 500)
Y_1 <- X_1 %*% beta + rnorm(n)
Y_2 <- X_2 %*% beta + rnorm(n)
X_3 <- X_1 + X_2
Y_3 <- Y_1 + Y_2

fit_1 <- EigenPrism(y = Y_1, X = X_1)
fit_2 <- EigenPrism(y = Y_2, X = X_2)
fit_3 <- EigenPrism(y = Y_3, X = X_3)
