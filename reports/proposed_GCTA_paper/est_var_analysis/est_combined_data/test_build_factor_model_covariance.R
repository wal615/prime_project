# Testing the result of the second decorrelation method
## load the dateset 
library(R.utils)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
library(Matrix)
options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
X_orignal <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual/PCB_1999_2004_common.csv", header = T, stringsAsFactors = F) 
pre_cor <- cor(X_orignal)
p <- ncol(pre_cor)
cor_h <- generate_normal(n = 10^4, p = p, pre_cor = pre_cor, structure = "un") %>% 
       add_inter(.) %>% cor(.)

X_test <- generate_normal(n = 150, p = p, pre_cor = pre_cor, structure = "un") %>% add_inter(.)   
cor(X_test) %>% offdiag(.) %>% hist(.)
cor(X_test) %>% offdiag(.) %>% summary(.)


X_decor1 <- true_value_method(input_data = X_test, emp = TRUE, combine = TRUE, sigma_total_emp = cor_h)$uncorr_data
cor_sample <- (X_decor1) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "historical decor")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.1)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.1")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.01)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.01")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.005)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.005")
cor_sample %>% offdiag(.) %>% summary(.)


#######################
set.seed(1234)
cor_h <- generate_chi(n = 10^5, p = p, pre_cor = pre_cor, structure = "un") %>% 
  add_inter(.) %>% cor(.)

X_test <- generate_chi(n = 150, p = p, pre_cor = pre_cor, structure = "un") %>% add_inter(.)   
cor(X_test) %>% offdiag(.) %>% hist(.)
cor(X_test) %>% offdiag(.) %>% summary(.)


X_decor1 <- true_value_method(input_data = X_test, emp = TRUE, combine = TRUE, sigma_total_emp = cor_h)$uncorr_data
cor_sample <- (X_decor1) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "historical decor")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.1)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.1")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.01)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.01")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.005)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.005")
cor_sample %>% offdiag(.) %>% summary(.)

## If the hist decorrelation data is missing one pice
set.seed(1234)
t <- 8
col_tran <- diag(t)
col_tran[upper.tri(col_tran)] <- 1
col_tran <- bdiag(col_tran, diag(21-t)) %>% as.matrix(.)
X_h <- generate_chi(n = 10^5, p = p, pre_cor = pre_cor, structure = "un") %*% col_tran
cor_h <- X_h %>% add_inter(.) %>% cor(.)

X_test <- generate_chi(n = 150, p = p, pre_cor = pre_cor, structure = "un") %>% add_inter(.)   
cor(X_test) %>% offdiag(.) %>% hist(.)
cor(X_test) %>% offdiag(.) %>% summary(.)


X_decor1 <- true_value_method(input_data = X_test, emp = TRUE, combine = TRUE, sigma_total_emp = cor_h)$uncorr_data
cor_sample <- (X_decor1) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "historical decor")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.1)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.1")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.01)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.01")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.005)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.005")
cor_sample %>% offdiag(.) %>% summary(.)

## If the hist decorrelation data is missing one pice
set.seed(1234)
X_h <- generate_chi(n = 10^5, p = p, pre_cor = pre_cor, structure = "un") + rnorm(p*10^5,0,sqrt(0.1)) %>% matrix(., ncol = p)
cor_h <- X_h %>% add_inter(.) %>% cor(.)

X_test <- generate_chi(n = 150, p = p, pre_cor = pre_cor, structure = "un") %>% add_inter(.)   
cor(X_test) %>% offdiag(.) %>% hist(.)
cor(X_test) %>% offdiag(.) %>% summary(.)


X_decor1 <- true_value_method(input_data = X_test, emp = TRUE, combine = TRUE, sigma_total_emp = cor_h)$uncorr_data
cor_sample <- (X_decor1) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "historical decor")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.1)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.1")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.01)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.01")
cor_sample %>% offdiag(.) %>% summary(.)

X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.005)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.005")
cor_sample %>% offdiag(.) %>% summary(.)
