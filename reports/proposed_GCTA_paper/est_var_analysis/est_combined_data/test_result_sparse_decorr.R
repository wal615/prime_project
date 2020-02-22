# Testing the result of the second decorrelation method
## load the dateset 
library(R.utils)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
X_orignal <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual/PCB_1999_2004_common.csv", header = T, stringsAsFactors = F) 
X_total <- X_orignal %>% std_fn(.) %>% add_inter(.)
cov_h <- cov(X_total)
set.seed(1234)
par(mfrow=c(1,1))
X_sample <- X_orignal[sample(1:nrow(X_total), 150, replace = F),] %>% std_fn(.) %>% add_inter(.)
cor(X_sample) %>% offdiag(.) %>% hist(., nclass = 40, main = "Histogram of correlations of PCBs with sample size 150")

X_decor1 <- X_sample %*% invsqrt(cov_h)
cor_sample <- (X_decor1) %>% cor(.)
par(mfrow=c(2,2))
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "historical decor")
cor_sample %>% offdiag(.) %>% summary(.)

# adding sparse covariance eistmation
X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.1)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.1")
cor_sample %>% offdiag(.) %>% summary(.)

# adding sparse covariance eistmation
X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.01)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.01")
cor_sample %>% offdiag(.) %>% summary(.)

# adding sparse covariance eistmation
X_decor2 <- dgpGLASSO_method(X_decor1, rho = 0.005)$uncorr_data
cor_sample <- (X_decor2) %>% cor(.)
cor_sample %>% offdiag(.) %>% hist(., nclass = 40, main = "sparse decor 0.005")
cor_sample %>% offdiag(.) %>% summary(.)


