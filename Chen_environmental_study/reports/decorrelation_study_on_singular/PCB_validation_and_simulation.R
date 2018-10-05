# Tesing how the small sample size affect the estimation of the covariance matrix 

library(MASS)
library(sas7bdat)
library(SASxport)
library(ggplot2)
library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/reports/decorrelation_study_on_singular/")

invsqrt <- function(Sigma, tol = 1e-10) {
  ## get rid of zero egienvalues
  Seign <- eigen(Sigma)
  eign_value <- Seign$values
  eign_value_rec_sqrt <- if_else(eign_value > tol, 1/sqrt(eign_value), 0)
  Seign$vectors %*% diag(eign_value_rec_sqrt) %*% t(Seign$vectors)
}


# How the singular sample covariance affect the SVD decorrelation result
p <- 200
n_1 <- 100
Sig <- matrix(rep(0.5, 200*200), ncol = 200)
diag(Sig) <- 1
x_100 <- mvrnorm(n_1, numeric(p), Sigma = Sig)
cov_1 <- cov(x)
Est_sqrt_ins_cov_100 <- invsqrt(cov(x_100))

cov(x_100%*%Est_sqrt_ins_cov_100) [195:200, 195:200] %>% round(.,4)
cov(x_100%*%Est_sqrt_ins_cov_100) %>% abs(.) %>% sum(.)
cov(x_100%*%Est_sqrt_ins_cov_100) %>% diag(.) %>% sum(.)

n_2 <- 200
Sig <- matrix(rep(0.5, 200*200), ncol = 200)
diag(Sig) <- 1
x_200 <- mvrnorm(n_2, numeric(p), Sigma = Sig)
Est_sqrt_ins_cov_200 <- invsqrt(cov(x_200))
cov(x_200%*%Est_sqrt_ins_cov_200)[1:5,1:5] %>% round(.,4)
cov(x_200%*%Est_sqrt_ins_cov_200) %>% abs(.) %>% sum(.)
cov(x_200%*%Est_sqrt_ins_cov_200) %>% diag(.) %>% sum(.)

# if we use the inverse information of x_200
cov(x_200[1:100,]%*%Est_sqrt_ins_cov_200)[1:5,1:5] %>% round(.,4)
cov(x_200[1:100,]%*%Est_sqrt_ins_cov_200) %>% abs(.) %>% sum(.)
cov(x_200[1:100,]%*%Est_sqrt_ins_cov_200) %>% diag(.) %>% sum(.)


# How the singular covariance affect the Proposed method's performance
PCB_99_04 <- read.csv("../../R_code/data/pcb_99_04_no_missing.csv")
PCB_99_13 <- read.csv("../../R_code/data/pcb_99_13_no_missing.csv")
Sigam_04 <- cov(PCB_99_04) 
eigen_value_04 <- eigen(Sigam_04, only.values = TRUE)
Sigam_13 <- cov(PCB_99_13) 
eigen_value_13 <- eigen(Sigam_13, only.values = TRUE)
