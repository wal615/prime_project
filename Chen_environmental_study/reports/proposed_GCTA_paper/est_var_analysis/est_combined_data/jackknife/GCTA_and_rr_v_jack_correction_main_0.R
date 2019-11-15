library(sas7bdat)
library(R.utils)
library(MASS)
library(tidyverse)
library(ggplot2)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")

nor_50 <- read.csv("./result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr/decor_None_sparse_None_normal_structure_I_main_0_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_1_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/rho_e_0.5_n_50_pro_1012_iteration_1.csv",
                   stringsAsFactors = F)
n <- nor_50[1,"n"]
point_est_GCTA <- nor_50[1, "GCTA_main"]
v_jack_GCTA <- jack_var(x = nor_50[1:n, "sub_GCTA_main"], pro = 101)
v_jack_corr_GCTA <- v_jact_correct(S = nor_50[1,"GCTA_main"], n = n, 
                                   i_1 = nor_50[,"i_1"], i_2 = nor_50[,"i_2"], 
                                   S_i = nor_50[,"sub_GCTA_main"],
                                   Q = T)

point_est_GCTA_rr <- nor_50[1, "GCTA_rr_main"]
v_jack_GCTA_rr <- jack_var(x = nor_50[1:n, "sub_GCTA_rr_main"], pro = 101)
v_jack_corr_GCTA_rr <- v_jact_correct(S = nor_50[1,"GCTA_rr_main"], n = n, 
                                      i_1 = nor_50[,"i_1"], i_2 = nor_50[,"i_2"], 
                                      S_i = nor_50[,"sub_GCTA_rr_main"],
                                      Q = T)

res_50_GCTA <- data.frame(main_effect_GCTA = point_est_GCTA, v_jack = v_jack_GCTA, v_jack_corr = v_jack_corr_GCTA[2], row.names = "GCTA")
res_50_GCTA_rr<- data.frame(main_effect_GCTA = point_est_GCTA_rr, v_jack = v_jack_GCTA_rr, v_jack_corr = v_jack_corr_GCTA_rr[2], row.names = "GCTA_rr")
res_50 <- rbind(res_50_GCTA, res_50_GCTA_rr)

h1 <- data.frame(Qii = attr(v_jack_corr_GCTA, "Q"), name = "GCTA") 
h2 <- data.frame(Qii = attr(v_jack_corr_GCTA_rr, "Q"), name = "GCTA_rr")
h <- rbind(h1,h2)
h_50 <- ggplot(h, aes(x=Qii, color=name, fill = name)) +
  geom_histogram(alpha=0.5, bins = 40) +
  ggtitle("hist of Q_ii") + 
  theme(plot.title = element_text(lineheight=3, face="bold"))

## 75
nor_75 <- read.csv("./result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr/decor_None_sparse_None_normal_structure_I_main_0_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_1_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/rho_e_0.5_n_75_pro_1012_iteration_1.csv",
                   stringsAsFactors = F)
n <- nor_75[1,"n"]
point_est_GCTA <- nor_75[1, "GCTA_main"]
v_jack_GCTA <- jack_var(x = nor_75[1:n, "sub_GCTA_main"], pro = 101)
v_jack_corr_GCTA <- v_jact_correct(S = nor_75[1,"GCTA_main"], n = n, 
                                   i_1 = nor_75[,"i_1"], i_2 = nor_75[,"i_2"], 
                                   S_i = nor_75[,"sub_GCTA_main"],
                                   Q = T)

point_est_GCTA_rr <- nor_75[1, "GCTA_rr_main"]
v_jack_GCTA_rr <- jack_var(x = nor_75[1:n, "sub_GCTA_rr_main"], pro = 101)
v_jack_corr_GCTA_rr <- v_jact_correct(S = nor_75[1,"GCTA_rr_main"], n = n, 
                                      i_1 = nor_75[,"i_1"], i_2 = nor_75[,"i_2"], 
                                      S_i = nor_75[,"sub_GCTA_rr_main"],
                                      Q = T)

res_75_GCTA <- data.frame(main_effect_GCTA = point_est_GCTA, v_jack = v_jack_GCTA, v_jack_corr = v_jack_corr_GCTA[2], row.names = "GCTA")
res_75_GCTA_rr<- data.frame(main_effect_GCTA = point_est_GCTA_rr, v_jack = v_jack_GCTA_rr, v_jack_corr = v_jack_corr_GCTA_rr[2], row.names = "GCTA_rr")
res_75 <- rbind(res_75_GCTA, res_75_GCTA_rr)

h1 <- data.frame(Qii = attr(v_jack_corr_GCTA, "Q"), name = "GCTA") 
h2 <- data.frame(Qii = attr(v_jack_corr_GCTA_rr, "Q"), name = "GCTA_rr")
h <- rbind(h1,h2)
h_75 <- ggplot(h, aes(x=Qii, color=name, fill = name)) +
  geom_histogram(alpha=0.5, bins = 40) +
  ggtitle("hist of Q_ii") + 
  theme(plot.title = element_text(lineheight=3, face="bold"))

## 100
nor_100 <- read.csv("./result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr/decor_None_sparse_None_normal_structure_I_main_0_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_1_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/rho_e_0.5_n_100_pro_1012_iteration_1.csv",
                    stringsAsFactors = F)
n <- nor_100[1,"n"]
point_est_GCTA <- nor_100[1, "GCTA_main"]
v_jack_GCTA <- jack_var(x = nor_100[1:n, "sub_GCTA_main"], pro = 101)
v_jack_corr_GCTA <- v_jact_correct(S = nor_100[1,"GCTA_main"], n = n, 
                                   i_1 = nor_100[,"i_1"], i_2 = nor_100[,"i_2"], 
                                   S_i = nor_100[,"sub_GCTA_main"], 
                                   Q = T)

point_est_GCTA_rr <- nor_100[1, "GCTA_rr_main"]
v_jack_GCTA_rr <- jack_var(x = nor_100[1:n, "sub_GCTA_rr_main"], pro = 101)
v_jack_corr_GCTA_rr <- v_jact_correct(S = nor_100[1,"GCTA_rr_main"], n = n, 
                                      i_1 = nor_100[,"i_1"], i_2 = nor_100[,"i_2"], 
                                      S_i = nor_100[,"sub_GCTA_rr_main"], 
                                      Q = T)

res_100_GCTA <- data.frame(main_effect_GCTA = point_est_GCTA, v_jack = v_jack_GCTA, v_jack_corr = v_jack_corr_GCTA[2], row.names = "GCTA")
res_100_GCTA_rr<- data.frame(main_effect_GCTA = point_est_GCTA_rr, v_jack = v_jack_GCTA_rr, v_jack_corr = v_jack_corr_GCTA_rr[2], row.names = "GCTA_rr")
res_100 <- rbind(res_100_GCTA, res_100_GCTA_rr)

h1 <- data.frame(Qii = attr(v_jack_corr_GCTA, "Q"), name = "GCTA") 
h2 <- data.frame(Qii = attr(v_jack_corr_GCTA_rr, "Q"), name = "GCTA_rr")
h <- rbind(h1,h2)
h_100 <- ggplot(h, aes(x=Qii, color=name, fill = name)) +
  geom_histogram(alpha=0.5, bins = 40) + 
  ggtitle("hist of Q_ii") + 
  theme(plot.title = element_text(lineheight=3, face="bold"))


## 150
nor_150 <- read.csv("./result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr/decor_None_sparse_None_normal_structure_I_main_0_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_1_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/rho_e_0.5_n_150_pro_1012_iteration_1.csv",
                    stringsAsFactors = F)
n <- nor_150[1,"n"]
point_est_GCTA <- nor_150[1, "GCTA_main"]
v_jack_GCTA <- jack_var(x = nor_150[1:n, "sub_GCTA_main"], pro = 101)
v_jack_corr_GCTA <- v_jact_correct(S = nor_150[1,"GCTA_main"], n = n, 
                                   i_1 = nor_150[,"i_1"], i_2 = nor_150[,"i_2"], 
                                   S_i = nor_150[,"sub_GCTA_main"], 
                                   Q = T)

point_est_GCTA_rr <- nor_150[1, "GCTA_rr_main"]
v_jack_GCTA_rr <- jack_var(x = nor_150[1:n, "sub_GCTA_rr_main"], pro = 101)
v_jack_corr_GCTA_rr <- v_jact_correct(S = nor_150[1,"GCTA_rr_main"], n = n, 
                                      i_1 = nor_150[,"i_1"], i_2 = nor_150[,"i_2"], 
                                      S_i = nor_150[,"sub_GCTA_rr_main"], 
                                      Q = T)

res_150_GCTA <- data.frame(main_effect_GCTA = point_est_GCTA, v_jack = v_jack_GCTA, v_jack_corr = v_jack_corr_GCTA[2], row.names = "GCTA")
res_150_GCTA_rr<- data.frame(main_effect_GCTA = point_est_GCTA_rr, v_jack = v_jack_GCTA_rr, v_jack_corr = v_jack_corr_GCTA_rr[2], row.names = "GCTA_rr")
res_150 <- rbind(res_150_GCTA, res_150_GCTA_rr)

h1 <- data.frame(Qii = attr(v_jack_corr_GCTA, "Q"), name = "GCTA") 
h2 <- data.frame(Qii = attr(v_jack_corr_GCTA_rr, "Q"), name = "GCTA_rr")
h <- rbind(h1,h2)
h_150 <- ggplot(h, aes(x=Qii, color=name, fill = name)) +
  geom_histogram(alpha=0.5, bins = 40) + 
  ggtitle("hist of Q_ii") + 
  theme(plot.title = element_text(lineheight=3, face="bold"))

## 200
nor_200 <- read.csv("./result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr/decor_None_sparse_None_normal_structure_I_main_0_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_1_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null/rho_e_0.5_n_200_pro_1012_iteration_1.csv",
                    stringsAsFactors = F)
n <- nor_200[1,"n"]
point_est_GCTA <- nor_200[1, "GCTA_main"]
v_jack_GCTA <- jack_var(x = nor_200[1:n, "sub_GCTA_main"], pro = 101)
v_jack_corr_GCTA <- v_jact_correct(S = nor_200[1,"GCTA_main"], n = n, 
                                   i_1 = nor_200[,"i_1"], i_2 = nor_200[,"i_2"], 
                                   S_i = nor_200[,"sub_GCTA_main"], 
                                   Q = T)

point_est_GCTA_rr <- nor_200[1, "GCTA_rr_main"]
v_jack_GCTA_rr <- jack_var(x = nor_200[1:n, "sub_GCTA_rr_main"], pro = 101)
v_jack_corr_GCTA_rr <- v_jact_correct(S = nor_200[1,"GCTA_rr_main"], n = n, 
                                      i_1 = nor_200[,"i_1"], i_2 = nor_200[,"i_2"], 
                                      S_i = nor_200[,"sub_GCTA_rr_main"],
                                      Q = T)

res_200_GCTA <- data.frame(main_effect_GCTA = point_est_GCTA, v_jack = v_jack_GCTA, v_jack_corr = v_jack_corr_GCTA[2], row.names = "GCTA")
res_200_GCTA_rr<- data.frame(main_effect_GCTA = point_est_GCTA_rr, v_jack = v_jack_GCTA_rr, v_jack_corr = v_jack_corr_GCTA_rr[2], row.names = "GCTA_rr")
res_200 <- rbind(res_200_GCTA, res_200_GCTA_rr)

h1 <- data.frame(Qii = attr(v_jack_corr_GCTA, "Q"), name = "GCTA") 
h2 <- data.frame(Qii = attr(v_jack_corr_GCTA_rr, "Q"), name = "GCTA_rr")
h <- rbind(h1,h2)
h_200 <- ggplot(h, aes(x=Qii, color=name, fill = name)) +
  geom_histogram(alpha=0.5, bins = 40) + 
  ggtitle("hist of Q_ii") + 
  theme(plot.title = element_text(lineheight=3, face="bold"))
