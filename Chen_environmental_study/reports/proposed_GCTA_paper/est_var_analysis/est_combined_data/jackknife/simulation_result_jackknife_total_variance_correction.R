## Simulation results summary 
library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}

upper <- 0.9
lower <- 0.1

z_p <-qnorm(lower, lower.tail = F)
CI_length <- function(x, upper, lower){
  CI_precentile <- quantile(x, c(lower, upper),na.rm = T)
  CI_precentile[2] - CI_precentile[1]
}
coverage_rate_emp <- function(x,true, upper, lower){
  CI1 <- quantile(x, lower ,na.rm = T)
  CI2 <- quantile(x, upper, na.rm = T)
  return(true >= CI1 & true <= CI2)
}
coverage_rate_z <- function(x,true, upper, lower){
  z_p <-qnorm(lower, lower.tail = F)
  CI1 <- mean(x,rm.na = T) - sd(x, rm.na = T)*z_p
  CI2 <- mean(x,rm.na = T) + sd(x, rm.na = T)*z_p
  return(true >= CI1 & true <= CI2)
}

jack_var <- function(x, pro = 0.5){
  x_m <- mean(x, na.rm = T)
  n_sub <- length(x) - sum(is.na(x)) # count for the NA data
  var_1 <- (x_m - x)^2 %>% sum(., na.rm = T)
  if(pro == 101){
    var_2 <-  (length(x) -1) * 1/n_sub * var_1
  } else if (pro ==102){
    var_2 <- 1/n_sub * var_1
  } else{
    var_2 <- pro/(1-pro) * 1/n_sub * var_1
  }
  var_2
}


sub_CI_lenght <- function(x, pro = 0.5, z_p){
  sd <- sqrt(jack_var(x, pro))
  2 * sd *z_p
}

sub_coverage_rate_z <- function(x,true, upper, lower,pro){
  z_p <-qnorm(lower, lower.tail = F)
  sd <- jack_var(x = x,pro = pro) %>% sqrt(.)
  CI1 <- mean(x,rm.na = T) - sd*z_p
  CI2 <- mean(x,rm.na = T) + sd*z_p
  return(true >= CI1 & true <= CI2)
}


v_jact_correct <- function(S, n, i_1, i_2, S_i) {
  S <- unique(S)
  n <- unique(n)
  S_1 <- S_i[1:n]
  i_10 <- i_1[1:n]
  S_2 <- S_i[-(1:n)]
  i_12 <- cbind(i_1[-(1:n)], i_2[-(1:n)])
  
  # Q is for correct the sencond order bias of jackknife variance by Eforn 1981
  Q_jj <- numeric(nrow(i_12))
  for(j in 1:nrow(i_12)){
    j_1 <- i_12[j, 1]
    j_2 <- i_12[j, 2]
    Q_jj[j] <- n*S - (n-1)*(S_1[j_1] + S_1[j_2]) + (n-2)*S_2[j]
  }
  # jackknife variance 
  v_jack <- jack_var(S_1, pro = 101)
  # jackknife bias correction
  v_jack_corr <- v_jack - ssd(Q_jj)/(n*(n+1))
  data.frame(v_jack = v_jack, v_jack_corr = v_jack_corr)
}

ssd <- function(x){
  sum((x - mean(x))^2) 
}

z_0.8 <- -qnorm(0.1)
#############################################################################################################################

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_jackknife_reports_09_25_2019/")
result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0.1_n_100_150_231_500_p_21_rho_e_0.5_decor_FALSE_subpro_101_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[sub_EigenPrism_total == 999, c("sub_EigenPrism_total", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_total, na.rm = T),
                                    NA_i = mean(is.na(EigenPrism_total)),
                                    var_total_effect = var_total_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_total_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_total = sum(NA_i)), by = n]


sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_total, na.rm = TRUE),
                                          var_jack = jack_var(sub_EigenPrism_total, pro = pro),
                                          sub_z_length = sub_CI_lenght(sub_EigenPrism_total, pro = pro,z_p = z_p),
                                          sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_total, mean(var_total_effect), upper = upper, lower = lower, pro = pro)),
                                      by = .(n,rho_e, p,i,pro)]
sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_total, na.rm = T),
                                      NA_i = mean(is.na(GCTA_total)),
                                      var_total_effect = var_total_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_total_effect))^2, na.rm = T),
                                                                                                est_var = var(est_mean, na.rm = T),
                                                                                                est_mean = mean(est_mean, na.rm = T),
                                                                                                NA_total = sum(NA_i)), by = n]

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_total, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_total, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_total, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_total, mean(var_total_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final_none_21 <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)


# 22 
result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0.1_n_100_253_500_600_700_p_22_rho_e_0.5_decor_FALSE_subpro_101_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[sub_EigenPrism_total == 999, c("sub_EigenPrism_total", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_total, na.rm = T),
                                    NA_i = mean(is.na(EigenPrism_total)),
                                    var_total_effect = var_total_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_total_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_total = sum(NA_i)), by = n]


sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_total, na.rm = TRUE),
                                          var_jack = jack_var(sub_EigenPrism_total, pro = pro),
                                          sub_z_length = sub_CI_lenght(sub_EigenPrism_total, pro = pro,z_p = z_p),
                                          sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_total, mean(var_total_effect), upper = upper, lower = lower, pro = pro)),
                                      by = .(n,rho_e, p,i,pro)]
sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_total, na.rm = T),
                                      NA_i = mean(is.na(GCTA_total)),
                                      var_total_effect = var_total_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_total_effect))^2, na.rm = T),
                                                                                                est_var = var(est_mean, na.rm = T),
                                                                                                est_mean = mean(est_mean, na.rm = T),
                                                                                                NA_total = sum(NA_i)), by = n]

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_total, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_total, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_total, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_total, mean(var_total_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final_none_22 <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)

######################################################

result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0.1_n_100_231_p_21_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_normal_total <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                    by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(GCTA_total, na.rm = T),
                                    est_jack = mean(GCTA_mean_jack),
                                    var = var(GCTA_total, na.rm = T),
                                    v_jack = mean(GCTA_var_jack),
                                    v_jack_c = mean(GCTA_var_jack_corr)),
                                    by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]
summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_normal_total <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)

###########################################################################################################################
result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_chi_structure_I_main_0.5_inter_0.1_n_100_231_p_21_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_chi_total <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]
summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_chi_total <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)


###########################################################################################################################
result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_chi_structure_I_main_0.5_inter_0.1_n_100_150_325_p_25_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_chi_total_25 <- sub_result[1,.(var_main_effect,
                                       var_inter_effect,
                                       cov_main_inter_effect,
                                       var_total_effect,
                                       structure,
                                       decor,
                                       x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_chi_total_25 <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)


######################################################

result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_100_231_p_21_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional_normal_main <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_main - mean(var_main_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_main, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_main, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_main - mean(var_main_effect))^2, na.rm = T),
                                      est = mean(GCTA_main, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_main, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_normal_main <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)


######################################################

result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_50_100_231_p_21_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional_normal_main_rr <- sub_result[1,.(var_main_effect,
                                         var_inter_effect,
                                         cov_main_inter_effect,
                                         var_total_effect,
                                         structure,
                                         decor,
                                         x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_main - mean(var_main_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_main, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_main, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_main - mean(var_main_effect))^2, na.rm = T),
                                      est = mean(GCTA_main, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_main, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_normal_main_rr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)

######################################################

result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_50_100_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional_normal_main_100 <- sub_result[1,.(var_main_effect,
                                         var_inter_effect,
                                         cov_main_inter_effect,
                                         var_total_effect,
                                         structure,
                                         decor,
                                         x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_main - mean(var_main_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_main, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_main, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2, na.rm = T)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_main - mean(var_main_effect))^2, na.rm = T),
                                      est = mean(GCTA_main, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_main, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_normal_main_100 <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE) %>% setorder(., n)


######################################################

result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_50_100_200_p_100_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)

additional_normal_main_100_rr <- sub_result[1,.(var_main_effect,
                                             var_inter_effect,
                                             cov_main_inter_effect,
                                             var_total_effect,
                                             structure,
                                             decor,
                                             x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_main - mean(var_main_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_main, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_main, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2, na.rm = T)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_main - mean(var_main_effect))^2, na.rm = T),
                                      est = mean(GCTA_main, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_main, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_normal_main_100_rr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE) %>% setorder(., n)




######################################################

result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_100_231_p_21_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_chi_main <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_main == 999, c("EigenPrism_main", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_main - mean(var_main_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_main, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_main, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_main - mean(var_main_effect))^2, na.rm = T),
                                      est = mean(GCTA_main, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_main, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_chi_main <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)


##########################################################################################################
result_path <- "decor_None_sparse_None_grho_0.1_PCB_structure_un_main_0.5_inter_0.1_n_100_p_21_rho_e_0.5_decor_FALSE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_year_1999_std_PCB_FALSE_c_betam_8_c_betai_2"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_PCB_total <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr,na.rm = T),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_PCB_total <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)
###############################################################
result_path <- "decor_hist_sparse_Glasso_grho_0.1_PCB_structure_un_main_0.5_inter_0.1_n_100_p_21_rho_e_0.5_decor_TRUE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_year_1999_std_PCB_FALSE_c_betam_8_c_betai_2"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_PCB_total_corr <- sub_result[1,.(var_main_effect,
                                       var_inter_effect,
                                       cov_main_inter_effect,
                                       var_total_effect,
                                       structure,
                                       decor,
                                       x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr,na.rm = T),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_PCB_total_corr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)

###############################################################
result_path <- "decor_hist_sparse_Glasso_grho_0.1_PCB_structure_un_main_0.5_inter_0.1_n_100_p_21_rho_e_0.5_decor_TRUE_subpro_1012_iter_10_nsub_0_EigenPrism_kernel_GCTA_rr_kernel_est_total_year_1999_std_PCB_original_c_betam_8_c_betai_2"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_PCB_total_corr_rr <- sub_result[1,.(var_main_effect,
                                            var_inter_effect,
                                            cov_main_inter_effect,
                                            var_total_effect,
                                            structure,
                                            decor,
                                            x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr,na.rm = T),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_PCB_total_corr_rr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)



###############################################################
result_path <- "decor_hist_sparse_Glasso_grho_0.1_PCB_structure_un_main_0.5_inter_0.1_n_100_p_21_rho_e_0.5_decor_TRUE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_year_1999_std_PCB_rank_each_c_betam_8_c_betai_2"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_PCB_rank_each_total <- sub_result[1,.(var_main_effect,
                                       var_inter_effect,
                                       cov_main_inter_effect,
                                       var_total_effect,
                                       structure,
                                       decor,
                                       x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr,na.rm = T),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_PCB_rank_each_total_corr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)

##############################################################################################################################################


result_path <- "decor_hist_sparse_Glasso_grho_0.1_PCB_structure_un_main_0.5_inter_0.1_n_100_p_21_rho_e_0.5_decor_TRUE_subpro_1012_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_year_1999_std_PCB_rank_c_betam_8_c_betai_2"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)


additional_PCB_rank_total <- sub_result[1,.(var_main_effect,
                                            var_inter_effect,
                                            cov_main_inter_effect,
                                            var_total_effect,
                                            structure,
                                            decor,
                                            x_dist)]


# replace the 999 as NA 
sub_result[EigenPrism_total == 999, c("EigenPrism_total", "EigenPrism_CI") := list(NA,NA)]


# EigenPrism
summary_result_Eg <- sub_result[, .(MSE = mean((EigenPrism_total - mean(var_total_effect))^2, na.rm = T),
                                    est = mean(EigenPrism_total, na.rm = T),
                                    est_jack = mean(Eg_mean_jack),
                                    var = var(EigenPrism_total, na.rm = T),
                                    v_jack = mean(Eg_var_jack),
                                    v_jack_c = mean(Eg_var_jack_corr,na.rm = T),
                                    v_Eg = mean((EigenPrism_CI/(2*z_0.8))^2)),
                                by = n]
summary_result_Eg[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_Eg <- cbind(summary_result_Eg)
summary_final_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(MSE = mean((GCTA_total - mean(var_total_effect))^2, na.rm = T),
                                      est = mean(GCTA_total, na.rm = T),
                                      est_jack = mean(GCTA_mean_jack),
                                      var = var(GCTA_total, na.rm = T),
                                      v_jack = mean(GCTA_var_jack),
                                      v_jack_c = mean(GCTA_var_jack_corr)),
                                  by = n]
summary_result_GCTA[,v_jack_diff := (v_jack_c + v_jack)/2]

summary_final_GCTA <- cbind(summary_result_GCTA)
summary_final_GCTA[,method := "GCTA"]
summary_final_PCB_rank_total_corr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)

##############################################################################################################################################
