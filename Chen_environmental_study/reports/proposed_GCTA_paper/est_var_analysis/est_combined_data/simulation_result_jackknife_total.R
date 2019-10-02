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

jack_var_2 <- function(x, pro){
  n_sub <- length(x) - is.na(x)
  x_m <- mean(x, na.rm = T)
  var_1 <- (x_m - x)^2 %>% sum(., na.rm = T)
  var_2 <- (1+(n_sub-1)*0.5)/(n_sub*(n_sub - 1))*var_1
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



#############################################################################################################################

setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_jackknife_reports_09_25_2019/")
result_path <- "decor_method_None_sparse_method_None_result_list_fixed_sub_PCB_structure_un_main_0.5_inter_0.1_n_100_200_500_p_21_rho_e_0.5_decor_FALSE_subpro_101_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_year_1999_std_PCB_FALSE_c_betam_8_c_betai_2/"
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

summary_final_none <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)


#############################################################################################################################
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_jackknife_reports_09_25_2019/")
result_path <- "decor_method_hist_sparse_method_dgpGLASSO_result_list_fixed_sub_PCB_structure_un_main_0.5_inter_0.1_n_100_200_500_p_21_rho_e_0.5_decor_TRUE_subpro_101_iter_100_nsub_0_EigenPrism_kernel_GCTA_kernel_est_total_year_1999_std_PCB_FALSE_c_betam_8_c_betai_2/"
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

summary_final_decorr <- rbindlist(list(summary_final_Eg, summary_final_GCTA), fill = TRUE)