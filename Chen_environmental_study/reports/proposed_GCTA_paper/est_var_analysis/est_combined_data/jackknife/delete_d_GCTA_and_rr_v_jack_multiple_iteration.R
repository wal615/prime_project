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


setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/combined_effects_GCTA_rr/")
#############################################################################################################################

# delete 1
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
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
d <- sub_result[,unique(d)]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_jack <- sub_result[, .(GCTA_main_jack = mean(S1_jack.GCTA_main, na.rm = T),
                                           GCTA_v_jack = mean(S1_v_jack_1, na.rm = T),
                                           GCTA_v_jack_var = var(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_8_d_1 <- merge(summary_result_GCTA,summary_result_GCTA_jack, by = "n")
summary_final_GCTA_8_d_1[, d := d]
# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_1 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_1[, d:=d]

# delete d = 1 1000
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_500_750_1000_1500_2000_p_1000_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
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
d <- sub_result[,unique(d)]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_jack <- sub_result[, .(GCTA_main_jack = mean(S1_jack.GCTA_main, na.rm = T),
                                           GCTA_v_jack = mean(S1_v_jack_1, na.rm = T),
                                           GCTA_v_jack_var = var(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_8_d_1_1000 <- merge(summary_result_GCTA,summary_result_GCTA_jack, by = "n")
summary_final_GCTA_8_d_1_1000[, d := d]

# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_1_1000 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_1_1000[, d := d]

# delete d = half 100
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_iter_100_nsub_1000_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
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
d <- sub_result[,unique(d)]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_jack <- sub_result[, .(GCTA_main_jack = mean(S1_jack.GCTA_main, na.rm = T),
                                           GCTA_v_jack = mean(S1_v_jack_1, na.rm = T),
                                           GCTA_v_jack_var = var(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_8_d_d <- merge(summary_result_GCTA,summary_result_GCTA_jack, by = "n")
summary_final_GCTA_8_d_d[, d := 0.5]
# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_d <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_d[, d := 0.5]

# delete d = half 100 with n_sub = n^1.5
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_100_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_iter_100_nsub_2828_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
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
d <- sub_result[,unique(d)]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_jack <- sub_result[, .(GCTA_main_jack = mean(S1_jack.GCTA_main, na.rm = T),
                                           GCTA_v_jack = mean(S1_v_jack_1, na.rm = T),
                                           GCTA_v_jack_var = var(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_8_d_d_m <- merge(summary_result_GCTA,summary_result_GCTA_jack, by = "n")
summary_final_GCTA_8_d_d_m[, d := 0.5]
summary_final_GCTA_8_d_d_m[, n_sub:= round(n^1.5,0)]
# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_d_m <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_d_m[, d := 0.5]
summary_final_GCTA_rr_8_d_d_m[, n_sub:= round(n^1.5,0)]

# delete d = half 1000
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_500_750_1000_1500_2000_p_1000_rho_e_0.5_decor_FALSE_subd_250_375_500_750_1000_iter_100_nsub_1000_GCTA_kernel_GCTA_rr_kernel_est_main_c_betam_8_c_betai_2_Var_null"
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
d <- sub_result[,unique(d)]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      NA_i = mean(is.na(GCTA_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_jack <- sub_result[, .(GCTA_main_jack = mean(S1_jack.GCTA_main, na.rm = T),
                                           GCTA_v_jack = mean(S1_v_jack_1, na.rm = T),
                                           GCTA_v_jack_var = var(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_8_d_d_1000 <- merge(summary_result_GCTA,summary_result_GCTA_jack, by = "n")
summary_final_GCTA_8_d_d_1000[, d:=0.5]
# GCTA_rr
summary_result_GCTA_rr <- sub_result[, .(est_mean = mean(GCTA_rr_main, na.rm = T),
                                         NA_i = mean(is.na(GCTA_rr_main)),
                                         var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                 est_var = var(est_mean, na.rm = T),
                                                                                                 est_mean = mean(est_mean, na.rm = T),
                                                                                                 NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_GCTA_rr_jack <- sub_result[, .(GCTA_rr_main_jack = mean(S2_jack.GCTA_rr_main, na.rm = T),
                                              GCTA_rr_v_jack = mean(S2_v_jack_1, na.rm = T),
                                              GCTA_rr_v_jack_var = var(S2_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_GCTA_rr_8_d_d_1000 <- merge(summary_result_GCTA_rr,summary_result_GCTA_rr_jack, by = "n")
summary_final_GCTA_rr_8_d_d_1000[,d:=0.5]

# correlation control groups with p = 1 d = 1 
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_un_main_0.5_inter_0_n_50_75_100_150_200_p_2_rho_e_0.5_decor_FALSE_subd_1_iter_100_nsub_0_correlation_null_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,var_main_effect := NULL] 
sub_result[,var_main_effect := 0.5] # the targe is the median of chi with df = 1

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,unique(d)]


# cor
summary_result_cor <- sub_result[, .(est_mean = mean(cor_main, na.rm = T),
                                      NA_i = mean(is.na(cor_main)),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                              est_var = var(est_mean, na.rm = T),
                                                                                              est_mean = mean(est_mean, na.rm = T),
                                                                                              NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_cor_jack <- sub_result[, .(cor_main_jack = mean(S1_jack.cor_main, na.rm = T),
                                           cor_v_jack = mean(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_cor_8_d_1_100 <- merge(summary_result_cor,summary_result_cor_jack, by = "n")
summary_final_cor_8_d_1_100[, d:=1]

# correlation control groups with p = 1 d = d
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_un_main_0.5_inter_0_n_50_75_100_150_200_p_2_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_iter_100_nsub_1000_correlation_null_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,var_main_effect := NULL] 
sub_result[,var_main_effect := 0.5] # the targe is the median of chi with df = 1

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,unique(d)]


# cor
summary_result_cor <- sub_result[, .(est_mean = mean(cor_main, na.rm = T),
                                     NA_i = mean(is.na(cor_main)),
                                     var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                             est_var = var(est_mean, na.rm = T),
                                                                                             est_mean = mean(est_mean, na.rm = T),
                                                                                             NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_cor_jack <- sub_result[, .(cor_main_jack = mean(S1_jack.cor_main, na.rm = T),
                                          cor_v_jack = mean(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_cor_8_d_d_100 <- merge(summary_result_cor,summary_result_cor_jack, by = "n")
summary_final_cor_8_d_d_100[, d:=0.5]


# median control groups d = 1 
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_2_rho_e_0.5_decor_FALSE_subd_1_iter_1000_nsub_0_median_null_est_main_c_betam_8_c_betai_2_Var_null"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,var_main_effect := NULL] 
sub_result[,var_main_effect := 0] # the targe correlation is 0.5

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,unique(d)]


# cor
summary_result_median <- sub_result[, .(est_mean = mean(median_main, na.rm = T),
                                        NA_i = mean(is.na(median_main)),
                                        var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                est_var = var(est_mean, na.rm = T),
                                                                                                est_mean = mean(est_mean, na.rm = T),
                                                                                                NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_median_jack <- sub_result[, .(median_main_jack = mean(S1_jack.median_main, na.rm = T),
                                             median_v_jack = mean(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_median_8_d_1_1000 <- merge(summary_result_median,summary_result_median_jack, by = "n")
summary_final_median_8_d_1_1000[, d := 1]

# median control groups d = d
#############################################################################################################################

result_path <- "decor_None_sparse_None_normal_structure_I_main_0.5_inter_0_n_50_75_100_150_200_p_2_rho_e_0.5_decor_FALSE_subd_25_38_50_75_100_iter_1000_nsub_5000_median_null_est_main_c_betam_8_c_betai_2_Var_null/"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,var_main_effect := NULL] 
sub_result[,var_main_effect := 0] # the targe is the median of chi with df = 1

additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]
d <- sub_result[,unique(d)]


# cor
summary_result_median <- sub_result[, .(est_mean = mean(median_main, na.rm = T),
                                        NA_i = mean(is.na(median_main)),
                                        var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect))^2, na.rm = T),
                                                                                                est_var = var(est_mean, na.rm = T),
                                                                                                est_mean = mean(est_mean, na.rm = T),
                                                                                                NA_main = sum(NA_i)), by = n] %>% 
  setorder(., by = n)
summary_result_median_jack <- sub_result[, .(median_main_jack = mean(S1_jack.median_main, na.rm = T),
                                             median_v_jack = mean(S1_v_jack_1, na.rm = T)),by = .(n)] %>%
  setorder(., by = n)

summary_final_median_8_d_d_1000 <- merge(summary_result_median,summary_result_median_jack, by = "n")
summary_final_median_8_d_d_1000[, d := 0.5]
