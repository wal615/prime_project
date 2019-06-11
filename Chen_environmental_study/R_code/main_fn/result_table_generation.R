library(data.table)
library(tidyverse)
library(ggforce)
library(ggpubr)
library(gridExtra)

# modified mean and var so that they ignores na
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
var <- function(x, ..., na.rm = TRUE) {
  stats::var(x, ..., na.rm = na.rm)
}

generate_result <- function(result_path, lower, upper, method, est_signal = "main", col_names){
  file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
  file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
  sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
  # replace the 999 as NA 
  if (method == "EigenPrism"){
    col_name <- paste(c(method, est_signal), collapse = "_")
    sub_result[get(col_name) == 999, c(col_name, "EigenPrism_CI1", "EigenPrism_CI2") := list(NA,NA,NA)]
    col_name <- paste(c("sub", method, est_signal), collapse = "_")
    sub_result[get(col_name) == 999, c("sub_EigenPrism_main", "sub_EigenPrism_CI1", "sub_EigenPrism_CI2") := list(NA,NA,NA)]
  }
  true_table <- sub_result[, .(true_emp_main = mean(var_main_effect), 
                               true_emp_inter = mean(var_inter_effect), 
                               true_emp_cov = mean(cov_main_inter_effect),
                               true_emp_total = mean(var_total_effect))]
  # point_estimation
  point_est_names <- col_names[!grepl(pattern = "_CI", perl = T, x = col_names)]
  est_table <- sub_result[, .(est = mean(get(point_est_names)),
                              var = var(get(point_est_names)))] 
  # sub sampling mehtod
  sub_point_est_names <- paste0("sub_",point_est_names)
  true_effect_name <- paste0("var_", est_signal, "_effect")
  sub_table <- sub_result[, .(sub_est = mean(get(sub_point_est_names), na.rm = TRUE),
                              sub_var = var(get(sub_point_est_names), na.rm = TRUE),
                              sub_NA = sum(is.na(get(sub_point_est_names)))), by = i]
  sub_est_table <- sub_table[,.(sub_est_ave = mean(sub_est),
                                sub_est_ave_var = var(sub_est),
                                sub_mean_of_var = mean(sub_var))]
  sub_CI_result <- sub_result[,.(true_value = mean(get(true_effect_name)),
                                 sub_CI1 = quantile(get(sub_point_est_names), lower, na.rm = TRUE), 
                                 sub_CI2 = quantile(get(sub_point_est_names), upper,na.rm = TRUE)), by = i]
  sub_CI_result <- sub_CI_result[,.(sub_CI_length = sub_CI2 - sub_CI1,
                                    sub_CI_coverage = ((true_value >= sub_CI1) & (true_value <= sub_CI2)))]
  sub_CI_table <- sub_CI_result[, .(sub_CI_length = mean(sub_CI_length),
                                    sub_CI_coverage_rate = mean(sub_CI_coverage))]
  # Eg CI 
  if(method == "EigenPrism"){
    CI_est_names <- c(col_names[grepl(pattern = "_CI1", perl = T, x = col_names)],col_names[grepl(pattern = "_CI2", perl = T, x = col_names)]) 
    CI_result <- sub_result[,.(CI_length = (get(CI_est_names[2]) - get(CI_est_names[1])), 
                               CI_coverage = (get(true_effect_name) >= get(CI_est_names[1])) & (get(true_effect_name) <= get(CI_est_names[2])))]
    CI_table <- CI_result[,.(CI_length = mean(CI_length),
                             CI_coverage_rate = mean(CI_coverage))]
    sub_CI_est_names <- paste0("sub_",CI_est_names)
    sub_eg_CI_result <- sub_result[,.(i = i,
                                      sub_eg_CI_length = get(sub_CI_est_names[2]) - get(sub_CI_est_names[1]),
                                      sub_eg_CI_coverage = (get(true_effect_name) >= get(sub_CI_est_names[1])) & (get(true_effect_name) <= get(sub_CI_est_names[2])))]
    sub_eg_CI_table <- sub_eg_CI_result[, .(sub_eg_CI_length = mean(sub_eg_CI_length, na.rm = T),
                                            sub_eg_CI_coverage_rate = mean(sub_eg_CI_coverage, na.rm = T)),
                                        by = i][,.(sub_eg_CI_length = mean(sub_eg_CI_length),
                                                   sub_eg_CI_coverage_rate = mean(sub_eg_CI_coverage_rate))]
  } else {
    CI_table <- data.table(CI_length = NA,CI_coverage = NA)
    sub_eg_CI_table <-data.table(sub_eg_CI_length = NA,sub_eg_CI_coverage = NA) 
  }
  additional <- sub_result[1,.(x_dist, structure, n, p,pro,decor,
                               method = method, est_signal = est_signal, true_CI_coverage_rate = upper - lower)]
  cbind(additional,true_table, est_table, CI_table, sub_est_table, sub_CI_table, sub_eg_CI_table)
}