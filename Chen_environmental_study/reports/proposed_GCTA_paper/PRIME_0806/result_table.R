library(knitr)
library(kableExtra)
options(knitr.table.format = "latex")
R.utils::sourceDirectory("~/dev/projects/Chen_environmental_study/R_code/main_fn",modifiedOnly = FALSE)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/decor/prime_0806/")

# for a singal file
upper = 0.9
lower = 0.1
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

result_path <- "decor_method_SVD_result_list_fixed_sub_normal_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_TRUE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                       est_var = var(est_mean, na.rm = T),
                                                                                       est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final %>% setorder(., n,method)
summary_final_normal <- cbind(summary_final, additional)

result_path <- "decor_method_SVD_result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_TRUE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final %>% setorder(., n, method)
summary_final_chi <- cbind(summary_final, additional)

summary_final_SVD <- rbind(summary_final_normal, summary_final_chi)
summary_final_SVD <- dcast(setDT(summary_final_SVD), x_dist + n ~ method, value.var = c("est_mean", "est_var", "MSE"))
write.csv(summary_final_SVD, file = "./SVD.csv", row.names = F)

#### True value 

result_path <- "decor_method_true_sigma_result_list_fixed_sub_normal_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_TRUE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final[,target := "main"] %>% setorder(., n)
summary_final_normal <- cbind(summary_final, additional)

result_path <- "decor_method_true_sigma_result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_TRUE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final[,target := "main"] %>% setorder(., n)
summary_final_chi <- cbind(summary_final, additional)

summary_final_true <- rbind(summary_final_normal, summary_final_chi)
summary_final_true <- dcast(setDT(summary_final_true), x_dist + n ~ method, value.var = c("est_mean", "est_var", "MSE"))


write.csv(summary_final_true, file = "./true.csv", row.names = F)

## None
result_path <- "decor_method_None_result_list_fixed_sub_normal_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_FALSE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final[,target := "main"] %>% setorder(., n)
summary_final_normal <- cbind(summary_final, additional)

result_path <- "decor_method_None_result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_FALSE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final[,target := "main"] %>% setorder(., n)
summary_final_chi <- cbind(summary_final, additional)

summary_final_none <- rbind(summary_final_normal, summary_final_chi)
summary_final_none <- dcast(setDT(summary_final_none), x_dist + n ~ method, value.var = c("est_mean", "est_var", "MSE"))
write.csv(summary_final_none, file = "./none.csv", row.names = F)


### Glasso
result_path <- "decor_method_GLASSO_result_list_fixed_sub_normal_structure_un_main_0.5_inter_0_n_200_500_1000_p_500_rho_e_0.5_dim_red_coeff__last__decor_TRUE_subpro_0_iter_100_nsub_1_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# additional 
additional <- sub_result[1,.(x_dist)]
# least_square
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    var_main_effect = var_main_effect[1]), by = .(i,n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                            est_var = var(est_mean, na.rm = T),
                                                                                            est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)
summary_result_Eg[,method := "EigenPrism"]
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i, n)][, .(MSE = mean((est_mean - mean(var_main_effect, na.rm = T))^2, na.rm = T),
                                                                                               est_var = var(est_mean, na.rm = T),
                                                                                               est_mean = mean(est_mean, na.rm = T)), by = n] %>% round(.,2)

summary_result_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_result_Eg, summary_result_GCTA), fill = TRUE)
summary_final[,target := "main"] %>% setorder(., n)
summary_final_normal <- cbind(summary_final, additional)

summary_final_Glasso <- rbind(summary_final_normal, summary_final_chi)
summary_final_Glasso <- dcast(setDT(summary_final_none), x_dist + n ~ method, value.var = c("est_mean", "est_var", "MSE"))
write.csv(summary_final_none, file = "./none.csv", row.names = F)

######################### 
# sub-sampling n with sample size
#######################
result_path <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_100_500_1000_2000_p_500_rho_e_0.5_dim_red_coeff__subpro_0.5_iter_500_nsub_200_GCTA_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[n > 100,]
# sub_result <- sub_result[i ==1,]
# sub_result[,data_path := NULL]
# sub_result[,p := 33]

# additional 
additional <- sub_result[1,.(x_dist)]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(est_var = var(est_mean),
                                                                                              est_mean = mean(est_mean)), by = n] %>% round(.,2)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_main, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,pro:=NULL][,p:=NULL] %>% round(.,2)
summary_final_GCTA <- merge(sub_summary_result_GCTA,summary_result_GCTA, by = "n")
summary_final_GCTA[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_final_GCTA_normal <- cbind(additional, summary_final_GCTA)



result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_100_500_1000_2000_p_500_rho_e_0.5_dim_red_coeff__subpro_0.5_iter_500_nsub_200_GCTA_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[n > 100,]
# sub_result <- sub_result[i ==1,]
# sub_result[,data_path := NULL]
# sub_result[,p := 33]

# additional 
additional <- sub_result[1,.(x_dist)]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i,n)][, .(est_var = var(est_mean),
                                                                                              est_mean = mean(est_mean)), by = n] %>% round(.,2)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_main, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,pro:=NULL][,p:=NULL] %>% round(.,2)
summary_final_GCTA <- merge(sub_summary_result_GCTA,summary_result_GCTA, by = "n")
summary_final_GCTA[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_final_GCTA_chi <- cbind(additional, summary_final_GCTA)
summary_final_GCTA_table <- rbind(summary_final_GCTA_normal, summary_final_GCTA_chi) 
summary_final_GCTA_table <- summary_final_GCTA_table[,.(x_dist, n, est_var, var_jack, sub_z_length, sub_z_coverage, var_diff_ratio)]
write.csv(summary_final_GCTA_table, file = "./sub_n.csv", row.names = F)



######################### 
# sub-sampling n with leave-1 out 
#######################

result_path <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_500_p_1000_rho_e_0.5_dim_red_coeff__subpro_101_iter_1000_nsub_500_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[pro != 0.1]
# sub_result <- sub_result[i ==1,]
# sub_result[,data_path := NULL]
# sub_result[,p := 33]

# additional 
additional <- sub_result[1,.(x_dist)]


# Eg
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    EigenPrism_CI1 = mean(EigenPrism_CI1),
                                    EigenPrism_CI2 = mean(EigenPrism_CI2),
                                    var_main_effect = var_main_effect[1]), by = i][, est_CI_length_coverage:= var_main_effect<=EigenPrism_CI2 &var_main_effect>=EigenPrism_CI1]
summary_result_Eg <- summary_result_Eg[, .(est_var = var(est_mean),
                                           est_mean = mean(est_mean),
                                           est_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE),
                                           est_CI_length_coverage = mean(est_CI_length_coverage, na.rm = TRUE))] %>% round(.,2)


sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                                          var_jack = jack_var(sub_EigenPrism_main, pro = pro),
                                          sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
                                          sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                      by = .(n,rho_e, p,i,pro)]
sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,p:=NULL] %>% round(.,2)
summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_Eg_101_normal <- summary_final_Eg[,method := "EigenPrism"]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i)][, .(est_var = var(est_mean),
                                                                                            est_mean = mean(est_mean))] %>% round(.,2)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_main, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,p:=NULL] %>% round(.,2)
summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_GCTA_101_normal <- summary_final_GCTA[,method := "GCTA"]




######################### 
# sub-sampling n with leave-d out 
#######################

result_path <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_500_p_1000_rho_e_0.5_dim_red_coeff__subpro_0.1_0.3_0.6_0.9_iter_1000_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[pro != 0.1]
# sub_result <- sub_result[i ==1,]
# sub_result[,data_path := NULL]
# sub_result[,p := 33]

# additional 
additional <- sub_result[1,.(x_dist)]


# Eg
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    EigenPrism_CI1 = mean(EigenPrism_CI1),
                                    EigenPrism_CI2 = mean(EigenPrism_CI2),
                                    var_main_effect = var_main_effect[1]), by = i][, est_CI_length_coverage:= var_main_effect<=EigenPrism_CI2 &var_main_effect>=EigenPrism_CI1]
summary_result_Eg <- summary_result_Eg[, .(est_var = var(est_mean),
                                           est_mean = mean(est_mean),
                                           est_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE),
                                           est_CI_length_coverage = mean(est_CI_length_coverage, na.rm = TRUE))] %>% round(.,2)


sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                                          var_jack = jack_var(sub_EigenPrism_main, pro = pro),
                                          sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
                                          sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                      by = .(n,rho_e, p,i,pro)]
sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,p:=NULL] %>% round(.,2)
summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg)
summary_final_Eg[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_final_Eg[,method := "EigenPrism"]
summary_final_Eg <- rbind(summary_final_Eg, summary_Eg_101_normal)

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i)][, .(est_var = var(est_mean),
                                                                                              est_mean = mean(est_mean))] %>% round(.,2)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_main, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,p:=NULL] %>% round(.,2)
summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_final_GCTA[,method := "GCTA"]
summary_final_GCTA <- rbind(summary_final_GCTA, summary_GCTA_101_normal)

summary_final_normal <- rbind(summary_final_Eg, summary_final_GCTA, fill = TRUE)
summary_final_normal <- cbind(additional, summary_final_normal)


result_path <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_500_p_1000_rho_e_0.5_dim_red_coeff__subpro_0.1_0.3_0.6_0.9_iter_1000_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[pro != 0.1]

# sub_result <- sub_result[i ==1,]
# sub_result[,data_path := NULL]
# sub_result[,p := 33]

# additional 
additional <- sub_result[1,.(x_dist)]


# Eg
summary_result_Eg <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = T),
                                    EigenPrism_CI1 = mean(EigenPrism_CI1),
                                    EigenPrism_CI2 = mean(EigenPrism_CI2),
                                    var_main_effect = var_main_effect[1]), by = i][, est_CI_length_coverage:= var_main_effect<=EigenPrism_CI2 &var_main_effect>=EigenPrism_CI1]
summary_result_Eg <- summary_result_Eg[, .(est_var = var(est_mean),
                                           est_mean = mean(est_mean),
                                           est_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE),
                                           est_CI_length_coverage = mean(est_CI_length_coverage, na.rm = TRUE))] %>% round(.,2)


sub_summary_result_Eg_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                                          var_jack = jack_var(sub_EigenPrism_main, pro = pro),
                                          sub_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
                                          sub_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                      by = .(n,rho_e, p,i,pro)] 
sub_summary_result_Eg <- sub_summary_result_Eg_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,p:=NULL] %>% round(.,2)
summary_final_Eg <- cbind(sub_summary_result_Eg, summary_result_Eg) 
summary_final_Eg[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_final_Eg[,method := "EigenPrism"]


# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = .(i)][, .(est_var = var(est_mean),
                                                                                            est_mean = mean(est_mean))] %>% round(.,2)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                            var_jack = jack_var(sub_GCTA_main, pro = pro),
                                            sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                            sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                        by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL][,rho_e:= NULL][,p:=NULL] %>% round(.,2)
summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA[, var_diff_ratio := ((var_jack - est_var)/est_var) %>% round(.,2)]
summary_final_GCTA[,method := "GCTA"]
summary_final_chi <- rbind(summary_final_Eg, summary_final_GCTA, fill = TRUE)
summary_final_chi <- cbind(additional, summary_final_chi)
# summary_final_table <- rbind(summary_final_normal, summary_final_chi)
summary_final_table <- summary_final_normal
summary_final_table <- summary_final_table[,n := NULL][]

summary_final_table <- summary_final_table[,.(x_dist, method,pro, est_effect = est_mean, est_var,var_jack, 
                       sub_z_length, sub_z_coverage, est_CI_length, est_CI_length_coverage, var_diff_ratio)]
# summary_final_table <- dcast(setDT(summary_final_table), x_dist + pro ~ method, value.var = c("est_var", "var_jack",
#                                                                        "sub_z_length","sub_z_coverage",
#                                                                        "est_CI_length","est_CI_length_coverage","var_diff_ratio" ))
summary_final_table[,pro:= 1 - pro]
summary_final_table[pro == -100, pro := 1]
write.csv(summary_final_table, file = "./sub_pro.csv", row.names = F) 
