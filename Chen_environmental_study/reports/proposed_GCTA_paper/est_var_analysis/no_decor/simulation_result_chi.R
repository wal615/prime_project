options(warn = 1, error = bettertrace::stacktrace)
R.utils::sourceDirectory("~/dev/projects/Chen_environmental_study/R_code/main_fn",modifiedOnly = FALSE)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/non_decore/")
result_args_generate <- function(long_name){
  long_list <- strsplit(x = long_name, split = "_", fixed = TRUE) %>% unlist(.)
  est_signal <- long_list[length(long_list)] 
  index <- c("GCTA", "EigenPrism", "square") %in% long_list 
  methods <- c("GCTA", "EigenPrism", "least_square")[index]
  name_list <- list()
  for(m in methods){
    if(m == "GCTA" & ("I" %in% long_list)){
      col_names <- paste0("GCTA_", est_signal)
      } else {
      col_names <- paste0("prop_", est_signal)}
    if(m == "EigenPrism"){
      col_names <- c(paste0("EigenPrism_", est_signal), "EigenPrism_CI1","EigenPrism_CI2")  
    }  else if(m == "least_square"){
      col_names <- c(paste0("least_square_", est_signal))
    }
    name_list <- append(name_list, list(col_names))
  }  
  args <- expand.grid(result_list_path = long_name, 
                      method = methods, 
                      est_signal = est_signal,
                      col_names = name_list, stringsAsFactors = FALSE) %>% data.table(.)
  args[,index := grepl(pattern = method,x = col_names), by = seq_len(nrow(args))]
  args <- args[index == TRUE,][,index:=NULL]
  args
}

result_list <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_100_500_1000_p_1000_rho_e_0.2_0.5_0.7_dim_red_coeff__subpro_0.5_iter_1000_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
args_table <- mapply(FUN = result_args_generate, long_name =result_list, SIMPLIFY = FALSE) %>% rbindlist(.)
  

result_list <- mapply(FUN = generate_result, 
                      result_path = args_table[,result_list_path],
                      method = args_table[,method],
                      est_signal = args_table[,est_signal],
                      col_names = args_table[,col_names],
                      MoreArgs = list(upper = 0.9, lower = 0.1), 
                      SIMPLIFY = FALSE) %>% rbindlist(.)
write.csv(file = "./result_list.csv",x = result_list, row.names = FALSE)


setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/non_decore/")
result_list <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_100_500_1000_p_1000_rho_e_0.2_0.5_0.7_dim_red_coeff__subpro_0.5_iter_1000_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
args_table <- args_table <- mapply(FUN = result_args_generate, long_name =result_list, SIMPLIFY = FALSE) %>% rbindlist(.) 
result_list <- mapply(FUN = generate_result, 
                      result_path = args_table[,result_list_path],
                      method = args_table[,method],
                      est_signal = args_table[,est_signal],
                      col_names = args_table[,col_names],
                      MoreArgs = list(upper = 0.9, lower = 0.1), 
                      SIMPLIFY = FALSE) %>% rbindlist(.)

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

result_path <- result_list <- "result_list_fixed_sub_normal_structure_I_main_0.5_inter_0_n_100_500_1000_p_1000_rho_e_0.2_0.5_0.7_dim_red_coeff__subpro_0.5_iter_1000_nsub_200_EigenPrism_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,CI_length_coverage := var_main_effect >= EigenPrism_CI1 & var_main_effect <= EigenPrism_CI2 ]
# EigenPrsim
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = TRUE),
                                            est_var = var(EigenPrism_main, na.rm = TRUE),
                                            est_mean_CI_emp_length = CI_length(EigenPrism_main, upper = upper, lower = lower),
                                            est_mean_CI_z_length = 2 * sd(EigenPrism_main, na.rm = TRUE)*z_p,
                                            est_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE),
                                            est_CI_length_coverage = mean(CI_length_coverage, na.rm = TRUE)), by = .(n,rho_e, p)] %>% setorder(., rho_e,n,p)

sub_summary_result_EigenPrism_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
                                                sub_est_var = var(sub_EigenPrism_main, na.rm = TRUE),
                                                sub_est_mean_CI_emp_length = CI_length(sub_EigenPrism_main, upper = upper, lower = lower),
                                                sub_est_mean_CI_emp_coverage = coverage_rate_emp(sub_EigenPrism_main, upper = upper, lower = lower, true = 10),
                                                sub_est_mean_CI_z_length = 2 * sd(sub_EigenPrism_main, na.rm = TRUE)*z_p,
                                                sub_est_mean_CI_z_coverage = coverage_rate_emp(sub_EigenPrism_main, upper = upper, lower = lower, true = 10),
                                                sub_est_CI_length = mean(sub_EigenPrism_CI2 - sub_EigenPrism_CI1, na.rm = TRUE)), by = .(n,rho_e, p,i)] %>% setorder(., rho_e,n,p,i)
sub_summary_result_EigenPrism <- sub_summary_result_EigenPrism_i[, lapply(.SD, mean), by = .(n,rho_e, p)][,i:=NULL]
summary_final_EigenPrism <- cbind(summary_result_EigenPrism, sub_summary_result_EigenPrism, method = "EigenPrism")
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = TRUE),
                                            est_var = var(GCTA_main, na.rm = TRUE),
                                            est_mean_CI_emp_length = CI_length(GCTA_main, upper = upper, lower = lower),
                                            est_mean_CI_z_length = 2 * sd(GCTA_main, na.rm = TRUE)*z_p), by = .(n,rho_e, p)] %>% setorder(., rho_e,n,p)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                                  sub_est_var = var(sub_GCTA_main, na.rm = TRUE),
                                                  sub_est_mean_CI_emp_length = CI_length(sub_GCTA_main, upper = upper, lower = lower),
                                                  sub_est_mean_CI_emp_coverage = coverage_rate_emp(sub_GCTA_main, upper = upper, lower = lower, true = 10),
                                                  sub_est_mean_CI_z_length = 2 * sd(sub_GCTA_main, na.rm = TRUE)*z_p,
                                                  sub_est_mean_CI_z_coverage = coverage_rate_emp(sub_GCTA_main, upper = upper, lower = lower, true = 10)), by = .(n,rho_e, p,i)] %>% setorder(., rho_e,n,p,i)
sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p)][,i:=NULL]
summary_final_GCTA <- cbind(summary_result_GCTA, sub_summary_result_GCTA, method = "GCTA")
summary_final <- rbindlist(list(summary_final_EigenPrism, summary_final_GCTA), fill = TRUE)











# total effect 

result_path <- result_list <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0.1_n_561_p_33_dim_red_coeff__subpro_0.5_iter_100_nsub_200_EigenPrism_kernel_GCTA_kernel_est_total"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "sub_sampling",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result[,CI_length_coverage := var_total_effect >= EigenPrism_CI1 & var_total_effect <= EigenPrism_CI2 ]
# EigenPrsim
summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_total, na.rm = TRUE),
                                            est_var = var(EigenPrism_total, na.rm = TRUE),
                                            est_mean_CI_emp_length = CI_length(EigenPrism_total, upper = upper, lower = lower),
                                            est_mean_CI_z_length = 2 * sd(EigenPrism_total, na.rm = TRUE)*z_p,
                                            est_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE),
                                            est_CI_length_coverage = mean(CI_length_coverage, na.rm = TRUE)), by = .(n,rho_e, p)] %>% setorder(., rho_e,n,p)

sub_summary_result_EigenPrism_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_total, na.rm = TRUE),
                                                  sub_est_var = var(sub_EigenPrism_total, na.rm = TRUE),
                                                  sub_est_mean_CI_emp_length = CI_length(sub_EigenPrism_total, upper = upper, lower = lower),
                                                  sub_est_mean_CI_emp_coverage = coverage_rate_emp(sub_EigenPrism_total, upper = upper, lower = lower, true = 10),
                                                  sub_est_mean_CI_z_length = 2 * sd(sub_EigenPrism_total, na.rm = TRUE)*z_p,
                                                  sub_est_mean_CI_z_coverage = coverage_rate_emp(sub_EigenPrism_total, upper = upper, lower = lower, true = 10),
                                                  sub_est_CI_length = mean(sub_EigenPrism_CI2 - sub_EigenPrism_CI1, na.rm = TRUE)), by = .(n,rho_e, p,i)] %>% setorder(., rho_e,n,p,i)
sub_summary_result_EigenPrism <- sub_summary_result_EigenPrism_i[, lapply(.SD, mean), by = .(n,rho_e, p)][,i:=NULL]
summary_final_EigenPrism <- cbind(summary_result_EigenPrism, sub_summary_result_EigenPrism, method = "EigenPrism")
# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_total, na.rm = TRUE),
                                      est_var = var(GCTA_total, na.rm = TRUE),
                                      est_mean_CI_emp_length = CI_length(GCTA_total, upper = upper, lower = lower),
                                      est_mean_CI_z_length = 2 * sd(GCTA_total, na.rm = TRUE)*z_p), by = .(n,rho_e, p)] %>% setorder(., rho_e,n,p)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_total, na.rm = TRUE),
                                            sub_est_var = var(sub_GCTA_total, na.rm = TRUE),
                                            sub_est_mean_CI_emp_length = CI_length(sub_GCTA_total, upper = upper, lower = lower),
                                            sub_est_mean_CI_emp_coverage = coverage_rate_emp(sub_GCTA_total, upper = upper, lower = lower, true = 10),
                                            sub_est_mean_CI_z_length = 2 * sd(sub_GCTA_total, na.rm = TRUE)*z_p,
                                            sub_est_mean_CI_z_coverage = coverage_rate_emp(sub_GCTA_total, upper = upper, lower = lower, true = 10)), by = .(n,rho_e, p,i)] %>% setorder(., rho_e,n,p,i)
sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p)][,i:=NULL]
summary_final_GCTA <- cbind(summary_result_GCTA, sub_summary_result_GCTA, method = "GCTA")
summary_final <- rbindlist(list(summary_final_EigenPrism, summary_final_GCTA), fill = TRUE)
