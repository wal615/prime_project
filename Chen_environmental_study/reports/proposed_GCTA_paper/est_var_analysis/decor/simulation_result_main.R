library(knitr)
library(kableExtra)
options(knitr.table.format = "latex")
R.utils::sourceDirectory("~/dev/projects/Chen_environmental_study/R_code/main_fn",modifiedOnly = FALSE)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/decor")

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
  var_1 <- (x_m - x)^2 %>% sum(., na.rm = T)
  if(pro == 101){
    var_2 <-  (length(x) -1) * 1/length(x) * var_1
  } else if (pro ==102){
    var_2 <- 1/length(x) * var_1
  } else{
    var_2 <- pro/(1-pro) * 1/length(x) * var_1
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



result_path <- "result_list_fixed_sub_PCB_main_0.5_inter_0_n_1000_p_33_rho_e_0.5_dim_red_coeff__last__decor_FALSE_subpro_101_iter_100_nsub_0_least_square_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
sub_result <- sub_result[i ==1,]
sub_result[,data_path := NULL]
sub_result[,p := 33]

# additional 
additional <- sub_result[1,.(var_main_effect,
                             var_inter_effect,
                             cov_main_inter_effect,
                             var_total_effect,
                             structure,
                             decor,
                             x_dist)]

# least_square
summary_result_least <- sub_result[, .(est_mean = mean(least_square_main, na.rm = T),
                                       var_main_effect = var_main_effect[1]), by = i][, .(MES = mean((est_mean - mean(var_main_effect))^2),
                                                                                                   est_var = var(est_mean),
                                                                                                   est_mean = mean(est_mean))]


sub_summary_result_least_i <- sub_result[, .(sub_est_mean = mean(sub_least_square_main, na.rm = TRUE),
                                             var_jack = jack_var(sub_least_square_main, pro = pro),
                                             sub_z_length = sub_CI_lenght(sub_least_square_main, pro = pro,z_p = z_p),
                                             sub_z_coverage = sub_coverage_rate_z(sub_least_square_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                         by = .(n,rho_e, p,i,pro)]
sub_summary_result_least <- sub_summary_result_least_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_least <- cbind(sub_summary_result_least, summary_result_least)
summary_final_least[,method := "least_square"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = T),
                                      var_main_effect = var_main_effect[1]), by = i][, .(MES = mean((est_mean - mean(var_main_effect))^2),
                                                                                                   est_var = var(est_mean),
                                                                                                   est_mean = mean(est_mean))]

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                             var_jack = jack_var(sub_GCTA_main, pro = pro),
                                             sub_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                             sub_z_coverage = sub_coverage_rate_z(sub_GCTA_main, mean(var_main_effect), upper = upper, lower = lower, pro = pro)),
                                         by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_GCTA <- cbind(sub_summary_result_GCTA,summary_result_GCTA)
summary_final_GCTA[,method:= "GCTA"]

summary_final <- rbindlist(list(summary_final_least, summary_final_GCTA), fill = TRUE)
summary_final[,target := "main"]
summary_final[, var_diff_ratio := (var_jack - est_var)/est_var]
summary_final <- cbind(summary_final, additional)

# summary_tmp <- summary_final

summary_tmp <- rbind(summary_tmp, summary_final) 

write.csv(table, file = "~/dev/projects/Chen_environmental_study/reports/proposed_GCTA_paper/est_var_analysis/decor/main_result.csv", row.names = F)
table <- read.csv("~/dev/projects/Chen_environmental_study/reports/proposed_GCTA_paper/est_var_analysis/decor/main_result.csv",stringsAsFactors = F) %>% data.table(.) 
table <- table[x_dist != "PCB"]
table[,X:=NULL]
table_final <- rbindlist(list(table,summary_tmp)) 
table_final <- table[,c("var_main_effect","x_dist","decor","method","est_mean","est_var","MES",
                        "sub_est_mean","var_jack","sub_z_length","sub_z_coverage")] %>% setorder(., x_dist,decor,method)

names <- c("var_main_effect","est_mean","est_var", "MES", "sub_est_mean", "var_jack","sub_z_length","sub_z_coverage")
table[,(names) := lapply(.SD, round,digits =3), .SDcols = names]
table[,var_diff_ratio := round((var_jack - est_var)/est_var,3)]
