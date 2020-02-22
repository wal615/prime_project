options(warn = 1, error = bettertrace::stacktrace)
library(knitr)
library(kableExtra)
options(knitr.table.format = "latex")
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


setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/decor")
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

result_path <- "result_list_fixed_sub_chi_structure_un_main_0.5_inter_0_n_1000_p_33_rho_e_0.5_dim_red_coeff__last__decor_FALSE_subpro_101_iter_100_nsub_0_least_square_kernel_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
sub_result <- lapply(file_list, function (x) {read.csv(x, header = TRUE, stringsAsFactors = FALSE)}) %>% rbindlist(., fill = TRUE)
# EigenPrsim
# sub_result[,CI_length_coverage := var_main_effect >= EigenPrism_CI1 & var_main_effect <= EigenPrism_CI2 ]
# sub_result[,sub_CI_length_coverage := var_main_effect >= sub_EigenPrism_CI1 & var_main_effect <= sub_EigenPrism_CI2 ]
# summary_result_EigenPrism <- sub_result[, .(est_mean = mean(EigenPrism_main, na.rm = TRUE),
#                                             est_var = var(EigenPrism_main, na.rm = TRUE),
#                                             est_mean_CI_emp_length = CI_length(EigenPrism_main, upper = upper, lower = lower),
#                                             est_mean_CI_z_length = 2 * sd(EigenPrism_main, na.rm = TRUE)*z_p,
#                                             est_CI_length = mean(EigenPrism_CI2 - EigenPrism_CI1, na.rm = TRUE),
#                                             est_CI_length_coverage = mean(CI_length_coverage, na.rm = TRUE)), by = .(n,rho_e, p, pro)] %>% setorder(., rho_e,n,p,pro)
# 
# sub_summary_result_EigenPrism_i <- sub_result[, .(sub_est_mean = mean(sub_EigenPrism_main, na.rm = TRUE),
#                                                 sub_est_var_jack = jack_var_2(sub_EigenPrism_main, pro = pro),
#                                                 sub_est_mean_CI_z_length = sub_CI_lenght(sub_EigenPrism_main, pro = pro,z_p = z_p),
#                                                 sub_est_mean_CI_z_coverage = sub_coverage_rate_z(sub_EigenPrism_main, 10, upper = upper, lower = lower, pro = pro),
#                                                 sub_CI_length_coverage = mean(sub_CI_length_coverage,na.rm = T)),
#                                               by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)
# sub_summary_result_EigenPrism <- sub_summary_result_EigenPrism_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
# summary_final_EigenPrism <- merge(summary_result_EigenPrism, sub_summary_result_EigenPrism, by = c("n","rho_e","p","pro"))
# summary_final_EigenPrism[,method := "EigenPrism"]

# least_square
summary_result_least <- sub_result[, .(est_mean = mean(least_square_main, na.rm = TRUE),
                                            est_var = var(least_square_main, na.rm = TRUE),
                                            est_mean_CI_emp_length = CI_length(least_square_main, upper = upper, lower = lower),
                                            est_mean_CI_z_length = 2 * sd(least_square_main, na.rm = TRUE)*z_p), by = .(n,rho_e, p, pro)] %>% setorder(., rho_e,n,p,pro)

sub_summary_result_least_i <- sub_result[, .(sub_est_mean = mean(sub_least_square_main, na.rm = TRUE),
                                                sub_est_var_jack = jack_var(sub_least_square_main, pro = pro),
                                                sub_est_mean_CI_z_length = sub_CI_lenght(sub_least_square_main, pro = pro,z_p = z_p),
                                                sub_est_mean_CI_z_coverage = sub_coverage_rate_z(sub_least_square_main, 10, upper = upper, lower = lower, pro = pro)),
                                              by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)
sub_summary_result_least <- sub_summary_result_least_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_least <- merge(summary_result_least, sub_summary_result_least, by = c("n","rho_e","p","pro"))
summary_final_least[,method := "least_square"]

# GCTA
summary_result_GCTA <- sub_result[, .(est_mean = mean(GCTA_main, na.rm = TRUE),
                                            est_var = var(GCTA_main, na.rm = TRUE),
                                            est_mean_CI_emp_length = CI_length(GCTA_main, upper = upper, lower = lower),
                                            est_mean_CI_z_length = 2 * sd(GCTA_main, na.rm = TRUE)*z_p), by = .(n,rho_e, p,pro)] %>% setorder(., rho_e,n,p,pro)

sub_summary_result_GCTA_i <- sub_result[, .(sub_est_mean = mean(sub_GCTA_main, na.rm = TRUE),
                                                  sub_est_var_jack = jack_var(sub_GCTA_main, pro = pro),
                                                  sub_est_mean_CI_z_length = sub_CI_lenght(sub_GCTA_main, pro = pro,z_p = z_p),
                                                  sub_est_mean_CI_z_coverage = sub_coverage_rate_z(sub_GCTA_main, 10, upper = upper, lower = lower, pro = pro)), 
                                              by = .(n,rho_e, p,i,pro)] %>% setorder(., rho_e,n,p,i,pro)

sub_summary_result_GCTA <- sub_summary_result_GCTA_i[, lapply(.SD, mean), by = .(n,rho_e, p,pro)][,i:=NULL]
summary_final_GCTA <- merge(summary_result_GCTA, sub_summary_result_GCTA, by = c("n","rho_e","p","pro"))
summary_final_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_final_least, summary_final_GCTA), fill = TRUE)
summary_final[,rho_e:= as.character(rho_e)]
summary_final[, var_diff_ratio := (sub_est_var_jack - est_var)/est_var]
summary_final[, CI_diff_ratio := (sub_est_mean_CI_z_length - est_mean_CI_z_length)/sub_est_mean_CI_z_length]
summary_final <- rbindlist(list(s0,s1)) %>% setorder(.,method,dist,n)
# make the latex code
summary_final_1 <- summary_final
summary_final <- rbindlist(list(summary_final, summary_final_1))
summary_final %>% setorder(.,method, pro)

kable(summary_final, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("scale_down"))

# make plots 
save_path <- "~/dev/GCTA-on-Environmental-data/draft/compare EigenPrism and GCTA method/fig/"
mean_plot_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = est_mean, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Mean estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"mean_plot_Eg_GCTA_Chi.eps"), plot = mean_plot_Eg_GCTA, dpi = 1200)

var_plot_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = est_var, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Var estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"var_plot_Eg_GCTA_Chi.eps"), plot = var_plot_Eg_GCTA, dpi = 1200)


mean_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = sub_est_mean, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Mean estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"mean_plot_sub_Eg_GCTA_Chi.eps"), plot = mean_plot_sub_Eg_GCTA, dpi = 1200)

var_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = var_diff_ratio, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Var estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"var_plot_sub_Eg_GCTA_Chi.eps"), plot = var_plot_sub_Eg_GCTA, dpi = 1200)


CI_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = CI_diff_ratio, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("CI widths estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"CI_plot_sub_Eg_GCTA_Chi.eps"), plot = CI_plot_sub_Eg_GCTA, dpi = 1200)

coverage_rate_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = sub_est_mean_CI_z_coverage, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  geom_hline(yintercept = 0.8) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Coverage rate of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"rate_plot_sub_Eg_GCTA_Chi.eps"), plot = coverage_rate_plot_sub_Eg_GCTA, dpi = 1200)


result_path <- result_list <- "result_list_fixed_sub_chi_structure_I_main_0.5_inter_0_n_500_5000_p_50_rho_e_0.5_dim_red_coeff__subpro_0.5_iter_1000_nsub_200_least_square_kernel_e_GCTA_kernel_est_main"
file_list_all <- list.files(paste0("./", result_path, "/")) %>% paste0(paste0("./", result_path, "/"),.)
file_list <- file_list_all[grep(x = file_list_all, pattern = "iteration",perl = TRUE)]
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
summary_final_EigenPrism <- merge(summary_result_EigenPrism, sub_summary_result_EigenPrism, by = c("n","rho_e","p"))
summary_final_EigenPrism[,method := "EigenPrism"]

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
summary_final_GCTA <- merge(summary_result_GCTA, sub_summary_result_GCTA, by = c("n","rho_e","p"))
summary_final_GCTA[,method:= "GCTA"]
summary_final <- rbindlist(list(summary_final_EigenPrism, summary_final_GCTA), fill = TRUE)
summary_final[,rho_e:= as.character(rho_e)]
summary_final[, var_diff_ratio := (sub_est_var - est_var)/est_var]
summary_final[, CI_diff_ratio := (sub_est_mean_CI_z_length - est_mean_CI_z_length)/sub_est_mean_CI_z_length]
# make plots 
save_path <- "~/dev/GCTA-on-Environmental-data/draft/compare EigenPrism and GCTA method/fig/"
mean_plot_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = est_mean, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Mean estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"mean_plot_Eg_GCTA_Chi.eps"), plot = mean_plot_Eg_GCTA, dpi = 1200)

var_plot_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = est_var, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Var estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"var_plot_Eg_GCTA_Chi.eps"), plot = var_plot_Eg_GCTA, dpi = 1200)


mean_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = sub_est_mean, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Mean estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"mean_plot_sub_Eg_GCTA_Chi.eps"), plot = mean_plot_sub_Eg_GCTA, dpi = 1200)

var_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = var_diff_ratio, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Var estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"var_plot_sub_Eg_GCTA_Chi.eps"), plot = var_plot_sub_Eg_GCTA, dpi = 1200)


CI_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = CI_diff_ratio, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("CI widths estimation of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"CI_plot_sub_Eg_GCTA_Chi.eps"), plot = CI_plot_sub_Eg_GCTA, dpi = 1200)

coverage_rate_plot_sub_Eg_GCTA <- summary_final %>%
  ggplot(., aes(x = n, y = sub_est_mean_CI_z_coverage, group = interaction(method, rho_e))) +
  geom_line(aes(color = method, linetype = rho_e)) +
  geom_point(aes(color = method)) +
  geom_hline(yintercept = 0.8) +
  facet_wrap_paginate(facets = vars(p), ncol = 3 ,nrow = 1, scales = "free", labeller  = "label_both", page = 1) +
  ggtitle("Coverage rate of EigenPrism and GCTA with Chi-square") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggsave(filename = paste0(save_path,"rate_plot_sub_Eg_GCTA_Chi.eps"), plot = coverage_rate_plot_sub_Eg_GCTA, dpi = 1200)
