options(warn = 1, error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn",modifiedOnly = FALSE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns


set.seed(1234)
beta <- rnorm(1)
seed_loop <- 1234
brep <- 1000
n <- 1000
n_sub <- 1000
pro <- 101
bs <- "leave-1"
cores <-10

if (cores == 1){
  foreach::registerDoSEQ() 
} else {
  doParallel::registerDoParallel(cores = cores) # setting cores
} 

result_raw <- foreach(ibrep = 1:brep, .verbose = TRUE, .errorhandling = "remove", .options.RNG = seed_loop) %dorng%   {
  result_tmp <- matrix(0,nrow = n_sub, ncol = 7)
  result_tmp[,1] <- 10
  result_tmp[,2] <- 5
  # generate raw data 
  X <- rnorm(n, 10,sqrt(5))^2 %>% as.matrix(.)
  result_tmp[,3] <- mean(X)
  result_tmp[,4] <- var(X)
  # generate sub_sampling effects
  for(i in 1:n_sub){
    sub_data <- generate_sub(data = list(X = X), 
                             pro = pro,
                             bs = bs,
                             n = length(X),
                             iteration = i)
    sub_X <- sub_data$X
    result_tmp[i,5] <- mean(sub_X)
    result_tmp[i,6] <- var(sub_X)
  }
  result_tmp[,7] <- ibrep
  result_tmp %>% data.table(.)
} %>% rbindlist(.)
names(result_raw) <- c("true_mean","true_variance", "est_mean","est_var","sub_mean","sub_var","i")

jack_var <- function(x, pro = 0.5){
  x_m <- mean(x, na.rm = T)
  var_1 <- (x_m - x)^2 %>% sum(., na.rm = T)
  if(pro == 101){
    var_2 <-  (length(x) -1) * 1/length(x) * var_1
  } else {
    var_2 <- pro/(1-pro) * 1/length(x) * var_1
  }
  var_2
}
result <- result_raw[,.(est_mean = mean(est_mean),
                        est_var = mean(est_var),
                        sub_mean = mean(sub_mean),
                        mean_jack_var = jack_var(sub_mean, pro = pro),
                        sub_var = mean(sub_var),
                        var_jack_var = jack_var(sub_var, pro = pro)), by = i]
result[,c("est_mean_var", "est_var_var") := list(var(est_mean), var(est_var))]
result[, .(mean_jack_var = mean(mean_jack_var),
           est_mean_var = mean(est_mean_var),
           var_jack_var = mean(var_jack_var),
           est_var_var = mean(est_var_var))]
