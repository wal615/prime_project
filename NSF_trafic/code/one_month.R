setwd("~/dev/projects/NSF_trafic/")
## MetaLonDA
library(MetaLonDA)
library(data.table)
library(tidyverse)
# options(warn = 2, error = bettertrace::stacktrace)
options(warn = 0, error = recover)
cores <- 20

##load data
t_s <- readRDS(file = "./data/clean_data")

# modify the data set
test <- copy(t_s)
test[, group := ifelse(weekend, "weekend", "weekday")]
test[,time_seq_min := difftime(as.POSIXct(hours, format="%H:%M:%S"), as.POSIXct("00:00:00", format="%H:%M:%S"), units = "mins") %>% as.numeric(.)]
test[,id := as.numeric(date)]

# subset 
test <- test[time_seq_min %% 10 == 0,]
data <- test[id <= (17167 + 360) ,]


# fit the model 
output.S312425 = metalonda(Count = data$S312425, Time = data$time_seq_min, Group = as.factor(data$group),
                           ID = as.factor(data$id), n.perm = 50, fit.method = "nbinomial", points = data$time_seq_min,
                           text = "S312425", parall = FALSE, pvalue.threshold = 0.05,
                           adjust.method = "BH", time.unit = "mins", ylabel = "Normalized Count",
                           col = c("black", "green"), prefix = "S312425_one_month_prem_50_10_mins_360_days")
