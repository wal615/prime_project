setwd("~/dev/projects/NSF_trafic/")
## MetaLonDA
library(MetaLonDA)
library(data.table)
library(tidyverse)
cores <- 20

##load data
t_s <- readRDS(file = "./data/clean_data")

# modify the data set
test <- copy(t_s)
test[, group := ifelse(weekend, "weekend", "weekday")]
test[,time_seq_min := difftime(as.POSIXct(hours, format="%H:%M:%S"), as.POSIXct("00:00:00", format="%H:%M:%S"), units = "mins") %>% as.numeric(.)]
test[,id := as.numeric(date)]
three_month_data <- copy(test[id <= 17226,])

# fit the model 
output.S312425 = metalonda(Count = three_month_data$S312425, Time = three_month_data$time_seq_min, Group = three_month_data$group,
                           ID = three_month_data$id, n.perm = 100, fit.method = "nbinomial", points = three_month_data$time_seq_min,
                           text = "S312425", parall = FALSE, pvalue.threshold = 0.05,
                           adjust.method = "BH", time.unit = "mins", ylabel = "Normalized Count",
                           col = c("black", "green"), prefix = "S312425_three_month_100")
