library("R.matlab")
library(data.table)
library(tidyverse)


setwd("~/dev/projects/NSF_trafic/")
# load data from .mat file
data_path = "./data/24-Oct-2018_data.mat"
loaded_data <- invisible(readMat(data_path))

# remove unnecessary nesting of lists
for(i in 1:length(loaded_data)) {
  loaded_data[[i]] <- unlist(loaded_data[[i]])
}

# convert string datetime into POSIXct
loaded_data$station.times <- as.POSIXct(loaded_data$station.times,format="%d-%b-%Y %H:%M:%S")
loaded_data$inc.timestamp <- as.POSIXct(loaded_data$inc.timestamp,format="%d-%b-%Y %H:%M:%S")

# transform the data from a list into a data.table 
## sensor data
t_s <- data.table(loaded_data$station.counts)
colnames(t_s) <-loaded_data$station.ids 
t_s[, time_stamp := loaded_data$station.times]
t_s[, date := as.Date(time_stamp)]
t_s[, hours := strftime(time_stamp, format="%H:%M:%S")]
## Incidents data 
inc_s <- data.table(duration = loaded_data$inc.duration, time_stamp_start = loaded_data$inc.timestamp, 
                    ids =  loaded_data$inc.station.id)
inc_s[,time_stamp_end := time_stamp_start + duration.V1 ]

# define the holiday
library(chron)
library(timeDate)
hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay")        
myholidays  <- dates(as.character(c(holiday(2017,hlist),holiday(2018,hlist))),format="Y-M-D")
t_s[,holiday := is.holiday(as.Date(time_stamp),myholidays)]
t_s[,weekend := is.weekend(as.Date(time_stamp))]
# is.holiday(as.Date("2017-11-23"),myholidays)
# is.weekend(.)

# # plot
# library(ggplot2)
# library(reshape2)
# meltdf <- melt(t_s[,c("time_stamp", "S312425","S312520")],id="time_stamp")
# ggplot(meltdf,aes(x=time_stamp,y=value,colour=variable,group=variable)) + geom_line()

# calculate the mean and median for each holiday to see if there is any difference
# select_col <- names(t_s)[1:10]
# res_weekend_mean <- t_s[, lapply(.SD, FUN = median, na.rm = T),.SDcol = select_col, by = .(hours, weekend)] %>% copy(.) %>% 
#            gather(data = ., key = key, value = value, contains("S3"))
# ggplot(res_weekend_mean, aes(x = hours, y = value,
#                       group = interaction(weekend, key),
#                       colour = key, linetype = weekend)) +
#   geom_line() +
#   theme_bw()
# 
# res_weekend_var <- t_s[, lapply(.SD, FUN = function(x) {var(x, na.rm = T)/length(x)}),.SDcol = select_col, by = .(hours, weekend)] %>% copy(.) %>% 
#   gather(data = ., key = key, value = value, contains("S3"))
# ggplot(res_weekend_var, aes(x = hours, y = value,
#                              group = interaction(weekend, key),
#                              colour = key, linetype = weekend)) +
#   geom_line() +
#   theme_bw()
# 
# # calculate the mean and median for each holiday to see if there is any difference
# select_col <- names(t_s)[1:10]
# res_1 <- t_s[weekend == FALSE, lapply(.SD, FUN = mean, na.rm = T),.SDcol = select_col, by = .(hours, holiday)] %>% copy(.) %>% 
#   gather(data = ., key = key, value = value, contains("S3"))
# ggplot(res_1, aes(x = hours, y = value,
#                   group = interaction(holiday, key),
#                   colour = key, linetype = holiday)) +
#   geom_line() +
#   theme_bw()
# 
# res_holiday_var <- t_s[weekend == FALSE, lapply(.SD, FUN = function(x) {var(x, na.rm = T)/length(x)}),.SDcol = select_col, by = .(hours, holiday)] %>% copy(.) %>% 
#   gather(data = ., key = key, value = value, contains("S3"))
# ggplot(res_holiday_var, aes(x = hours, y = value,
#                             group = interaction(holiday, key),
#                             colour = key, linetype = holiday)) +
#   geom_line() +
#   theme_bw()

## MetaLonDA
library(MetaLonDA)

# modify the data set
test <- copy(t_s)
test[, group := ifelse(weekend, "weekend", "weekday")]
test[,time_seq_min := difftime(as.POSIXct(hours, format="%H:%M:%S"), as.POSIXct("00:00:00", format="%H:%M:%S"), units = "mins") %>% as.numeric(.)]
test[,id := as.numeric(date)]
one_month_data <- copy(test[id <= 17197,])

# fit the model 
output.S312425 = metalonda(Count = one_month_data$S312425, Time = one_month_data$time_seq_min, Group = one_month_data$group,
                                ID = one_month_data$id, n.perm = 40, fit.method = "nbinomial", points = one_month_data$time_seq_min,
                                text = "S312425", parall = TRUE, pvalue.threshold = 0.05,
                                adjust.method = "BH", time.unit = "mins", ylabel = "Normalized Count",
                                col = c("black", "green"), prefix = "S312425_one_month")
