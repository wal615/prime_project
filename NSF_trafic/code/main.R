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
t_s[, hours := strftime(time_stamp, format="%H%M%S")]
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
select_col <- names(t_s)[1:10]
res_1 <- t_s[, lapply(.SD, FUN = mean, na.rm = T),.SDcol = select_col, by = .(hours, weekend)] %>% copy(.)
