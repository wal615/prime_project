library("R.matlab")
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
t_s <- data.table(loaded_data$station.counts)
colnames(t_s) <-loaded_data$station.ids 
t_s[, time_stamp := loaded_data$station.times]
inc_s <- data.table(duration = loaded_data$inc.duration, time_stamp_start = loaded_data$inc.timestamp, 
                    ids =  loaded_data$inc.station.id)
inc_s[,time_stamp_end := ]
# plot
library(ggplot2)
library(reshape2)
meltdf <- melt(t_s[,c("time_stamp", "S312425","S312520")],id="time_stamp")
ggplot(meltdf,aes(x=time_stamp,y=value,colour=variable,group=variable)) + geom_line()
