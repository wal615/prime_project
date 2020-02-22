library(sas7bdat)
library(SASxport)
library(ggplot2)
library(data.table)

setwd("~/dev/projects/Chen_environmental_study/R_code/data/05_13_PCB/")
PCB_99_04 <- read.csv("../pcb_99_04_no_missing.csv", header = TRUE)
name_list <- list.files("./", pattern = ".XPT")
data_list <- as.list(numeric(length(name_list)))

for(i in 1:length(name_list)) {
  data_tmp <- read.xport(name_list[i])
  colnames(data_tmp) <- gsub(pattern = "LBC", replacement = "LBX", x =colnames(data_tmp), fixed = TRUE)
  col_overlap <- match(colnames(PCB_99_04), colnames(data_tmp))
  data_tmp <- data_tmp[,na.omit(col_overlap)]
  data_tmp_no_missing <- data_tmp[complete.cases(data_tmp),]
  data_tmp <- data.frame(data_tmp, section = name_list[i])
  data_list[[i]] <- data_tmp
  } 

PCB_05_13_no_missing <- rbindlist(data_list,use.names = TRUE, fill = TRUE)
PCB_05_13_no_missing <- PCB_05_13_no_missing[complete.cases(PCB_05_13_no_missing),] # select no column missing rows
PCB_05_13_no_missing <- as.data.frame(PCB_05_13_no_missing)

inter_col <- intersect(colnames(PCB_05_13_no_missing), colnames(PCB_99_04))
PCB_99_13_no_missing <- rbind(PCB_05_13_no_missing[,inter_col], PCB_99_04[,inter_col])
write.csv(PCB_99_13_no_missing, "../pcb_99_13_no_missing.csv")
