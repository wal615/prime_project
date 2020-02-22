library(sas7bdat)
library(SASxport)
library(ggplot2)
library(data.table)
library(tidyverse)

setwd("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/raw/individual/")
name_list <- list.files(pattern = "PCB",full.names = T)
data_list <- as.list(numeric(length(name_list)))

for(i in 1:length(name_list)) {
  # for PCBs for 2003-2004 there are two PCBs files so we need to combine them
  if("2003" %in% strsplit(name_list[i], split = "_")[[1]]){
    x <- read.xport("./PCB_2003_2004.XPT") %>% data.table(.)
    y <- read.xport("./addtional_2003_2004.XPT") %>% data.table(.)
    data_tmp <- merge(x,y)
  } else {
    data_tmp <- read.xport(name_list[i]) %>% data.table(.)
    
  }
  
  # get demo data 
  demo_tmp <- read.xport(name_list[i] %>% gsub(pattern = "PCB",replacement = "DEMO" )) %>% data.table(.)
  demo_tmp <- demo_tmp[,.(SEQN, RIAGENDR, RIDRETH = RIDRETH1, SDDSRVYR)]
  
  # unify the PCB's names
  colnames(data_tmp) <- gsub(pattern = "LBC", replacement = "LBX", x = colnames(data_tmp), fixed = TRUE)
  
  # select only PCBs
  select_index <- grep(pattern = "(LBD|LBX)\\d", colnames(data_tmp),perl = T,value = T)
  select_index <- c("SEQN", select_index)
  data_tmp <- data_tmp[,..select_index]
  
  # merge with demo info
  data_m <- merge(data_tmp, demo_tmp, by = "SEQN", all.x = T) 
  
  colnames(data_m) <- gsub(x = colnames(data_m), pattern = "(LBD|LBX)", replacement = "PCB")
  write.csv(x = data_m,
            file = paste0("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual/PCB", "_", strsplit(name_list[i], "_")[[1]][2], "_clean.csv"),
            row.names = F)
} 





#############
## 2005 -2014
#############


setwd("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/raw/pooled/")
name_list <- list.files(pattern = "PCB",full.names = T)
data_list <- as.list(numeric(length(name_list)))

for(i in 1:length(name_list)) {
  data_tmp <- read.xport(name_list[i]) %>% data.table(.)
  # unify the PCB's names
  colnames(data_tmp) <- gsub(pattern = "LBC", replacement = "LBX", x = colnames(data_tmp), fixed = TRUE)
  colnames(data_tmp) <- gsub(pattern = "RIDRETH\\d$", replacement = "RIDRETH", x = colnames(data_tmp), perl = TRUE)
  colnames(data_tmp) <- gsub(pattern = "WTBSMSMA", replacement = "WTSMSMPA", x = colnames(data_tmp), perl = TRUE)
  # select only PCBs
  select_index <- grep(pattern = "(LBD|LBX)\\d", colnames(data_tmp),perl = T,value = T)
  if("RIDRETH" %in% colnames(data_tmp)){
    select_index <- c("SAMPLEID", "RIDRETH", "RIDAGGRP", "RIAGENDR", "RIANSMP",  "WTSMSMPA",
                      select_index)
  } else {
    select_index <- c("SAMPLEID", "RIDAGGRP", "RIAGENDR", "RIANSMP",  "WTSMSMPA",
                      select_index)
  }
  
  # get realse year from demo data 
  demo_tmp <- read.xport(name_list[i] %>% gsub(pattern = "PCB",replacement = "DEMO" )) %>% data.table(.)
  SDDSRVYR <- demo_tmp[1,SDDSRVYR] %>% as.numeric(.)
  
  data_tmp <- data_tmp[,..select_index]
  data_tmp[, SDDSRVYR := SDDSRVYR]
  
  colnames(data_tmp) <- gsub(x = colnames(data_tmp), pattern = "(LBD|LBX)", replacement = "PCB")
  write.csv(x = data_tmp,
            file = paste0("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/pooled/PCB", "_", strsplit(name_list[i], "_")[[1]][2], "_clean_P.csv"),
            row.names = F)
}

