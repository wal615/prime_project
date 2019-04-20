# missing data imputation
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)

# got all the exposures'names 
expousres_type <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/exposure_type.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>% 
                  data.table(.)

name_PCB_all<- expousres_type[grepl("^PCB", x = type), exposure] %>% 
               gsub(pattern = "LBX|LBD", replacement = "PCB", x = ., perl = TRUE)

name_PCB_LA <- name_PCB_all[grep(name_PCB_all, pattern = ".*LA$", perl = TRUE)]
name_PCB_LC <- name_PCB_all[grep(name_PCB_all, pattern = ".*LC$", perl = TRUE)]
name_PCB <- name_PCB_all[-grep(name_PCB_all, pattern = ".*(LC|LA)$", perl = TRUE)]
length(name_PCB_all) == length(name_PCB_LA) + length(name_PCB_LC) + length(name_PCB)



