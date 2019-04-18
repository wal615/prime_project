# missing data imputation
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)

# got all the exposures'names 
expousres_type <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/exposure_type.csv", header = TRUE, stringsAsFactors = FALSE) %>% data.table(.)
PCB_all<- expousres_type[grepl("^PCB", x = type), exposure]
PCB_LA <- PCB_all[grep(PCB_all, pattern = ".*LA$", perl = TRUE)]
PCB_LC <- PCB_all[grep(PCB_all, pattern = ".*LC$", perl = TRUE)]
PCB <- PCB_all[-grep(PCB_all, pattern = ".*(LC|LA)$", perl = TRUE)]
length(PCB_all) == length(PCB_LA) + length(PCB_LC) + length(PCB)



