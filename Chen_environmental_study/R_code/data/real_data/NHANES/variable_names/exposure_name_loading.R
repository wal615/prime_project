# missing data imputation
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)

wd <- getwd()
setwd("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/variable_names/")

# get all the exposures'names 
expousres_type <- read.csv("./exposure_type.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>% 
                  data.table(.)

name_PCB_all<- expousres_type[grepl("^PCB", x = type), exposure]
              

name_PCB_LA <- name_PCB_all[grep(name_PCB_all, pattern = ".*LA$", perl = TRUE)]
name_PCB_LC <- name_PCB_all[grep(name_PCB_all, pattern = ".*LC$", perl = TRUE)]
name_PCB <- name_PCB_all[-grep(name_PCB_all, pattern = ".*(LC|LA)$", perl = TRUE)]
length(name_PCB_all) == length(name_PCB_LA) + length(name_PCB_LC) + length(name_PCB)



# get all other_variables
other_varibles <- read.csv("./other_variable.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>% 
                  data.table(.)

name_other_variables <- other_varibles[,Variable]

setwd(wd)