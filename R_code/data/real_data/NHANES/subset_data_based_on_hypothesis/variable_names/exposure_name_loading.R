# missing data imputation
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)

# The following R script is to get the variable information based on the code book file 
# ohter_variables are confonder variables like gender and ages which are get from the code book file 
# exposure_type are PCBs which are also get from the code book file 

# get all the exposures'names 
expousres_type <- read.csv("./real_data/NHANES/subset_data_based_on_hypothesis/variable_names/exposure_type.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>% 
                  data.table(.)

# select the columns that related to the PCBs
name_PCB_all<- expousres_type[grepl("^PCB", x = type), exposure]

# check if all the columns numbers are matched                
name_PCB_LA <- name_PCB_all[grep(name_PCB_all, pattern = ".*LA$", perl = TRUE)]
name_PCB_LC <- name_PCB_all[grep(name_PCB_all, pattern = ".*LC$", perl = TRUE)]
name_PCB <- name_PCB_all[-grep(name_PCB_all, pattern = ".*(LC|LA)$", perl = TRUE)]
length(name_PCB_all) == length(name_PCB_LA) + length(name_PCB_LC) + length(name_PCB)

# get all other_variables (non-PCB)
other_varibles <- read.csv("./real_data/NHANES/subset_data_based_on_hypothesis/variable_names/other_variable.csv", 
                           header = TRUE, 
                           stringsAsFactors = FALSE) %>% 
                  data.table(.)

name_other_variables <- other_varibles[,Variable]
