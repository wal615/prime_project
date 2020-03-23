setwd("~/dev/prime_project/R_code/data/")
# loading the variables names based on the NHANES code book
source("./real_data/NHANES/subset_data_based_on_hypothesis/variable_names/exposure_name_loading.R")
# loading some helper functions for update LC
source("./real_data/NHANES/subset_data_based_on_hypothesis/LC_update.R")
# loading some required packages
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)
library(foreign)


##########################################################################################
# Are POPs associated with diabetes? #### 
#######################################################################################
diabetes_data_sas <- read.sas7bdat("./real_data/NHANES/subset_data_based_on_hypothesis/diabetes/nhance_diabetes.sas7bdat") 
diabetes_data <- diabetes_data_sas %>% data.table(.)

# select the PCB exposure and outcome
outcome_name <- c("diabetes")
selected_col <- c(outcome_name, name_PCB, name_PCB_LC, name_other_variables)
diabetes_data_selected <- diabetes_data[,..selected_col]

# replace all the PCB related exposures' name to PCB
colnames(diabetes_data_selected) <-  gsub(pattern = "^(LBX|LBD)", 
                                            replacement = "PCB", 
                                            x = colnames(diabetes_data_selected),
                                            perl = TRUE)
name_PCB <- gsub(pattern = "^(LBX|LBD)", 
                 replacement = "PCB", 
                 x = name_PCB,
                 perl = TRUE)

name_PCB_LC <- gsub(pattern = "^(LBX|LBD)", 
                    replacement = "PCB", 
                    x = name_PCB_LC,
                    perl = TRUE)

name_PCB_LA <- gsub(pattern = "^(LBX|LBD)", 
                    replacement = "PCB", 
                    x = name_PCB_LA,
                    perl = TRUE)

name_PCB_all <- gsub(pattern = "^(LBX|LBD)", 
                     replacement = "PCB", 
                     x = name_PCB_all,
                     perl = TRUE)

# raw data with only response and PCB 
# nhance_diabetes_PCB.csv
selected_col <- c(outcome_name, name_PCB)
diabetes_data <- diabetes_data_selected[,.SD, .SDcols = selected_col]

# number of observations has at least non-missing PCB 
index <- !(is.na(diabetes_data[,-1]))
apply(index, MARGIN = 1, any) %>% sum(.)
# number of observations has all PCB without any missing
dim(na.omit(diabetes_data))[1]
# table of diabetes
table(diabetes_data$diabetes)

write.csv(diabetes_data, file = "./real_data/NHANES/subset_data_based_on_hypothesis/diabetes/nhance_diabetes_PCB.csv", row.names = FALSE)
