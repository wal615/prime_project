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
# Are POPs associated with hemoglobin A1c in non-diabetics? #### 
#######################################################################################
hemoglobin_data_sas <- read.sas7bdat("./real_data/NHANES/subset_data_based_on_hypothesis/hemoglobin/nhance_hemoglobin.sas7bdat") 
hemoglobin_data <- hemoglobin_data_sas %>% data.table(.)

# select the PCB exposure and outcome
outcome_name <- c("LBXGH")
selected_col <- c(outcome_name, name_PCB, name_PCB_LC, name_other_variables)
hemoglobin_data_selected <- hemoglobin_data[,..selected_col]

# change the outcome name
colnames(hemoglobin_data_selected)[colnames(hemoglobin_data_selected) %in% outcome_name] <- "outcome_LBXGH"

# replace all the PCB related exposures' name to PCB
colnames(hemoglobin_data_selected) <-  gsub(pattern = "^(LBX|LBD)", 
                                            replacement = "PCB", 
                                            x = colnames(hemoglobin_data_selected),
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

# 0. remove all the NA response # 1623 rows removed
hemoglobin_data_tmp <- hemoglobin_data_selected[!(is.na(LBXGH)),]
(nrow(hemoglobin_data_selected) - nrow(hemoglobin_data_tmp)) %>% cat(., "row removed")

# 1. remove all empty PCB and PCBLC row, # 8351 rows removed
n1 <- nrow(hemoglobin_data_tmp)
selected_col <- c(name_PCB, name_PCB_LC)
index <- hemoglobin_data_tmp[,rowSums(is.na(.SD)) == length(selected_col), .SDcols = selected_col]
hemoglobin_data_tmp <- hemoglobin_data_tmp[!index,]
(n1 - nrow(hemoglobin_data_tmp)) %>% cat(., "row removed")

# 2. recover the limit of detection and generate the comprehensive table
name_PCB_with_LC <- gsub(pattern = "LC", 
                         replacement = "", 
                         x = name_PCB_LC,
                         perl = TRUE)
update_LC(data = hemoglobin_data_tmp, PCB_names = name_PCB_with_LC)
# 2.1 remove all the PCB_LC 
hemoglobin_data_tmp[,(name_PCB_LC) := NULL]

# 3 check the special values for indicating missing value for the confonder variables  
special_value_confonder <- list()
for(n in name_other_variables){
  special_value_confonder <- append(special_value_confonder, list(hemoglobin_data_tmp[,table(eval(parse(text = n)), exclude = NULL)])) 
}
names(special_value_confonder) <- name_other_variables

# capture.output(print(special_value_confonder),
#                file = "./real_data/NHANES/hemoglobin/special_value_confonder.txt")

# 3.1 indicator variable for confonders
add_missing_indicator(data = hemoglobin_data_tmp, var = name_other_variables)

# 4 reorder the name
sorted_name <- sort(colnames(hemoglobin_data_tmp))
hemoglobin_data_tmp <- hemoglobin_data_tmp[, ..sorted_name]

# # 5 save output as csv
# write.csv(hemoglobin_data_tmp, file = "./real_data/NHANES/hemoglobin/nhance_hemoglobin_PCB_LC_comp.csv", row.names = FALSE)
# 
# # 5 output as sas file
# write.foreign(df=hemoglobin_data_tmp, datafile="./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB_LC_comp.csv", codefile="./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB_LC_comp.sas", package="SAS")

# raw data with only response and PCB 
# nhance_hemoglobin_PCB.csv
# outcome_name <- c("LBXGH")
# selected_col <- c(outcome_name, name_PCB)
# hemoglobin_data <- hemoglobin_data[,.SD, .SDcols = selected_col]
# hemoglobin_data_tmp <- hemoglobin_data[rowSums(is.na(hemoglobin_data)) < ncol(hemoglobin_data),]



##########################################################################################
# Are POPs associated with diabetes prevalence A1c in non-diabetics? #### 
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

# 0. remove all the NA response # 2 rows removed
diabetes_data_tmp <- diabetes_data_selected[!(is.na(diabetes)),]
(nrow(diabetes_data_selected) - nrow(diabetes_data_tmp)) %>% cat(., "row removed")

# 1. remove all empty PCB and PCBLC row, # 11163 rows removed
n1 <- nrow(diabetes_data_tmp)
selected_col <- c(name_PCB, name_PCB_LC)
index <- diabetes_data_tmp[,rowSums(is.na(.SD)) == length(selected_col), .SDcols = selected_col]
diabetes_data_tmp <- diabetes_data_tmp[!index,]
(n1 - nrow(diabetes_data_tmp)) %>% cat(., "row removed")

# 2. recover the limit of detection and generate the comprehensive table
name_PCB_with_LC <- gsub(pattern = "LC", 
                         replacement = "", 
                         x = name_PCB_LC,
                         perl = TRUE)
update_LC(data = diabetes_data_tmp, PCB_names = name_PCB_with_LC)
# 2.1 remove all the PCB_LC 
diabetes_data_tmp[,(name_PCB_LC) := NULL]

# 3 check the special values for indicating missing value for the confonder variables  
special_value_confonder <- list()
for(n in name_other_variables){
  special_value_confonder <- append(special_value_confonder, list(diabetes_data_tmp[,table(eval(parse(text = n)), exclude = NULL)])) 
}
names(special_value_confonder) <- name_other_variables

# capture.output(print(special_value_confonder),
#                file = "./real_data/NHANES/diabetes/special_value_confonder.txt")

# 3.1 indicator variable for confonders
add_missing_indicator(data = diabetes_data_tmp, var = name_other_variables)

# 4 reorder the name
sorted_name <- sort(colnames(diabetes_data_tmp))
diabetes_data_tmp <- diabetes_data_tmp[, ..sorted_name]

# # 5 save output as csv
# write.csv(diabetes_data_tmp, file = "./real_data/NHANES/diabetes/nhance_diabetes_PCB_LC_comp.csv", row.names = FALSE)
# 
# # 5 output as sas file
# write.foreign(df=diabetes_data_tmp, datafile="./real_data/NHANES/diabetes/SAS_nhance_diabetes_PCB_LC_comp.csv", codefile="./real_data/NHANES/diabetes/SAS_nhance_diabetes_PCB_LC_comp.sas", package="SAS")

# raw data with only response and PCB 
# nhance_diabetes_PCB.csv
# outcome_name <- c("LBXGH")
# selected_col <- c(outcome_name, name_PCB)
# diabetes_data <- diabetes_data[,.SD, .SDcols = selected_col]
# diabetes_data_tmp <- diabetes_data[rowSums(is.na(diabetes_data)) < ncol(diabetes_data),]

write.csv(diabetes_data_selected, file = "./real_data/NHANES/subset_data_based_on_hypothesis/diabetes/nhance_diabetes_PCB.csv", row.names = FALSE)
