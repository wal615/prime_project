# This file is for create and mentain a R package related to the PRIME project
# inital the package ####

# create the the folder 
# install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
setwd("~/dev/prime_project/")
create("prime.total.effect")

# create the document 
setwd("~/dev/prime.total.effect/")
document()

# install the develop version of the prime.total.effect package 
install("~/dev/prime.total.effect/")
library(prime.total.effect)

# create help documents 
setwd("~/dev/prime.total.effect/")
devtools::use_vignette(name = "Introduction")
devtools::use_vignette(name = "Master")
devtools::use_vignette(name = "Summary")
# Add the modified vignette into the package
setwd("~/dev/prime.total.effect/")
devtools::build_vignettes()

# adding the dataset #### 
# setwd("~/dev/prime.total.effect/")
# create a folder for raw data
# devtools::use_data_raw() 
# add the raw data into the data-raw folder
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
hemoglobin_data_sas <- read.sas7bdat("./real_data/NHANES/subset_data_based_on_hypothesis/hemoglobin/nhance_hemoglobin.sas7bdat") 
hemoglobin_data <- hemoglobin_data_sas %>% data.table(.)
# select the PCB exposure and outcome
outcome_name <- c("LBXGH")
selected_col <- c(outcome_name, name_PCB, name_PCB_LC, name_other_variables)
hemoglobin_data_selected <- hemoglobin_data[,..selected_col]

# replace all the PCB related exposures' name to PCB
colnames(hemoglobin_data_selected)[-1] <-  gsub(pattern = "^(LBX|LBD)", 
                                                replacement = "PCB", 
                                                x = colnames(hemoglobin_data_selected)[-1],
                                                perl = TRUE)
write.csv(hemoglobin_data_selected, 
          file = "~/dev/prime.total.effect/data-raw/hemoglobin_data.csv",
          row.names = F)
# add the data from the raw-data
source("~/dev/prime.total.effect/data-raw/hemoglobin_PCBs.R")

