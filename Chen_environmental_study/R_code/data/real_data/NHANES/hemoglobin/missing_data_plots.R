library(tidyverse)
library(data.table)
library(mice)
library(VIM)
setwd("~/dev/projects/Chen_environmental_study/R_code/data/")
source("./real_data/NHANES/exposure_name_loading.R")

PCB_and_LC <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/hemoglobin/nhance_hemoglobin_PCB_LC.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  data.table(.)
# remove the LC part
PCB <- PCB_and_LC[,(PCB_LC):=NULL]

# using mice pattern function to investiage the pattern of missing data

missing_pattern <- md.pattern(PCB[,!("LBXGH")], rotate.names = TRUE)
aggr_plot <- aggr(PCB[,!("LBXGH")], numbers=TRUE, sortVars=TRUE, labels=names(PCB[,!("LBXGH")]), ylab=c("Histogram of missing data","Pattern"))
