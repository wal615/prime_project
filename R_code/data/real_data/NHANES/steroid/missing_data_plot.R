options(error = bettertrace::stacktrace)
setwd("~/dev/projects/Chen_environmental_study/")
R.utils::sourceDirectory("./R_code/main_fn", modifiedOnly = FALSE)
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/steroid/"
source("./R_code/simulation_proposed_GCTA/local_helpers.R")
source("./R_code/data/real_data/NHANES/exposure_name_loading.R")
source("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/exposure_name_loading.R")
library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)
library(gtools) # for rbind based on columns
library(mice)
library(VIM)

# data pre-process and generating 
# steroid 
PCB_and_LC <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/steroid/nhance_steroid_PCB_LC.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  data.table(.)

# check for all empty columns and removed
all_empty_col <- sapply(PCB_and_LC, FUN = function(x) sum(is.na(x)) == length(x), simplify = TRUE)
PCB_and_LC <- PCB_and_LC[,!all_empty_col, with = FALSE] %>% as.data.table(.)
PCB <- PCB[!all_empty_col]

# mssing pattern for a single PCB
missing_pattern <- md.pattern(PCB_and_LC[,.(LBD199, LBD199LC)], rotate.names = TRUE)
aggr_plot <- aggr(PCB_and_LC[,.(LBD199, LBD199LC)], numbers=TRUE, sortVars=TRUE, labels=names(PCB_and_LC[,.(LBD199, LBD199LC)]), ylab=c("Histogram of missing data","Pattern"))


# missing data pattern for all PCB
missing_pattern <- md.pattern(PCB_and_LC[,PCB[1:10],with = FALSE], rotate.names = TRUE)
aggr_plot <- aggr(PCB_and_LC[,..(PCB[1:10])], numbers=TRUE, sortVars=TRUE, labels=names(PCB_and_LC[,..(PCB[1:10])]), ylab=c("Histogram of missing data","Pattern"))

