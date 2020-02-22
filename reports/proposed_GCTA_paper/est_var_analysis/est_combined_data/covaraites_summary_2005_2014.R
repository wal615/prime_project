library(sas7bdat)
library(SASxport)
library(ggplot2)
library(data.table)
library(tidyverse)
library(R.utils)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")

name_list <- list.files(path = "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/pooled/",
                        pattern = "PCB",full.names = T)
PCB_2005_2014_raw <- lapply(name_list, FUN = fread) %>% rbindlist(., fill = TRUE, use.names = TRUE)

### summary the data 

# common PCBs

PCB_name_list <- list()
for(i in 1:length(name_list)){
  x <- fread(name_list[i])
  PCB_name <- colnames(x) %>% grep(pattern = "(PCB)*\\d$", x = .,perl = T, value = T)
  PCB_name_list[[i]] <- PCB_name
}
names(PCB_name_list) <- seq(2005,2013,2)
# find the common PCBs
PCB_common <- Reduce(intersect, PCB_name_list) %>% sort(.)
select_index <- c("SAMPLEID","RIAGENDR","SDDSRVYR",PCB_common)
PCB_2005_2014_common <- PCB_2005_2014_raw[,..select_index] %>% na.omit(.)

# main
cov_2005_2014 <- cov(PCB_2005_2014_common[,..PCB_common])
PCB_common_2005 <- PCB_common

# add interaction terms 
main <- PCB_2005_2014_common[,..PCB_common]
main_inter <- model.matrix(~.+.*.+0, main)
Combined_PCB_common <- colnames(main_inter)
Combined_PCB_2005_2014_common <- cbind(PCB_2005_2014_common[,c("SAMPLEID","RIAGENDR","SDDSRVYR")], main_inter)
Combined_cov_2005_2014 <- cov(Combined_PCB_2005_2014_common[,..Combined_PCB_common])
Combined_PCB_common_2005 <- Combined_PCB_common
