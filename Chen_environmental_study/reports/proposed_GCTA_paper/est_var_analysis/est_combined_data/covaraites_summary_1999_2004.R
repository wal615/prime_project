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

name_list <- list.files(path = "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual",
                        pattern = "PCB",full.names = T)
PCB_1999_2004_raw <- lapply(name_list, FUN = fread) %>% rbindlist(., fill = TRUE)

### summary the data 

# common PCBs

PCB_name_list <- list()
for(i in 1:length(name_list)){
  x <- fread(name_list[i])
  PCB_name <- colnames(x) %>% grep(pattern = "(PCB)*\\d$", x = .,perl = T, value = T)
  PCB_name_list[[i]] <- PCB_name
}
names(PCB_name_list) <- c("1999", "2001", "2003")

# find the common PCBs
PCB_common <- Reduce(intersect, PCB_name_list) %>% sort(.)
select_index <- c("SEQN","RIAGENDR","RIDRETH","SDDSRVYR",PCB_common)
PCB_1999_2004_common <- PCB_1999_2004_raw[,..select_index] %>% na.omit(.)

# main effect
cov_1999_2004 <- cov(PCB_1999_2004_common[,..PCB_common])
PCB_common_1999 <- PCB_common


# add interaction terms 
main <- PCB_1999_2004_common[,..PCB_common]
main_inter <- model.matrix(~.+.*.+0, main)
Combined_PCB_common <- colnames(main_inter)
Combined_PCB_1999_2004_common <- cbind(PCB_1999_2004_common[,c("SEQN","RIAGENDR","RIDRETH","SDDSRVYR")], main_inter)
Combined_cov_1999_2004 <- cov(Combined_PCB_1999_2004_common[,..Combined_PCB_common])
Combined_PCB_common_1999 <- Combined_PCB_common

