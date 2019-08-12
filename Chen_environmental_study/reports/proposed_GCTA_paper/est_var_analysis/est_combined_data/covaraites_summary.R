library(sas7bdat)
library(SASxport)
library(ggplot2)
library(data.table)
library(tidyverse)
setwd("~/dev/projects/Chen_environmental_study/")
sourceDirectory("./R_code/main_fn/",modifiedOnly = FALSE, recursive = TRUE)
sourceDirectory("./R_code/main_fn/method/",modifiedOnly = FALSE, recursive = TRUE)
source("./R_code/simulation_proposed_GCTA/local_helpers.R")

name_list <- list.files(path = "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual",
                        pattern = "PCB",full.names = T)
PCB_1999_2004_raw <- lapply(name_list, FUN = fread) %>% rbindlist(., fill = TRUE)

### summary the data 

# commen PCBs

PCB_name_list <- list()
for(i in 1:length(name_list)){
  x <- fread(name_list[i])
  PCB_name <- colnames(x) %>% grep(pattern = "(PCB)*\\d$", x = .,perl = T, value = T)
  PCB_name_list[[i]] <- PCB_name
}

# find the commen PCBs
PCB_commen <- Reduce(intersect, PCB_name_list)
select_index <- c("SEQN","RIAGENDR","RIDRETH",PCB_commen)
PCB_1999_2004_commen <- PCB_1999_2004_raw[,..select_index] %>% 

# covariates strcuture
pre_cor <- real_data_corr.mat(data_path) 
pre_cor_inv <- inv(pre_cor)
h_map <- function(x) {
  browser()
  h1 <- ggplot(data = melt(x), aes(x=Var1, y=Var2, fill=value)) + 
    scale_fill_gradient(low = "white", high = "blue") +
    geom_tile()  
}

h2 <- ggplot(data = melt(pre_cor>0.9), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
h1_inv <- ggplot(data = melt(pre_cor_inv), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
h2_inv <- ggplot(data = melt(abs(pre_cor_inv)< 0.01), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()