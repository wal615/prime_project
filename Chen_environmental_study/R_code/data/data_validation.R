library(sas7bdat)
library(SASxport)
library(tidyverse)

setwd("~/dev/projects/Chen_environmental_study/R_code/data/")
pops <- read.sas7bdat("./pops_abc.sas7bdat")
pcb_2000 <- read.xport("./LAB28POC_99_00.XPT")

pcb_Chen <- read.sas7bdat("../pcbs1000nomiss.sas7bdat")
pcb_Chen <- pcb_Chen[,-1] # remove the subject ID

# compare the pops and the PCb_chen 

# select overlap columns 
col_overlap <- match(colnames(pcb_Chen), colnames(pops))

# missing columns 
colnames(pcb_Chen)[is.na(col_overlap)]
pops_tmp <- pops[,na.omit(col_overlap)]
dim(pops_tmp)

# remove rows which have missing values
pops_no_missing <- pops_tmp[complete.cases(pops_tmp),]
dim(pops_no_missing)

# compare the boxplot for each column
combined_pops_Chen <- 



