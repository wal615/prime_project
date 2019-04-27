library(tidyverse)
library(data.table)
library(mice)
library(VIM)
setwd("~/dev/projects/Chen_environmental_study/R_code/data/")

# PCB_and_LC <- read.csv("~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/hemoglobin/nhance_hemoglobin_PCB_LC.csv", header = TRUE, stringsAsFactors = FALSE) %>%
#   data.table(.)
# # remove the LC part
# PCB <- PCB_and_LC[,(PCB_LC):=NULL]

# using mice pattern function to investiage the pattern of missing data

# missing_pattern <- md.pattern(PCB[,!("LBXGH")], rotate.names = TRUE)
# aggr_plot <- aggr(PCB[,!("LBXGH")], numbers=TRUE, sortVars=TRUE, labels=names(PCB[,!("LBXGH")]), ylab=c("Histogram of missing data","Pattern"))

imputed_hemoglobin <- read.csv("./real_data/NHANES/hemoglobin/nhanceImputedNormalScore.csv", stringsAsFactors = FALSE) %>% 
                       data.table(.)
names(imputed_hemoglobin)[1:2] <- c("index", "LBXGH")

# remove the "fcol"
imputed_hemoglobin <- imputed_hemoglobin[index != "fcol",]

# change the outcome to numerical
imputed_hemoglobin$LBXGH <- as.numeric(imputed_hemoglobin$LBXGH)

# save the data 
imputed_hemoglobin_tmp <- imputed_hemoglobin[imputed_hemoglobin$index ==0][,"index":=NULL]

write.csv(imputed_hemoglobin_tmp, "./real_data/NHANES/hemoglobin/imputed_hemoglobin_0.csv", row.names = FALSE)

imputed_hemoglobin_tmp <- imputed_hemoglobin[imputed_hemoglobin$index ==1][,"index":=NULL]

write.csv(imputed_hemoglobin_tmp, "./real_data/NHANES/hemoglobin/imputed_hemoglobin_1.csv", row.names = FALSE)

imputed_hemoglobin_tmp <- imputed_hemoglobin[imputed_hemoglobin$index ==2][,"index":=NULL]

write.csv(imputed_hemoglobin_tmp, "./real_data/NHANES/hemoglobin/imputed_hemoglobin_2.csv", row.names = FALSE)

imputed_hemoglobin_tmp <- imputed_hemoglobin[imputed_hemoglobin$index ==3][,"index":=NULL]

write.csv(imputed_hemoglobin_tmp, "./real_data/NHANES/hemoglobin/imputed_hemoglobin_3.csv", row.names = FALSE)

imputed_hemoglobin_tmp <- imputed_hemoglobin[imputed_hemoglobin$index ==4][,"index":=NULL]

write.csv(imputed_hemoglobin_tmp, "./real_data/NHANES/hemoglobin/imputed_hemoglobin_4.csv", row.names = FALSE)

imputed_hemoglobin_tmp <- imputed_hemoglobin[imputed_hemoglobin$index ==5][,"index":=NULL]

write.csv(imputed_hemoglobin_tmp, "./real_data/NHANES/hemoglobin/imputed_hemoglobin_5.csv", row.names = FALSE)

########################################################################################################################################

imputed_hemoglobin <- read.csv("./real_data/NHANES/hemoglobin/nhanceMiceImpute10.csv", stringsAsFactors = FALSE) %>% 
  data.table(.)
names(imputed_hemoglobin)[1] <- "LBXGH"
write.csv(imputed_hemoglobin, file = "./real_data/NHANES/hemoglobin/nhanceMiceImpute10.csv", row.names = FALSE)

imputed_hemoglobin <- read.csv("./real_data/NHANES/hemoglobin/nhanceMiceImpute25.csv", stringsAsFactors = FALSE) %>% 
  data.table(.)
names(imputed_hemoglobin)[1] <- "LBXGH"
write.csv(imputed_hemoglobin, file = "./real_data/NHANES/hemoglobin/nhanceMiceImpute25.csv", row.names = FALSE)
