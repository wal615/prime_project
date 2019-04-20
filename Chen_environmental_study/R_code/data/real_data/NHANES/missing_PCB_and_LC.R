setwd("~/dev/projects/Chen_environmental_study/R_code/data/")
source("./real_data/NHANES/variable_names/exposure_name_loading.R")
source("./real_data/NHANES/LC_update.R")
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)
library(foreign)
# nhance_data <- read.sas7bdat("./real_data/NHANES/pops_hormones.sas7bdat")
# nhance_data_table <- data.table(nhance_data)



##########################################################################################
## Are POPs associated with thyroid hormone levels?
##########################################################################################
# clean the data
# thyroid_data <- nhance_data_table[!(WTSPO4YR <0),] %>%
#                                 .[!(MCQ160I==1 | MCQ160M==1 | thyroidcancer==1 | rx_thyroid==1)|(is.na(MCQ160I)|is.na(MCQ160M)|is.na(thyroidcancer)),] %>%
#                                 .[!(rx_corticosteroids==1 | rx_hormones==1),] %>%
#                                 .[!(RIDAGEYR<20),]  

  
# selected total rows :2649
thyroid_data <- read.sas7bdat("./real_data/NHANES/thyroid/nhance_thyroid.sas7bdat") %>% data.table(.) 

# select the only exposure and outcome
outcome_name <- c("LBXTSH", "LBXT4")
selected_col <- c(outcome_name, PCB, PCB_LC)
thyroid_data <- thyroid_data[,.SD, .SDcols = selected_col]

# 1. remove all empty row, # 213 rows removed
thyroid_data_tmp <- thyroid_data[rowSums(is.na(thyroid_data)) < ncol(thyroid_data),]

# 2. create the missing data table
missing_count_table <- rowSums(is.na(thyroid_data_tmp)) %>% table(.) %>% cumsum(.) %>% 
  data.frame(NA_each_row= as.integer(names(.)), total = .)
p <- length(PCB)
missing_count_plot_thyroid <- ggplot() +
  geom_step(data = missing_count_table, mapping = aes(x = NA_each_row, y = total)) +
  ggtitle("thyroid_PCB_and_LC_75") +
  geom_vline(xintercept= 28, linetype="dashed", color = "red") +
  geom_hline(yintercept= p*(p-1)/2, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=c(c(0,5,10,15,20,25,28),seq(30,75,5))) +
  theme(plot.title = element_text(hjust = 0.5))

##########################################################################################
## Are POPs associated with steroid hormones in males?
##########################################################################################
# steroid_data <- nhance_data_table[!(WTSPO4YR <0),] %>%
#                                 .[!(rx_corticosteroids==1 | rx_hormones==1 | rx_thyroid==1),] %>%
#                                 .[!(RIDAGEYR<20),] %>%
#                                 .[!(RIAGENDR==1),]

# selected total rows :1249
steroid_data <- read.sas7bdat("./real_data/NHANES/steroid/nhance_steroid.sas7bdat") %>% data.table(.) 

# select the only exposure and outcome
outcome_name <- outcome_name <- c("SS3ADLG", "SSSE2", "SSSHBG", "SSTESTO")
selected_col <- c(outcome_name, PCB, PCB_LC)
steroid_data <- steroid_data[,.SD, .SDcols = selected_col]

# 1. remove all empty row, # 152 rows removed
steroid_data_tmp <- steroid_data[rowSums(is.na(steroid_data)) < ncol(steroid_data),]

# 2. create the missing data table
missing_count_table <- rowSums(is.na(steroid_data_tmp)) %>% table(.) %>% cumsum(.) %>% 
  data.frame(NA_each_row= as.integer(names(.)), total = .)
p <- length(PCB)
missing_count_plot_steroid <- ggplot() +
  geom_step(data = missing_count_table, mapping = aes(x = NA_each_row, y = total)) +
  ggtitle("steroid_PCB_and_LC_77") +
  geom_vline(xintercept= 30, linetype="dashed", color = "red") +
  geom_hline(yintercept= p*(p-1)/2, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=c(c(0,10,20,30),seq(30,75,5))) +
  theme(plot.title = element_text(hjust = 0.5))




















##########################################################################################
## Are POPs associated with hemoglobin A1c in non-diabetics?
#######################################################################################
# hemoglobin_data <- nhance_data_table[!(RIDAGEYR < 20),] %>%
#                                    .[(!(DIQ010==1 | rx_antidiab==1)) | (is.na(DIQ010)|is.na(rx_antidiab)),] 

hemoglobin_data <- read.sas7bdat("./real_data/NHANES/hemoglobin/nhance_hemoglobin.sas7bdat") %>% 
                   data.table(.)
# select the only exposure and outcome
outcome_name <- c("LBXGH")
selected_col <- c(outcome_name, name_PCB, name_PCB_LC, name_other_variables)
hemoglobin_data_selected <- hemoglobin_data[,..selected_col]

# change the outcome name
colnames(hemoglobin_data_selected)[colnames(hemoglobin_data_selected) %in% outcome_name] <- "outcome_LBXGH"

# change the exposures' name to PCB
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
hemoglobin_data_tmp <- hemoglobin_data_selected[!(is.na(outcome_LBXGH)),]
(nrow(hemoglobin_data_selected) - nrow(hemoglobin_data_tmp)) %>% cat(., "row removed")

# 1. remove all empty PCB and PCBLC row, # 0 rows removed
n1 <- nrow(hemoglobin_data_tmp)
selected_col <- c(name_PCB, name_PCB_LC)
index <- hemoglobin_data_tmp[,rowSums(is.na(.SD)) == length(selected_col), .SDcols = selected_col]
hemoglobin_data_tmp <- hemoglobin_data_tmp[!index,]
(n1 - nrow(hemoglobin_data_tmp)) %>% cat(., "row removed")

# 2. recover the limit of detection
name_PCB_with_LC <- gsub(pattern = "LC", 
                         replacement = "", 
                         x = name_PCB_LC,
                         perl = TRUE)

hemoglobin_data_test_comp <- update_LC(data = hemoglobin_data_tmp, PCB_names = name_PCB_with_LC)

# 2. create the missing data table
missing_count_table <- rowSums(is.na(hemoglobin_data_tmp)) %>% table(.) %>% cumsum(.) %>% 
                         data.frame(NA_each_row= as.integer(names(.)), total = .)
p <- length(PCB)
missing_count_plot_hemoglobin <- ggplot() +
                                  geom_step(data = missing_count_table, mapping = aes(x = NA_each_row, y = total)) +
                                  ggtitle("hemoglobin_PCB_and_LC_74") +
                                  geom_vline(xintercept= 26, linetype="dashed", color = "red") +
                                  geom_hline(yintercept= p*(p-1)/2, linetype="dashed", color = "red") +
                                  scale_x_continuous(breaks=c(c(0,5,10,15,20,26),seq(30,75,5))) +
                                  theme(plot.title = element_text(hjust = 0.5))

###

# pdf(file = "./real_data/NHANES/missing_data_count.pdf",
#     width = 11,
#     height = 11)
# 
# print(missing_count_plot_thyroid)
# print(missing_count_plot_steroid)
# print(missing_count_plot_hemoglobin)
# 
# dev.off()
# output 
write.csv(thyroid_data_tmp,file = "./real_data/NHANES/thyroid/nhance_thyroid_PCB_LC.csv", row.names = FALSE)
write.csv(steroid_data_tmp,file = "./real_data/NHANES/steroid/nhance_steroid_PCB_LC.csv", row.names = FALSE)
write.csv(hemoglobin_data_tmp,file = "./real_data/NHANES/hemoglobin/nhance_hemoglobin_PCB_LC.csv", row.names = FALSE)

# output as sas file
write.foreign(df=thyroid_data_tmp, datafile="./real_data/NHANES/thyroid/SAS_nhance_thyroid_PCB_LC.csv", codefile="./real_data/NHANES/thyroid/SAS_nhance_thyroid_PCB_LC.sas", package="SAS")
write.foreign(df=thyroid_data_tmp, datafile="./real_data/NHANES/steroid/SAS_nhance_steroid_PCB_LC.csv", codefile="./real_data/NHANES/steroid/SAS_nhance_steroid_PCB_LC.sas", package="SAS")
write.foreign(df=thyroid_data_tmp, datafile="./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB_LC.csv", codefile="./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB_LC.sas", package="SAS")
