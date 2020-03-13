setwd("~/dev/projects/Chen_environmental_study/R_code/data/")
library(sas7bdat)
library(SASxport)
library(tidyverse)
library(data.table)
# nhance_data <- read.sas7bdat("./real_data/NHANES/pops_hormones.sas7bdat")
# nhance_data_table <- data.table(nhance_data)

# got all the exposures'names 
expousres_type <- read.csv("./real_data/NHANES/variable_names/exposure_type.csv", header = TRUE, stringsAsFactors = FALSE) %>% data.table(.)
PCB_all<- expousres_type[grepl("^PCB", x = type), exposure]
PCB_LA <- PCB_all[grep(PCB_all, pattern = ".*LA$", perl = TRUE)]
PCB_LC <- PCB_all[grep(PCB_all, pattern = ".*LC$", perl = TRUE)]
PCB <- PCB_all[-grep(PCB_all, pattern = ".*(LC|LA)$", perl = TRUE)]
length(PCB_all) == length(PCB_LA) + length(PCB_LC) + length(PCB)


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
selected_col <- c(outcome_name, PCB)
thyroid_data <- thyroid_data[,.SD, .SDcols = selected_col]

# 1. remove all empty row, # 213 rows removed
thyroid_data_tmp <- thyroid_data[rowSums(is.na(thyroid_data)) < ncol(thyroid_data),]
# 2. create the missing data table
missing_count_table <- rowSums(is.na(thyroid_data_tmp)) %>% table(.) %>% cumsum(.) %>% 
  data.frame(NA_each_row= as.integer(names(.)), total = .)
p <- length(PCB)
missing_count_plot_thyroid <- ggplot() +
  geom_step(data = missing_count_table, mapping = aes(x = NA_each_row, y = total)) +
  ggtitle("thyroid_PCB_without_LC_40") +
  geom_vline(xintercept= 13, linetype="dashed", color = "red") +
  geom_hline(yintercept= p*(p-1)/2, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=c(c(0,5,10,13,20,25),seq(30,75,5))) +
  theme(plot.title = element_text(hjust = 0.5))

##########################################################################################
## Are POPs associated with steroid hormones in males?
##########################################################################################
# steroid_data <- nhance_data_table[!(WTSPO4YR <0),] %>%
#                                 .[!(rx_corticosteroids==1 | rx_hormones==1 | rx_thyroid==1),] %>%
#                                 .[!(RIDAGEYR<20),] %>%
#                                 .[!(RIAGENDR==1),]

# selected total rows :1294
steroid_data <- read.sas7bdat("./real_data/NHANES/steroid/nhance_steroid.sas7bdat") %>% data.table(.) 

# select the only exposure and outcome
outcome_name <- outcome_name <- c("SS3ADLG", "SSSE2", "SSSHBG", "SSTESTO")
selected_col <- c(outcome_name, PCB)
steroid_data <- steroid_data[,.SD, .SDcols = selected_col]

# 1. remove all empty row, # 152 rows removed
steroid_data_tmp <- steroid_data[rowSums(is.na(steroid_data)) < ncol(steroid_data),]

# 2. create the missing data table
missing_count_table <- rowSums(is.na(steroid_data_tmp)) %>% table(.) %>% cumsum(.) %>% 
  data.frame(NA_each_row= as.integer(names(.)), total = .)
p <- length(PCB)
missing_count_plot_steroid <- ggplot() +
  geom_step(data = missing_count_table, mapping = aes(x = NA_each_row, y = total)) +
  ggtitle("steroid_PCB_without_LC_42") +
  geom_vline(xintercept= 17, linetype="dashed", color = "red") +
  geom_hline(yintercept= p*(p-1)/2, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=c(c(0,10,17,27),seq(30,75,5))) +
  theme(plot.title = element_text(hjust = 0.5))




##########################################################################################
## Are POPs associated with hemoglobin A1c in non-diabetics?
#######################################################################################
# hemoglobin_data <- nhance_data_table[!(RIDAGEYR < 20),] %>%
#                                    .[(!(DIQ010==1 | rx_antidiab==1)) | (is.na(DIQ010)|is.na(rx_antidiab)),] 
hemoglobin_data <- read.sas7bdat("./real_data/NHANES/hemoglobin/nhance_hemoglobin.sas7bdat") %>% data.table(.)
# select the only exposure and outcome
outcome_name <- c("LBXGH")
selected_col <- c(outcome_name, PCB)
hemoglobin_data <- hemoglobin_data[,.SD, .SDcols = selected_col]


# 1. remove all empty row, # 1619 rows removed
hemoglobin_data_tmp <- hemoglobin_data[rowSums(is.na(hemoglobin_data)) < ncol(hemoglobin_data),]

# 2. create the missing data table
missing_count_table <- rowSums(is.na(hemoglobin_data_tmp)) %>% table(.) %>% cumsum(.) %>% 
  data.frame(NA_each_row= as.integer(names(.)), total = .)
p <- length(PCB)
missing_count_plot_hemoglobin <- ggplot() +
  geom_step(data = missing_count_table, mapping = aes(x = NA_each_row, y = total)) +
  ggtitle("hemoglobin_PCB_without_LC_39") +
  geom_vline(xintercept= 13, linetype="dashed", color = "red") +
  geom_hline(yintercept= p*(p-1)/2, linetype="dashed", color = "red") +
  scale_x_continuous(breaks=c(c(0,5,10,13,20,26),seq(30,75,5))) +
  theme(plot.title = element_text(hjust = 0.5))

########################################################################
## generate pdf
########################################################################

# pdf(file = "./real_data/NHANES/missing_data_count_without_LC.pdf",
#     width = 11,
#     height = 11)
# 
# print(missing_count_plot_thyroid)
# print(missing_count_plot_steroid)
# print(missing_count_plot_hemoglobin)
# 
# dev.off()

########################################################################
## generate tmp_tables
########################################################################

write.csv(thyroid_data_tmp,file = "./real_data/NHANES/thyroid/nhance_thyroid_PCB.csv", row.names = FALSE)
write.csv(steroid_data_tmp,file = "./real_data/NHANES/steroid/nhance_steroid_PCB.csv", row.names = FALSE)
write.csv(hemoglobin_data_tmp,file = "./real_data/NHANES/hemoglobin/nhance_hemoglobin_PCB.csv", row.names = FALSE)

write.foreign(df=thyroid_data_tmp, datafile="./real_data/NHANES/thyroid/SAS_nhance_thyroid_PCB.csv", codefile="./real_data/NHANES/thyroid/SAS_nhance_thyroid_PCB.sas", package="SAS")
write.foreign(df=thyroid_data_tmp, datafile="./real_data/NHANES/steroid/SAS_nhance_steroid_PCB.csv", codefile="./real_data/NHANES/steroid/SAS_nhance_steroid_PCB.sas", package="SAS")
write.foreign(df=thyroid_data_tmp, datafile="./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB.csv", codefile="./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB.sas", package="SAS")

