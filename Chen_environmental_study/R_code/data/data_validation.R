library(sas7bdat)
library(SASxport)
library(ggplot2)

setwd("~/dev/projects/Chen_environmental_study/R_code/data/")
# pcb_99_04 <- read.sas7bdat("./pops_abc.sas7bdat")
pcb_2000 <- read.xport("./LAB28POC_99_00.XPT")
pcb_05_06 <- read.xport("./PCBPOL_D_05_06.XPT")
pcb_Chen <- read.sas7bdat("../pcbs1000nomiss.sas7bdat")
nhance_data <- read.sas7bdat("./NHANES/pops_hormones.sas7bdat")

###########################################
## compare the pcb_99_04 and the PCb_chen 
###########################################

# # select overlap columns 
# col_overlap <- match(colnames(pcb_Chen), colnames(pcb_99_04))
# 
# # missing columns 
# colnames(pcb_Chen)[is.na(col_overlap)]
# pcb_99_04_tmp <- pcb_99_04[,na.omit(col_overlap)]
# dim(pcb_99_04_tmp)
# 
# # remove rows which have missing values
# pcb_99_04_no_missing <- pcb_99_04_tmp[complete.cases(pcb_99_04_tmp),]
# dim(pcb_99_04_no_missing)
# 
# # compare the boxplot for each column
# 
# pcb_Chen_gather <- tidyr::gather(pcb_Chen, key = "variable_names", value = "values") %>% 
#                    data.frame(., dataset = "Chen")
# 
# pcb_99_04_no_missing_gather <- tidyr::gather(pcb_99_04_no_missing, key = "variable_names", value = "values") %>% 
#                          data.frame(., dataset = "pcb_99_04")
# 
# compare_Chen_pcb_99_04 <- ggplot(data = rbind(pcb_Chen_gather, pcb_99_04_no_missing_gather)) +
#                      geom_boxplot(aes(x = dataset, y = values, fill = dataset)) +
#                      scale_y_continuous(trans='log10') +
#                      ggforce::facet_wrap_paginate(~variable_names,
#                                                   scales = "free", 
#                                                   ncol = 3, 
#                                                   nrow = 3, page = 1)  
# # saving the pcb_99_04_no_missing
# write.csv(pcb_99_04_no_missing, file ="./pcb_99_04_no_missing.csv")

##################################
## compare the pcb_00 with pcb_06
##################################

# select overlap columns 
colnames(pcb_05_06) <- gsub(pattern = "LBC", replacement = "LBX", x =colnames(pcb_05_06), fixed = TRUE)
colnames(pcb_05_06)[1] <- colnames(pcb_Chen)[1]

col_overlap <- match(colnames(pcb_Chen), colnames(pcb_05_06))

# missing columns 
colnames(pcb_Chen)[is.na(col_overlap)]
pcb_05_06_tmp <- pcb_05_06[,na.omit(col_overlap)]
dim(pcb_05_06_tmp)

# remove rows which have missing values
pcb_05_06_no_missing <- pcb_05_06_tmp[complete.cases(pcb_05_06_tmp),]
dim(pcb_05_06_no_missing)

# compare the boxplot for each column

pcb_Chen_gather <- tidyr::gather(pcb_Chen[,-1], key = "variable_names", value = "values")
pcb_Chen_gather <-  data.frame(pcb_Chen_gather, dataset = "Chen")

pcb_05_06_no_missing_gather <- tidyr::gather(pcb_05_06_no_missing[,-1], key = "variable_names", value = "values")
pcb_05_06_no_missing_gather <-  data.frame(pcb_05_06_no_missing_gather, dataset = "pcb_05_06")

compare_Chen_pcb_05_06 <- as.list(numeric(4))

for(i in (1:4)){
compare_Chen_pcb_05_06[[i]] <- ggplot(data = rbind(pcb_Chen_gather, pcb_05_06_no_missing_gather)) +
  geom_boxplot(aes(x = dataset, y = values, fill = dataset)) +
  scale_y_continuous(trans='log10') +
  ggforce::facet_wrap_paginate(~variable_names,
                               scales = "free", 
                               ncol = 3, 
                               nrow = 3, page = i)  
}

N

