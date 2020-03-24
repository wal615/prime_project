# create the hemoglobin with PCBs only 
hemoglobin.data <- read.csv("hemoglobin_data.csv", 
                        header = T,
                        stringsAsFactors = T)

# remove all the PCB_LC
hemoglobin.PCB <- hemoglobin.data[,!grepl("^PCB.+LC$", colnames(hemoglobin.data))]
# export to the package
devtools::use_data(hemoglobin.PCB)
