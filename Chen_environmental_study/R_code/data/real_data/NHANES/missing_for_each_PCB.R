setwd("~/dev/projects/Chen_environmental_study/R_code/data/")
library(tidyverse)
library(data.table)
library(reshape2)


# loading data
nhance_thyroid_PCB <- read.csv("./real_data/NHANES/nhance_thyroid_PCB.csv", header = TRUE, stringsAsFactors = FALSE)
nhance_steroid_PCB <- read.csv("./real_data/NHANES/nhance_steroid_PCB.csv", header = TRUE, stringsAsFactors = FALSE)
nhance_hemoglobin_PCB <- read.csv("./real_data/NHANES/nhance_hemoglobin_PCB.csv", header = TRUE, stringsAsFactors = FALSE)

# NA-plot
NA_each_PCB_thyroid <- sapply(X = nhance_thyroid_PCB, FUN = function(x) (sum(is.na(x))),simplify = TRUE) %>% 
                       data.frame(missing = ., no_missing = nrow(nhance_thyroid_PCB) - ., name = names(.))
NA_each_PCB_thyroid <- NA_each_PCB_thyroid[order(-NA_each_PCB_thyroid$missing), ] 
                
plot_data <- gather(NA_each_PCB_thyroid, missing:no_missing, key = "key", value = "value")
NA_each_PCB_thyroid_plot <- ggplot() +
                            geom_bar(data = plot_data , mapping = aes(x = name, y = value, fill=key), stat = "identity") +
                            scale_x_discrete(limits = NA_each_PCB_thyroid$name) +
                            ggtitle("PCB_missing_thyroid") + 
                            theme(plot.title = element_text(hjust = 0.5)) +
                            theme(axis.text.x = element_text(angle = 90)) 

###############################################################################################
# steroid
###############################################################################################

NA_each_PCB_steroid <- sapply(X = nhance_steroid_PCB, FUN = function(x) (sum(is.na(x))),simplify = TRUE) %>% 
  data.frame(missing = ., no_missing = nrow(nhance_steroid_PCB) - ., name = names(.))
NA_each_PCB_steroid <- NA_each_PCB_steroid[order(-NA_each_PCB_steroid$missing), ] 

plot_data <- gather(NA_each_PCB_steroid, missing:no_missing, key = "key", value = "value")
NA_each_PCB_steroid_plot <- ggplot() +
  geom_bar(data = plot_data , mapping = aes(x = name, y = value, fill=key), stat = "identity") +
  scale_x_discrete(limits = NA_each_PCB_steroid$name) +
  ggtitle("PCB_missing_steroid") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) 
  
###############################################################################################
# hemoglobin
###############################################################################################
NA_each_PCB_hemoglobin <- sapply(X = nhance_hemoglobin_PCB, FUN = function(x) (sum(is.na(x))),simplify = TRUE) %>% 
  data.frame(missing = ., no_missing = nrow(nhance_hemoglobin_PCB) - ., name = names(.))
NA_each_PCB_hemoglobin <- NA_each_PCB_hemoglobin[order(-NA_each_PCB_hemoglobin$missing), ] 

plot_data <- gather(NA_each_PCB_hemoglobin, missing:no_missing, key = "key", value = "value")
NA_each_PCB_hemoglobin_plot <- ggplot() +
  geom_bar(data = plot_data , mapping = aes(x = name, y = value, fill=key), stat = "identity") +
  scale_x_discrete(limits = NA_each_PCB_hemoglobin$name) +
  ggtitle("PCB_missing_hemoglobin") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) 

###################################################################
pdf(file = "./real_data/NHANES/missing_each_PCB.pdf",
    width = 8,
    height = 8)

print(NA_each_PCB_thyroid_plot)
print(NA_each_PCB_steroid_plot)
print(NA_each_PCB_hemoglobin_plot)

dev.off()
                              