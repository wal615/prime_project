## Dimension reduction ####
library(dr)
source("~/dev/prime_project/R_code/main_fn/preprocess.R")
setwd("~/dev/prime_project/")
# data <- read.csv("./R_code/data/real_data/NHANES/subset_data_based_on_hypothesis/diabetes/nhance_diabetes_PCB.csv")
data <- read.csv("./R_code/data/real_data/NHANES/subset_data_based_on_hypothesis/hemoglobin/nhance_hemoglobin_PCB.csv")
data <- na.omit(data)
data[,-1] <- std_fn(data[,-1])
data <- data.table(data)

# Apply linear regression on full data set ####
lm.full <- lm(LBXGH ~., data = data)
summary(lm.full)

# Apply PCA on full data set ####
x <- data[,-1]
pca.x <- prcomp(x, retx = T)
x.new <- pca.x$x[,1:9]
data.pca <- data.frame(LBXGH = data$LBXGH, x.new)
lm.pca <- lm(LBXGH ~., data = data.pca)
summary(lm.pca)

# Apply SDR method 
sir.x <- dr(formula = LBXGH ~., data = data, method = "sir", nslice = 10)
summary(sir.x)
x.new <- data.matrix(x) %*% sir.x$evectors[,1,drop = FALSE] 
data.sir <- data.frame(LBXGH = data$LBXGH, x.new)
lm.sir <- lm(LBXGH ~., data = data.sir)
summary(lm.sir)


save.x <- dr(formula = LBXGH ~., data = data, method = "save", nslice = 10)
summary(save.x)
x.new <- data.matrix(x) %*% save.x$evectors[,1,drop = FALSE] 
data.save <- data.frame(LBXGH = data$LBXGH, x.new)
lm.save <- lm(LBXGH ~., data = data.save)
summary(lm.save)

