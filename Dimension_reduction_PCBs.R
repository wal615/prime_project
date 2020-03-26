## Dimension reduction ####
library(caret)
library(dr)
library(prime.master)
library(tidyverse)
library(caret)
library(pls)

setwd("~/dev/prime_project/")
data <- hemoglobin.PCB[,1:39]
data <- na.omit(data)

# standardized the covariates and a log transformation
data[,-1] <- std.fn(log(data[,-1])) 


# Apply linear regression on full data set ####
lm.full <- lm(LBXGH ~., data = data)

# Apply PCA method ####
model <- train(LBXGH~., data = data, method = "pcr",
  scale = TRUE,
  trControl = trainControl("cv", number = 30),
  tuneLength = 38)
numdir <- model$bestTune$ncomp
x <- data[,-1]
pca.x <- prcomp(x, retx = T)
x.new <- pca.x$x[,1:numdir]
data.pca <- data.frame(LBXGH = data$LBXGH, x.new)
lm.pca <- lm(LBXGH ~., data = data.pca)


# Apply SDR method 
# sir
sir.res <- dr(formula = LBXGH ~., data = data, method = "sir", nslice = 10)
sir.test <- dr.test(sir.res, numdir = 9)
numdir <- sum(sir.test$p.value <= 0.05)
x.new <- data.matrix(x) %*% sir.res$evectors[,1:numdir, drop = FALSE] 
data.sir <- data.frame(LBXGH = data$LBXGH, x.new)
lm.sir <- lm(LBXGH ~., data = data.sir)

# save
save.res <- dr(formula = LBXGH ~., data = data, method = "save", nslice = 2)
save.test <- dr.test(save.res, numdir = 38)
numdir <- sum(save.test$`p.value(Nor)` <= 0.05)
x.new <- data.matrix(x) %*% save.res$evectors[,1:numdir,drop = FALSE] 
data.save <- data.frame(LBXGH = data$LBXGH, x.new)
lm.save <- lm(LBXGH ~., data = data.save)



