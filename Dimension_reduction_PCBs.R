## Dimension reduction ####
library(caret)
library(dr)
library(prime.master)
library(tidyverse)
library(caret)
library(pls)
options(digits = 3)
setwd("~/dev/prime_project/")
data <- hemoglobin.PCB[,1:39]
data <- na.omit(data)

# standardized the covariates and a log transformation
data[,-1] <- std.fn(data[,-1]) 


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

# compare the first direction

lm.1PC <- lm.full$coefficients[-1]
sir.1PC <- sir.res$evectors[,1]
pca.1PC <- pca.x$rotation[,1]
table.1PC <- cbind(lm.1PC,sir.1PC,pca.1PC)
# Compare with GCTA method 

x <- data.matrix(data[,-1])
y <- data[,1]
total.effect.method <- GCTA.rr
total.effect.args.list <- list(target = "h2", bs.iter = 2)
total.effect.test.res <- total.effect.master(x = x, y = y, decorrelation.method = "historical",
                                             total.effect.method = total.effect.method, 
                                             total.effect.args.list = total.effect.args.list)

estimated.R2 <- c(full = summary(lm.full)$r.squared,
            pca = summary(lm.pca)$r.squared,
            sir = summary(lm.sir)$r.squared,
            save = summary(lm.save)$r.squared,
            GCTA = total.effect.test.res$total.effect.res[1],
            p = ncol(data))

###################################################################
## add interaction
###################################################################

x <- add.inter(x)
data <- data.frame(LBXGH = data[,1], x)

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
sir.test <- dr.test(sir.res)
numdir <- sum(sir.test$p.value <= 0.05)
x.new <- data.matrix(x) %*% sir.res$evectors[,1:numdir, drop = FALSE] 
data.sir <- data.frame(LBXGH = data$LBXGH, x.new)
lm.sir <- lm(LBXGH ~., data = data.sir)

# save
# save.res <- dr(formula = LBXGH ~., data = data, method = "save", nslice = 2)
# save.test <- dr.test(save.res, numdir = 38)
# numdir <- sum(save.test$`p.value(Nor)` <= 0.05)
# x.new <- data.matrix(x) %*% save.res$evectors[,1:numdir,drop = FALSE] 
# data.save <- data.frame(LBXGH = data$LBXGH, x.new)
# lm.save <- lm(LBXGH ~., data = data.save)

# Compare with GCTA method 

x <- data.matrix(data[,-1])
y <- data[,1]
total.effect.method <- GCTA.rr
total.effect.args.list <- list(target = "h2", bs.iter = 2)
total.effect.test.res <- total.effect.master(x = x, y = y, decorrelation.method = "historical",
                                             total.effect.method = total.effect.method, 
                                             total.effect.args.list = total.effect.args.list)

estimated.inter.R2 <- c(full = summary(lm.full)$r.squared,
                  pca = summary(lm.pca)$r.squared,
                  sir = NA,
                  save = NA,
                  GCTA = total.effect.test.res$total.effect.res[1],
                  p = ncol(data))


#######################
## with only one PC ####
#######################
numdir <- 1
data <- hemoglobin.PCB[,1:39]
data <- na.omit(data)

# standardized the covariates and a log transformation
data[,-1] <- std.fn(data[,-1]) 


# Apply linear regression on full data set ####
lm.full <- lm(LBXGH ~., data = data)

# Apply PCA method ####
x <- data[,-1]
pca.x <- prcomp(x, retx = T)
x.new <- pca.x$x[,1:numdir]
data.pca <- data.frame(LBXGH = data$LBXGH, x.new)
lm.pca <- lm(LBXGH ~., data = data.pca)


# Apply SDR method 
# sir
sir.res <- dr(formula = LBXGH ~., data = data, method = "sir", nslice = 10)
x.new <- data.matrix(x) %*% sir.res$evectors[,1:numdir, drop = FALSE] 
data.sir <- data.frame(LBXGH = data$LBXGH, x.new)
lm.sir <- lm(LBXGH ~., data = data.sir)

# save
save.res <- dr(formula = LBXGH ~., data = data, method = "save", nslice = 2)
x.new <- data.matrix(x) %*% save.res$evectors[,1:numdir,drop = FALSE] 
data.save <- data.frame(LBXGH = data$LBXGH, x.new)
lm.save <- lm(LBXGH ~., data = data.save)

# Compare with GCTA method 

x <- data.matrix(data[,-1])
y <- data[,1]
total.effect.method <- GCTA.rr
total.effect.args.list <- list(target = "h2", bs.iter = 2)
total.effect.test.res <- total.effect.master(x = x, y = y, decorrelation.method = "historical",
                                             total.effect.method = total.effect.method, 
                                             total.effect.args.list = total.effect.args.list)

estimated.R2.1PC <- c(full = summary(lm.full)$r.squared,
                  pca = summary(lm.pca)$r.squared,
                  sir = summary(lm.sir)$r.squared,
                  save = summary(lm.save)$r.squared,
                  GCTA = total.effect.test.res$total.effect.res[1],
                  p = ncol(data))

###################################################################
## add interaction
###################################################################

x <- add.inter(x)
data <- data.frame(LBXGH = data[,1], x)

# Apply linear regression on full data set ####
lm.full <- lm(LBXGH ~., data = data)

# Apply PCA method ####
x <- data[,-1]
pca.x <- prcomp(x, retx = T)
x.new <- pca.x$x[,1:numdir]
data.pca <- data.frame(LBXGH = data$LBXGH, x.new)
lm.pca <- lm(LBXGH ~., data = data.pca)


# Apply SDR method 
# sir
sir.res <- dr(formula = LBXGH ~., data = data, method = "sir", nslice = 10)
x.new <- data.matrix(x) %*% sir.res$evectors[,1:numdir, drop = FALSE] 
data.sir <- data.frame(LBXGH = data$LBXGH, x.new)
lm.sir <- lm(LBXGH ~., data = data.sir)

# save
save.res <- dr(formula = LBXGH ~., data = data, method = "save", nslice = 2)
x.new <- data.matrix(x) %*% save.res$evectors[,1:numdir,drop = FALSE]
data.save <- data.frame(LBXGH = data$LBXGH, x.new)
lm.save <- lm(LBXGH ~., data = data.save)

# Compare with GCTA method 

x <- data.matrix(data[,-1])
y <- data[,1]
total.effect.method <- GCTA.rr
total.effect.args.list <- list(target = "h2", bs.iter = 2)
total.effect.test.res <- total.effect.master(x = x, y = y, decorrelation.method = "historical",
                                             total.effect.method = total.effect.method, 
                                             total.effect.args.list = total.effect.args.list)

estimated.inter.R2.1PC <- c(full = summary(lm.full)$r.squared,
                        pca = summary(lm.pca)$r.squared,
                        sir = summary(lm.sir)$r.squared,
                        save = summary(lm.save)$r.squared,
                        GCTA = total.effect.test.res$total.effect.res[1],
                        p = ncol(data))
rbind(estimated.R2, estimated.inter.R2, estimated.R2.1PC, estimated.inter.R2.1PC)
