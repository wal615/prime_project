# demo_code for stat 494
options(width = 40)
library(doParallel)
library(foreach)
library(parallel)
library(iterators)
head(cars)
class(cars)

plot(x = cars$speed, y = cars$dist)
boxplot(cars$speed, main="Speed")
boxplot(cars$dist, main="Distance")
help(lm)
class(lm)
linearMod <- lm(dist ~ speed, data=cars)
class(linearMod)
print(linearMod)


par(mfrow = c(2, 2))
plot(linearMod)

help(predict)
help(predict.lm)

# esimation
est <- predict(linearMod)
est[1:5]

# prediction
new_data <- data.frame(speed = 200)
pre <- predict(linearMod, newdata = new_data)
pre

plot(x = cars$speed, y = cars$dist)
lines(x = cars$speed, linearMod$fitted.values, type='l', col='blue')


# Introduction to parallel computation 


### Getting start Example


system.time(foreach(i=1:10000) %do% sum((1:i)^2))[3]
system.time(foreach(i=1:10000) %dopar% sum((1:i)^2))[3]



registerDoParallel()
getDoParWorkers()


system.time(foreach(i=1:10000) %do% sum(sqrt(1:i)))[3]
system.time(foreach(i=1:10000) %dopar% sum(sqrt(1:i)))[3]

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
dopar <- system.time(r <- foreach(i = 1:10000) %dopar% {
ind <- sample(100, 100, replace=TRUE)
result1 <- glm(x[ind,2]~x[ind,1],
family=binomial(logit))
coefficients(result1)
})[3]

do <- system.time(r <- foreach(i = 1:10000) %do% {
ind <- sample(100, 100, replace=TRUE)
result1 <- glm(x[ind,2]~x[ind,1],
family=binomial(logit))
coefficients(result1)
})[3]



