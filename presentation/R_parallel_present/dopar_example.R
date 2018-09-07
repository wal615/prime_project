library(doParallel)
library(foreach)

registerDoParallel(4)
x <- iris[which(iris[,5] != "setosa"), c(1,5)]

print("Running dopar...")

r <- foreach(i = 1:10000) %dopar% {
  ind <- sample(100, 100, replace=TRUE)
  result1 <- glm(x[ind,2]~x[ind,1],
                 family=binomial(logit))
  coefficients(result1)
}

print("Done!")