---
title: "Untitled"
author: "Xuelong Wang"
date: "10/27/2017"
output: html_document
---


```{r}
library(tidyverse)
library(data.table)

# simulation data
set.seed(1014)
n <- 10^(3)
y <- rnorm(sd = 50, n)
x <- rnorm(n*3,c(1, 20, 100)) %>% matrix(. , nrow = n, ncol = 3, byrow = TRUE)

# assume we can read all the data in the ram and sort the data by y's value
labels_factor <- LETTERS[1:8] %>% as.factor()
data_set <- data.table(y = y, x = x) %>% setorder(., y) 
labels <- data_set$y %>% cut(., breaks = 8, labels = labels_factor)
data_set_labled <- data.table(data_set, labels = labels)

# iterating each row to calculate covariance matrix and sliced mean vector
x_sum <- numeric(3)
x_x_t <- rep(0, ncol(x)) %>% diag()

x_each_slice <- matrix(0, nrow = length(labels_factor), ncol = 3)
rownames(x_each_slice) <- levels(labels_factor)

total_number_each_slice <- numeric(8)
names(total_number_each_slice) <- labels_factor %>% levels()
for (i in (1:nrow(data_set_labled))) {
  x_i <- data_set_labled[i,!c("y", "labels"), with = FALSE]
  labels <- data_set_labled[i, "labels", with = FALSE] 
  x_x_t <- x_x_t + tcrossprod(x_i %>% unlist() %>% matrix(., ncol = 1)) # x*x^t
  x_sum <- x_sum + x_i 
  slice_posistion <- match(labels[[1]], labels_factor)
  x_each_slice[slice_posistion,] <- unlist(x_i) + x_each_slice[slice_posistion,]
  total_number_each_slice[slice_posistion] <- total_number_each_slice[slice_posistion] + 1
}

# calculate the covariance matrix
x_mean <- (x_sum/n) %>% unlist()
covariance <- (x_x_t - n*tcrossprod(x_mean))/n
var(x) - covariance

# calcuate sliced mean vector
h_mean_unnormalized <- sweep(x_each_slice, MARGIN = 1, total_number_each_slice, '/')
h_mean <- (sweep(h_mean_unnormalized, MARGIN = 2, x_mean, FUN = "-")) %*% solve(covariance)^(-1/2) 
```
