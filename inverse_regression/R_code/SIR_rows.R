setwd("~/Google Drive/independent study/inverse regression/R_code")
library(tidyverse)

## simulation data from the Li(2007)
set.seed(1014)
p <- 6 # demension
n <- 10^(5)
e <- rnorm(n)
sigma <- 0.2
x <- rnorm(p*n) %>% matrix(., ncol = p)
beta_1 <- matrix(c(1,1,1,0,0,0),ncol = 1)
beta_2 <- matrix(c(1,0,0,0,1,3),ncol = 1)

y_1 <- 0.4*(x%*%beta_1)^2 + 3*sin(x%*%beta_2/4) + sigma*e
y_2 <- 3*sin(x%*%beta_1/4) + 3*sin(x%*%beta_2/4) + sigma*e
y_3 <- 0.4*(x%*%beta_1)^2 + (x%*%beta_2) %>% abs(.) %>% sqrt(.) + sigma*e
y_4 <- 3*sin(x%*%beta_2/4) + (1+x%*%beta_2)*sigma*e
y <- list(y_1 = y_1, y_2 = y_2, y_3 = y_3, y_4 = y_4)

##################################################################

slice_number <- 8
slice_level <- letters[1:slice_number]
y_names <- c("y_1","y_2","y_3","y_4")
file_names <- paste0("simulation_Li_2007_", y_names, ".csv")

for(i in 1:4) {
  y_i <- y[[i]] %>% as.numeric(.) # data.table need the y to be a vector instead of matrix
  slice_data(x, y_i, slice_number) %>% write.csv(., file = file_names[i])
  sir_Yang <- sirself(y_i, x, n, p, slice_number)
  sir_row <- SIR_rows(file_names[i], ncol = p, slice_level)
  print(paste0("the eigenvalues difference on the ", i, "th simulation model from the Li, Bing(2007)"))
  print(sir_row$eigenvalues - sir_Yang$eigenvalues)
  }


