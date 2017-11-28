#### This file contains the simulation strings 
Li_2009 <- "
set.seed(1014)
p <- {p} # demension
n <- {n}
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
cat('Simulation data generating')
"
names(Li_2009) <- "Li_2009"
