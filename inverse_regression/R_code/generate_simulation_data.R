library(tidyverse)
library(data.table)
working_dir <- "~/shell/home/wangxx14/inverse_regression/"
setwd(working_dir)

#### generate the simulation data and adding slice lable 

#### get the slice position of the 
#### return the break points for each slice
slice_method <- function(slice, n){
  hslice<-matrix(0,2,slice);  ## i^th slice is hslice[1,i]~hslice[2,i]
  hslice[1,1]<-1;
  hslice[2,slice]<-n;
  for(i in 1:(slice-1)) { 
    hslice[2,i]<-floor(n*i/slice);
    hslice[1,i+1]<-hslice[2,i]+1;
  }
  hslice
}

#### order the data by y and append a columan for group 
#### return a data.frame ordered by y and appended a group colomn
slice_data_with_index <- function(x, y, slice_number){
  ordered_data <- data.table::data.table(x = x, y = as.numeric(y)) 
  
  ordered_data[, index:= .I] %>% data.table::setorder(., y)
  # .I is the special value in data.table
  
  # using slice break points to generate slice factor
  slice_position <- slice_method(slice_number, length(y))
  
  slice <- letters[1:slice_number] %>% 
    rep(., (slice_position[2,]-slice_position[1,]+1))
  
  ordered_data[,slice:= slice] %>% 
    data.table::setorder(., index) %>%
    set(., j = "index", value = NULL) %>%
        data.frame(.) 
}


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

y_names <- c("y_1","y_2","y_3","y_4")
file_list <- paste0("simulation_Li_2007_", y_names, ".csv")
slice_number <- 8
## adding lable 
for(i in 1:4) {
  y_i <- y[[i]] %>% as.numeric(.) # data.table need the y to be a vector instead of matrix
  slice_data_with_index(x, y_i, slice_number) %>% write.csv(., file = file_list[i], row.names = FALSE, sep = ",")
}


