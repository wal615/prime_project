# This file is for create and mentain a R package related to the PRIME project
# inital the package ####

# create the the folder 
# install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
setwd("~/dev/prime_project/")
create("prime.total.effect")

# create the document 
setwd("~/dev/prime_project/prime.total.effect/")
document()

# install the develop version of the prime.total.effect package 
install("~/dev/prime_project/prime.total.effect/")
library(prime.total.effect)
# create help documents 
setwd("~/dev/prime_project/prime.total.effect/")
devtools::use_vignette(name = "Introduction")

# adding the dataset
test_data <- read.csv("~/dev/prime_project/R_code/data/real_data/NHANES/hemoglobin/imputed_hemoglobin_1.csv")
save(test_data, file="data/test.RData")
