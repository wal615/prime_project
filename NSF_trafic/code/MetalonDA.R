# implement of ‘MetaLonDA’
# install from CRAN 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("metagenomeSeq")
# BiocManager::install("DESeq2")
# BiocManager::install("edgeR")
# install.packages("MetaLonDA")
# library(devtools) ?
# install_github("aametwally/MetaLonDA", ref = "master") ??

# install the local modified package
install.packages("~/dev/projects/NSF_trafic/code/MetaLonDA 2/", repos = NULL, type="source")
install.packages("MetaLonDA")


library(MetaLonDA)
library(tidyverse)
library(data.table)


## Load read counts of 8 features from 100 samples. Samples are from 2 groups, 5 subjects per group, and 10 time points per subject.
data(metalonda_test_data)
View(metalonda_test_data[,1:20])

## Create Group, Time, and ID annotation vectors
n.group = 2
n.sample = 5 
n.timepoints = 10
Group = factor(c(rep("A", n.sample*n.timepoints), rep("B",n.sample*n.timepoints)))
Time = rep(rep(1:n.timepoints, times = n.sample), 2)
ID = factor(rep(1:(2*n.sample), each = n.timepoints))

## Define the prediction timeponits 
points = seq(1, 10, length.out = 100)

## fit the feature 5
output.metalonda.f5 = metalonda(Count = metalonda_test_data[5,], Time = Time, Group = Group,
                                ID = ID, n.perm = 100, fit.method = "nbinomial", points = points,
                                text = rownames(metalonda_test_data)[5], parall = TRUE, pvalue.threshold = 0.05,
                                adjust.method = "BH", time.unit = "days", ylabel = "Normalized Count",
                                col = c("black", "green"), prefix = "Test_F5")
