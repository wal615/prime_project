# example of prime project 
options(warn = 1, error = bettertrace::stacktrace)
library(prime.total.effect)
# total.effect.estimation 
data <- na.omit(hemoglobin.PCB)
data <- data[,1:39] # only select y and PCBs 
data[,-1] <- std.fn(data[,-1])  
res.GCTA <- GCTA.rr(y = data$LBXGH, x = data[,-1], target = "beta2")
data.sub <- data[1:40,]
res.Dicker <- Dicker.2013(y = data.sub$LBXGH, x = data.sub[,-1], target = "h2")
res.EignPrism <- EigenPrism(y = data.sub$LBXGH, X = data.sub[,-1], target = "beta2")

# addding the decorrelation procedure
data <- read.csv("~/dev/prime_project/R_code/data/real_data/NHANES/subset_data_based_on_hypothesis/hemoglobin/missing_value_imputation/nhanceMiceImpute10.csv")
data[,-1] <- std.fn(data[,-1])
x.glasso <- GLASSO.method(x = data.matrix(data[,-1]), rho = 0.01)$uncorr.data
x.empirical <- empirical.cov.method(x = data.matrix(data[,-1]), emp.sigma = cov(data[,-1]))$uncorr.data
res.GCTA <- GCTA.rr(y = data$LBXGH, x = x.empirical, target = "beta2")
res.Dicker <- Dicker.2013(y = data.sub$LBXGH, x = data.sub[,-1], target = "beta2")

# adding the total effect 
x.total <- add.inter(b = data[,-1])
y <- data$LBXGH
x.empirical <- empirical.cov.method(x = x.total, emp.sigma = cov(x.total))$uncorr.data
res.GCTA <- GCTA.rr(y = y, x = x.empirical, target = "beta2")
res.GCTA <- GCTA.rr(y = y, x = x.total, target = "beta2")
