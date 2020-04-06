# example of prime project 
options(warn = 1, error = bettertrace::stacktrace)
library(prime.total.effect)
# hemoglobin application ####
# total.effect.estimation 
data <- hemoglobin.PCB[,1:39] # only select y and PCBs 
data <- na.omit(data)
data[,-1] <- std.fn(data[,-1])  
res.GCTA <- GCTA.rr(y = data$LBXGH, x = data[,-1], target = "beta2")
data.sub <- data[1:40,]
res.Dicker <- Dicker.2013(y = data.sub$LBXGH, x = data.sub[,-1], target = "h2")
res.EignPrism <- EigenPrism(y = data.sub$LBXGH, X = data.sub[,-1], target = "beta2")

# addding the decorrelation procedure
data <- read.csv("~/dev/prime_project/R_code/data/real_data/NHANES/subset_data_based_on_hypothesis/hemoglobin/missing_value_imputation/nhanceMiceImpute10.csv")
data[,-1] <- std.fn(data[,-1])
x.glasso <- GLASSO.method(x = data.matrix(data[,-1]), rho = 0.01)$uncorr.data
x.empirical <- emp.sigma.method(x = data.matrix(data[,-1]), emp.sigma = cov(data[,-1]))$uncorr.data
res.GCTA <- GCTA.rr(y = data$LBXGH, x = x.empirical, target = "beta2")
res.Dicker <- Dicker.2013(y = data.sub$LBXGH, x = data.sub[,-1], target = "beta2")

# adding the total effect 
x.total <- add.inter(b = data[,-1])
y <- data$LBXGH
x.empirical <- emp.sigma.method(x = x.total, emp.sigma = cov(x.total))$uncorr.data
res.GCTA <- GCTA.rr(y = y, x = x.empirical, target = "beta2")
res.GCTA <- GCTA.rr(y = y, x = x.total, target = "beta2")

# simulation with PCBs covariates ####
set.seed(1234)
# 1. generate covariates
n <- 200
p <- 500
sigma.x <- diag(p)
x <- MASS::mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = sigma.x)

# 2. main coefficient 
betam <- rnorm(p)
# set several coefficient to 0 to mimic sparsity situation
sparse_ratio <- 0.5
zero_index <- ((1/sparse_ratio)*1:floor(p*sparse_ratio)) # 2  4  6  8 10 12 14 16 18 20
betam[zero_index] <- 0
# set the total main effect as 10
betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma.x%*%betam))*betam

# 3. generate y
# generate the error term
rho_e <- 0.5
sigma_e <- sqrt(10*((1-rho_e)/rho_e))
epsilon <- rnorm(n, mean = 0, sd=sigma_e)
y <- x %*% betam + epsilon

# 4. total variance estimation target with 10
res.GCTA <- GCTA.rr(x = x, y = y, target = "h2")
bootstrap.GCTA <- para.bootstrap(h2 = res.GCTA, x = x, n.sub = 1000, h2.method = GCTA.rr, target = "h2")
res.Dicker <- Dicker.2013(x = x, y = y, target = "h2")
res.EignPrism <- EigenPrism(x = x, y = y, target = "h2")


# simulation examples ####
set.seed(1234)
# 1. generate covariates
n <- 200
p <- 500
sigma.x <- diag(p)
x <- MASS::mvrnorm(n = n,
                   mu = rep(0,p),
                   Sigma = sigma.x)

# 2. main coefficient 
betam <- rnorm(p)
# set several coefficient to 0 to mimic sparsity situation
sparse_ratio <- 0.5
zero_index <- ((1/sparse_ratio)*1:floor(p*sparse_ratio)) # 2  4  6  8 10 12 14 16 18 20
betam[zero_index] <- 0
# set the total main effect as 10
betam <- as.numeric(sqrt(10)/sqrt(t(betam)%*%sigma.x%*%betam))*betam

# 3. generate y
# generate the error term
rho_e <- 0.5
sigma_e <- sqrt(10*((1-rho_e)/rho_e))
epsilon <- rnorm(n, mean = 0, sd=sigma_e)
y <- x %*% betam + epsilon

# 4. total variance estimation target with 10
res.GCTA <- GCTA.rr(x = x, y = y, target = "h2")
bootstrap.GCTA <- para.bootstrap(h2 = res.GCTA, x = x, n.sub = 1000, h2.method = GCTA.rr, target = "h2")
res.Dicker <- Dicker.2013(x = x, y = y, target = "h2")
res.EignPrism <- EigenPrism(x = x, y = y, target = "h2")

total.effect.res <- total.effect.master(x,y)



data <- hemoglobin.PCB[,1:39] # only select y and PCBs 
data <- na.omit(data)
x <- data.matrix(data[,-1])
y <- data[,1]
total.effect.method <- GCTA.rr
total.effect.args.list <- list(target = "h2", bs.iter = 10)
total.effect.test.res <- total.effect.master(x = x, y = y, decorrelation.method = "none", rho = 0.001,
                                             total.effect.method = total.effect.method, 
                                             total.effect.args.list = total.effect.args.list)
