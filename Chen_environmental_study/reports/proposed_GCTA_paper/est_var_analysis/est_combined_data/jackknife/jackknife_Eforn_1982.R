# function
source("~/dev/projects/Chen_environmental_study/R_code/main_fn/jackknife.R")



# efron sample
LAST <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572, 594)
GPA <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)
n <- 15
S <- cor(LAST, GPA) 

# efron 
i_1 <- cbind(1:n, rep(0,15))
i_2 <- t(combn(n, 2))

index <- rbind(i_1, i_2)

S_j <- numeric(nrow(index))
for(j in 1:nrow(index)){
  S_j[j] <- cor(LAST[-index[j,]],GPA[-index[j,]]) 
}

S_1 <- S_j[1:n]
S_2 <- S_j[-(1:n)]

v_jack <- jack_var(S_1, pro = 101)
v_jack_corr <- v_jact_correct(S = S, n = n, i_1 = index[,1], i_2 = index[,2], S_i = S_j)
bias_jack <- (n-1)*(mean(S_1) - S)
e_jack_corr <- S - bias_jack
