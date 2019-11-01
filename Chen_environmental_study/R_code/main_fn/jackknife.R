# functions for calculat jackknife related method

ssd <- function(x){
  sum((x - mean(x))^2) 
}

# jackknife bias correction
bias_jack_corr <- function(S, S_i) {
  n <- length(S_i)
  S_corr <- n*S - (n-1)*mean(S_i)
  S_corr
}

# jackknife variance estimation
jack_var <- function(x, pro = 0.5){
  x_m <- mean(x, na.rm = T)
  n_sub <- length(x) - sum(is.na(x)) # count for the NA data
  var_1 <- (x_m - x)^2 %>% sum(., na.rm = T)
  if(pro == 101){
    var_2 <-  (length(x) -1) * 1/n_sub * var_1
  } else if (pro ==102){
    var_2 <- 1/n_sub * var_1
  } else{
    var_2 <- pro/(1-pro) * 1/n_sub * var_1
  }
  var_2
}

# jackknife variance estimation correction based on the quadratic form 
v_jact_correct <- function(S, n, i_1, i_2, S_i) {
  S <- unique(S)
  n <- unique(n)
  S_1 <- S_i[1:n]
  i_10 <- i_1[1:n]
  S_2 <- S_i[-(1:n)]
  i_12 <- cbind(i_1[-(1:n)], i_2[-(1:n)])
  
  # Q is for correct the sencond order bias of jackknife variance by Eforn 1981
  Q_jj <- numeric(nrow(i_12))
  for(j in 1:nrow(i_12)){
    j_1 <- i_12[j, 1]
    j_2 <- i_12[j, 2]
    Q_jj[j] <- n*S - (n-1)*(S_1[j_1] + S_1[j_2]) + (n-2)*S_2[j]
  }
  # jackknife variance 
  v_jack <- jack_var(S_1, pro = 101)
  # jackknife bias correction
  v_jack_corr <- v_jack - ssd(Q_jj)/(n*(n-1))
  c(v_jack, v_jack_corr)
}

subsample_summary <- function (result_tmp, bs, combine, n){
  # get the jackknife bias corrected estimation and variance estimation
  result_tmp <- data.table(result_tmp)
  if(bs == "leave-1"){ #leave - 1 means no variance bias correction#
    if (combine == TRUE){
      # Eg
      S_Eg <- result_tmp[1, EigenPrism_total]
      S_jack <- bias_jack_corr(S_Eg, result_tmp$sub_EigenPrism_total)
      v_jack <- result_tmp[,jack_var(sub_EigenPrism_total, pro = 101)] 
      Eg_jack <- data.table(Eg_mean_jack = S_jack, Eg_var_jack = v_jack)
      # GCTA
      S_GCTA <- result_tmp[1, GCTA_total]
      S_jcak <- bias_jack_corr(S_GCTA, result_tmp$sub_GCTA_total)
      v_jack <- result_tmp[,jack_var(sub_GCTA_total, pro = 101)] 
      GCTA_jack <- data.table(GCTA_mean_jack = S_jack, GCTA_var_jack = v_jack)
    } else {
      # Eg
      S_Eg <- result_tmp[1, EigenPrism_main]
      S_jack <- bias_jack_corr(S_Eg, result_tmp$sub_EigenPrism_main)
      v_jack <- result_tmp[,jack_var(sub_EigenPrism_main, pro = 101)] 
      Eg_jack <- data.table(Eg_mean_jack = S_jack, Eg_var_jack = v_jack)
      # GCTA
      S_GCTA <- result_tmp[1, GCTA_main]
      S_jcak <- bias_jack_corr(S_GCTA, result_tmp$sub_GCTA_main)
      v_jack <- result_tmp[,jack_var(sub_GCTA_main, pro = 101)] 
      GCTA_jack <- data.table(GCTA_mean_jack = S_jack, GCTA_var_jack = v_jack)
    }
  } else if(bs == "leave-1-2"){ #leave-1-2 means add variance bias correction for quadratic #
    if(combine == TRUE){
      # Eg
      S_Eg <- result_tmp[1, EigenPrism_total]
      S_jack <- bias_jack_corr(S_Eg, result_tmp$sub_EigenPrism_total[1:n])
      v_jack <- v_jact_correct(S = S_Eg, n = n, S_i = result_tmp$sub_EigenPrism_total, i_1 = result_tmp$i_1, i_2 = result_tmp$i_2)
      Eg_jack <- data.table(Eg_mean_jack = S_jack, Eg_var_jack = v_jack[1], Eg_var_jack_corr = v_jack[2])
      # GCTA
      S_GCTA <- result_tmp[1, GCTA_total]
      S_jack <- bias_jack_corr(S_GCTA, result_tmp$sub_GCTA_total[1:n])
      v_jack <- v_jact_correct(S = S_GCTA, n = n, S_i = result_tmp$sub_GCTA_total, i_1 = result_tmp$i_1, i_2 = result_tmp$i_2)
      GCTA_jack <- data.table(GCTA_mean_jack = S_jack, GCTA_var_jack = v_jack[1], GCTA_var_jack_corr = v_jack[2])
    } else {
      S_Eg <- result_tmp[1, EigenPrism_main]
      S_jack <- bias_jack_corr(S_Eg, result_tmp$sub_EigenPrism_main[1:n])
      v_jack <- v_jact_correct(S = S_Eg, n = n, S_i = result_tmp$sub_EigenPrism_main, i_1 = result_tmp$i_1, i_2 = result_tmp$i_2)
      Eg_jack <- data.table(Eg_mean_jack = S_jack, Eg_var_jack = v_jack[1], Eg_var_jack_corr = v_jack[2])
      # GCTA
      S_GCTA <- result_tmp[1, GCTA_main]
      S_jack <- bias_jack_corr(S_GCTA, result_tmp$sub_GCTA_main[1:n])
      v_jack <- v_jact_correct(S = S_GCTA, n = n, S_i = result_tmp$sub_GCTA_main, i_1 = result_tmp$i_1, i_2 = result_tmp$i_2)
      GCTA_jack <- data.table(GCTA_mean_jack = S_jack, GCTA_var_jack = v_jack[1], GCTA_var_jack_corr = v_jack[2])
    }
  }

  
  # for bootstrap 
  if(bs == "bs"){
    if (combine == TRUE){
      # Eg
      S_Eg <- result_tmp[1, EigenPrism_total]
      s_B <- result_tmp[,mean(sub_EigenPrism_total, na.rm = T)] 
      v_B <- result_tmp[,var(sub_EigenPrism_total, na.rm = T)] 
      Eg_B <- data.table(Eg_mean_B = s_B, Eg_var_B = v_B)
      
      # GCTA
      S_GCTA <- result_tmp[1, GCTA_total]
      s_B <- result_tmp[,mean(sub_GCTA_total, na.rm = T)] 
      v_B <- result_tmp[,var(sub_GCTA_total, na.rm = T)] 
      GCTA_B <- data.table(GCTA_mean_B = s_B, GCTA_var_B = v_B)
    } else {
      # Eg
      S_Eg <- result_tmp[1, EigenPrism_main]
      s_B <- result_tmp[,mean(sub_EigenPrism_main, na.rm = T)] 
      v_B <- result_tmp[,var(sub_EigenPrism_main, na.rm = T)] 
      Eg_B <- data.table(Eg_mean_B = s_B, Eg_var_B = v_B)
      
      # GCTA
      S_GCTA <- result_tmp[1, GCTA_main]
      s_B <- result_tmp[,mean(sub_GCTA_main, na.rm = T)] 
      v_B <- result_tmp[,var(sub_GCTA_main, na.rm = T)] 
      GCTA_B <- data.table(GCTA_mean_B = s_B, GCTA_var_B = v_B)
    }
  }
  
  if(bs == "bs"){
    if(combine == TRUE){
      cbind(result_tmp[1,1:4], EigenPrism_total = result_tmp[1,EigenPrism_total], EigenPrism_CI = result_tmp[1,EigenPrism_CI], Eg_B, GCTA_total = result_tmp[1,GCTA_total], GCTA_B)  
    } else{
      cbind(result_tmp[1,1:4], EigenPrism_main = result_tmp[1,EigenPrism_main], EigenPrism_CI = result_tmp[1,EigenPrism_CI], Eg_B, GCTA_main = result_tmp[1,GCTA_main], GCTA_B)
    }
  } else if (bs == "leave-1"|bs == "leave-1-2" ){
    if(combine == TRUE){
      cbind(result_tmp[1,1:4], EigenPrism_total = result_tmp[1,EigenPrism_total], EigenPrism_CI = result_tmp[1,EigenPrism_CI], Eg_jack, GCTA_total = result_tmp[1,GCTA_total], GCTA_jack)  
    } else{
      cbind(result_tmp[1,1:4], EigenPrism_main = result_tmp[1,EigenPrism_main], EigenPrism_CI = result_tmp[1,EigenPrism_CI], Eg_jack, GCTA_main = result_tmp[1,GCTA_main], GCTA_jack)
    }
  }
  
}