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
jack_var <- function(x, pro = 0.5, d = 1, n){
  if (mean(is.na(x)) > 0.05) warning("too many NA in jackknife subsample result")
  x <- x[!is.na(x)]
  x_m <- mean(x, na.rm = T)
  var_1 <- (x_m - x)^2 %>% sum(., na.rm = T)
  if(pro == 101){
    var_2 <-  (n -d) / (d * length(x)) * var_1 # lenght(x) = cbn (n,d) if no NAs
  } else if (pro ==102){
    var_2 <- 1/n * var_1
  } else{
    var_2 <- pro/(1-pro) * 1/n * var_1
  }
  var_2
}

# jackknife variance estimation correction based on the quadratic form 
v_jact_correct <- function(S, n, i_1, i_2, S_i, Q = FALSE) {
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
  v_jack <- jack_var(x = S_1, pro = 101, n = n, d = 1)
  # jackknife bias correction
  v_jack_corr <- v_jack - ssd(Q_jj)/(n*(n-1))
  res <- c(v_jack = v_jack, v_jack_corr = v_jack_corr)
  if(Q == TRUE){
    attributes(res) <- append(attributes(res) , list(Q = Q_jj))
  }
  res
}


subsample_summary <- function (result_tmp, bs, combine, n_obs){
  result_tmp <- data.table(result_tmp)
  # select the name of the col
  if (combine == TRUE){
    name_pattern <- "sub.*total|total.*sub"
  } else {
    name_pattern <- "sub.*main|main.*sub"
  }
  sub_col_name <- names(result_tmp)[grepl(names(result_tmp), pattern = name_pattern)]
  col_name <- substr(sub_col_name, 5, stop = 100) # whole sample result
  
  
  # get the jackknife bias corrected estimation and variance estimation
  if(bs == "leave-1"){ #leave - 1 means no variance bias correction#
    
    # col1
    col1 <- col_name[1]
    sub_col1 <- sub_col_name[1]
    S1 <- data.matrix(result_tmp[1, ..col1])
    S_jack <- bias_jack_corr(S1, data.matrix(result_tmp[, ..sub_col1]))
    v_jack <- jack_var(x = data.matrix(result_tmp[, ..sub_col1]), pro = 101, d = 1, n = n_obs)
    S1_jack <- data.table(S1_jack = S_jack, S1_v_jack_1 = v_jack)
    
    # col2
    if(length(col_name) ==2 ) {
      col2 <- col_name[2]
      sub_col2 <- sub_col_name[2]
      S2 <- data.matrix(result_tmp[1, ..col2])
      S_jack <- bias_jack_corr(S2, data.matrix(result_tmp[, ..sub_col2]))
      v_jack <- jack_var(x = data.matrix(result_tmp[, ..sub_col2]), pro = 101, d = 1, n = n_obs)
      S2_jack <- data.table(S2_jack = S_jack, S2_v_jack_1 = v_jack)
    }
    
    
  } else if(bs == "leave-1-2"){ #leave-1-2 means add variance bias correction for quadratic #
    
    # col1
    col1 <- col_name[1]
    sub_col1 <- sub_col_name[1]
    S1 <- data.matrix(result_tmp[1, ..col1])
    S_jack <- bias_jack_corr(S1, data.matrix(result_tmp[1:n_obs, ..sub_col1]))
    v_jack_leave_1 <- jack_var(x = data.matrix(result_tmp[1:n_obs, ..sub_col1]), pro = 101, d = 1, n = n_obs)
    v_jack_leave_2 <- jack_var(x = data.matrix(result_tmp[-(1:n_obs), ..sub_col1]), pro = 101, d = 2, n = n_obs)
    v_jack_corr <- v_jact_correct(S = S1, n = n_obs, S_i = data.matrix(result_tmp[, ..sub_col1]), 
                                  i_1 = result_tmp$i_1, i_2 = result_tmp$i_2)
    S1_jack <- data.table(S1_jack = S_jack, S1_v_jack_1 = v_jack_leave_1, S1_v_jack_2 = v_jack_leave_2, S1_v_jack_corr = v_jack_corr[2])
    
    
    # col2
    if(length(col_name) == 2) {
    col2 <- col_name[2]
    sub_col2 <- sub_col_name[2]
    S2 <- data.matrix(result_tmp[1, ..col2])
    S_jack <- bias_jack_corr(S2, data.matrix(result_tmp[1:n_obs, ..sub_col2]))
    v_jack_leave_1 <- jack_var(x = data.matrix(result_tmp[1:n_obs, ..sub_col2]), pro = 101, d = 1, n = n_obs)
    v_jack_leave_2 <- jack_var(x = data.matrix(result_tmp[-(1:n_obs), ..sub_col2]), pro = 101, d = 2, n = n_obs)
    v_jack_corr <- v_jact_correct(S = S2, n = n_obs, S_i = data.matrix(result_tmp[, ..sub_col2]), 
                                  i_1 = result_tmp$i_1, i_2 = result_tmp$i_2)
    S2_jack <- data.table(S2_jack = S_jack, S2_v_jack_1 = v_jack_leave_1, S2_v_jack_2 = v_jack_leave_2, S2_v_jack_corr = v_jack_corr[2])
    }
  }
  if(length(col_name) == 2) {
    cbind(result_tmp[1,!sub_col_name, with = FALSE], S1_jack, S2_jack)  
  } else {
    cbind(result_tmp[1,!sub_col_name, with = FALSE], S1_jack)
  }
}