library(foreach)
library(tidyverse)

###############################################################################################

### helper functions for slice inverse regression ###
 
###############################################################################################

#### self-defined function for non-negative matrix decomposition
#### A=P^2, return P
#### idea: A=UDV^T, P=U*sqrt(D)*V^T (U=V if A=A^T)
AAself<-function(A) {   ## A: p*p non-negative matrix
  p<-dim(A)[1];
  Asvd<-svd(A);
  D<-matrix(0,p,p);
  for(i in 1:p) {
    D[i,i]<-Asvd$d[i];
    if(D[i,i]<0) {cat("\n Not non-negative matrix!\n");return(-1);}
    D[i,i]<-sqrt(D[i,i]);
  }
  P<-Asvd$u%*%D%*%t(Asvd$v);
  return(P);
}

#### self-defined function for non-negative matrix decomposition
#### A=P^2, return P^{-1}=A^{-1/2}
#### idea: A=UDV^T, P^{-1}=U*sqrt(1/D)*V^T (U=V if A=A^T)
A1self<-function(A) {   ## A: p*p non-negative matrix
  p<-dim(A)[1];
  Asvd<-svd(A);
  D<-matrix(0,p,p);
  for(i in 1:p) {
    D[i,i]<-Asvd$d[i];
    if(D[i,i]<0) {cat("\n Not non-negative matrix!\n");return(-1);}
    D[i,i]<-sqrt(1/D[i,i]);
  }
  P<-Asvd$u%*%D%*%t(Asvd$v);
  return(P);
}


#### Calculate the mean for each slice 
calculate_slice_mean <- function(sufficient_stat) {
  # Calculate the sum of x*x_t and x_bar and x_bar_each_slice
  n_h <- sufficient_stat$total_number_slice
  sum_h <- sufficient_stat$sum_h
  m_h_original <- sum_h/n_h
  m_x <- colSums(sum_h)/sum(n_h)
  sig_xx_2 <- sufficient_stat$sig_xx_2
  
  # (m_h* - m_x)^t * sig_xx_1/2 = slice mean 
  m_h <- (m_h_original - matrix(m_x, nrow(m_h_original), ncol(m_h_original),byrow = TRUE)) %*% sig_xx_2
  m_h
}

#### calculate the weighted covariance matrix 
calculate_SIR_direction <- function(slice_mean, weight, sig_xx_2){
  
  # v <- sum of p_h * m_h *m_h_t, which can be done in matrix format
  v <- t(slice_mean) %*% sweep(slice_mean, 1, weight, "*")
  v_egien<-eigen(v)
  v_eigenvalues<-v_egien$values
  v_eigenvectors<-sig_xx_2%*%v_egien$vectors
  stat <- apply(v_eigenvectors, 2, function(x) sqrt(sum(x^2)))
  v_eigenvectors_scale <- sweep(v_eigenvectors, 2, STATS = stat, FUN = "/")
  list(v = v, v_eigenvectors_scale = v_eigenvectors_scale, eigenvalues = v_eigenvalues)
}

#### chi_square test for sir 
dr_test_sir<-function(object,numdir=4,...) {
#compute the sir test statistic for the first numdir directions
    e<-sort(object$eigenvalues)
    p<-length(object$eigenvalues)
    n<-object$cases
    st<-df<-pv<-0
    nt <- min(p,numdir)
    for (i in 0:(nt-1))
      {st[i+1]<-n*(p-i)*mean(e[seq(1,p-i)])
       df[i+1]<-(p-i)*sum(object$slice_number-i-1)
       pv[i+1]<-1-pchisq(st[i+1],df[i+1])
    }
    
    z<-data.frame(cbind(st,df,pv))
    rr<-paste(0:(nt-1),"D vs >= ",1:nt,"D",sep="")
    dimnames(z)<-list(rr,c("Stat","df","p.value")) 
    z
}

###############################################################################################

### helper functions for block sir method ###
 
###############################################################################################

#### generate file_list 
#### the return file is a character object
generate_file_list <- function(prefix = NULL, suffix = NULL, sep =NULL, location, file_name = NA) {
  if(is.na(file_name)) {
    file_name <- paste(prefix, sep = sep, suffix)
  } 
    file_list <- paste(location, file_name, sep = "")
    file_list
}

#### read the header of the block files 
#### assume the first file with "01" has the header

read_header <- function(input_file) {
  read.csv(input_file, header = T) %>% colnames(.)
}


list_sum <- function(x, y) list (
  sum_x_x_t = x$sum_x_x_t + y$sum_x_x_t,
  sum_h = x$sum_h + y$sum_h,
  total_number_slice = x$total_number_slice + y$total_number_slice
  )

calculate_sufficient_data_block <- function(input_file, slice_levels, header, ...){
  
  result <- foreach::foreach(f = input_file, .combine = list_sum, ...) %dopar% {
    block_data <- read.table(f, header = FALSE, col.names = header, sep = ",", stringsAsFactors = FALSE)
    x_location <- grep("x", header)
    slice_location <- grep("slice", header)
    x <- block_data[,x_location] %>% data.matrix(.)
    slice <- block_data[,slice_location]
    sum_x_x_t <- crossprod(x)

    # generate the sum_h matrix, in case a block not including all slice leves
    missing_slice <- subset(slice_levels, !(slice_levels %in% unique(slice)))
    if (length(missing_slice)>0) {
       slice <- append(slice, missing_slice)
       x <- matrix(0, nrow = length(missing_slice), ncol = ncol(x)) %>% 
              rbind(x, .)
    }

    # using keyby, so sum_h sorted based on slice
    tem_table <- data.table::data.table(x, slice)
    sum_h <- tem_table[, lapply(.SD, sum), keyby = slice][, slice:=NULL] %>% data.matrix(.)
    
    # in case a block does not include all slice levels
    tem_total <- table(slice)
    tem_total[missing_slice] <- 0
    total_number_slice <- as.numeric(tem_total)
    
    result_b <- list(sum_x_x_t = sum_x_x_t, sum_h = sum_h, total_number_slice = total_number_slice)
  }
  
  m_x <- colSums(result$sum_h)/sum(result$total_number_slice)
  sig_xx_2 <- (result$sum_x_x_t/sum(result$total_number_slice) - tcrossprod(m_x)) %>% A1self(.)
  
  result$sig_xx_2 <- sig_xx_2
  
  result
}



#### Calculate the mean for each slice 
calculate_slice_mean <- function(sufficient_stat) {
  # Calculate the sum of x*x_t and x_bar and x_bar_each_slice
  n_h <- sufficient_stat$total_number_slice
  sum_h <- sufficient_stat$sum_h
  m_h_original <- sum_h/n_h
  m_x <- colSums(sum_h)/sum(n_h)
  sig_xx_2 <- sufficient_stat$sig_xx_2
  
  # (m_h* - m_x)^t * sig_xx_1/2 = slice mean 
  m_h <- (m_h_original - matrix(m_x, nrow(m_h_original), ncol(m_h_original),byrow = TRUE)) %*% sig_xx_2
  m_h
}

#### calculate the weighted covariance matrix 
calculate_SIR_direction <- function(slice_mean, weight, sig_xx_2){
  
  # v <- sum of p_h * m_h *m_h_t, which can be done in matrix format
  v <- t(slice_mean) %*% sweep(slice_mean, 1, weight, "*")
  v_egien<-eigen(v)
  v_eigenvalues<-v_egien$values
  v_eigenvectors<-sig_xx_2%*%v_egien$vectors
  stat <- apply(v_eigenvectors, 2, function(x) sqrt(sum(x^2)))
  v_eigenvectors_scale <- sweep(v_eigenvectors, 2, STATS = stat, FUN = "/")
  list(v = v, v_eigenvectors_scale = v_eigenvectors_scale, eigenvalues = v_eigenvalues)
}

## self-defined function for SIR, based on Li(1991)
## output: eigenvalues, eigenvectors, matrix v, total number: case, the number of slice leves 
## read the data block_by_block and calcuate the sufficient statistcs
## based on Zhang (2016)
SIR_blocks <- function (input_file, slice_levels, header, ...) {
  sufficient_stat <- calculate_sufficient_data_block(input_file, slice_levels, header, ...)
  m_h <- calculate_slice_mean(sufficient_stat)
  result <- calculate_SIR_direction(slice_mean = m_h, sig_xx_2 = sufficient_stat$sig_xx_2, weight = sufficient_stat$total_number_slice/sum(sufficient_stat$total_number_slice) )
  result <- append(result, list(cases = sum(sufficient_stat$total_number_slice), slice_number = length(slice_levels)))
  result 
}

###############################################################################################

### helper functions for row sir method ###
 
###############################################################################################

sufficient_stat_each_row <- function (x, x_slice, slice_levels){
  x_x_t <- crossprod(x)
  sum_h <- matrix(0, nrow = length(slice_levels), ncol = length(x))      
  sum_h[match(x_slice, slice_levels),] <- x
  # return which slice this rows belong to 
  slice_indicator <- match(slice_levels, x_slice, nomatch = 0)
  list(x_x_t = x_x_t, sum_h = sum_h, slice_indicator = slice_indicator)
}

#### Calculate the sufficient data for each row
#### return a list of 3 sufficient statistics 
calculate_sufficient_data <- function(x, slice){
  n <- nrow(x)
  slice_levels <- unique(slice)
  sum_x_x_t <- matrix(0, ncol(x), ncol(x))
  sum_h <- matrix(0, length(slice_levels), ncol(x))
  total_number_slice <- numeric(length(slice_levels))
  
  for (i in 1:n) {
    suff_stat <- sufficient_stat_each_row(x[i,], slice[i], slice_levels)
    sum_x_x_t <- sum_x_x_t + suff_stat[[1]]
    sum_h <- sum_h + suff_stat[[2]]
    total_number_slice <- total_number_slice + suff_stat[[3]]
  }
  
  m_h_original <- sum_h/total_number_slice
  m_x <- colSums(sum_h)/sum(total_number_slice)
  sig_xx_2 <- (sum_x_x_t/sum(total_number_slice) - tcrossprod(m_x)) %>% A1self(.)
  list (sum_x_x_t = sum_x_x_t, sum_h = sum_h, total_number_slice = total_number_slice, sig_xx_2 = sig_xx_2)
}


#### Calculate the sufficient data for each row
#### return a list of 3 sufficient statistics 
calculate_sufficient_data_disk <- function(input_file, ncol, slice_levels){
  
  sum_x_x_t <- matrix(0, ncol, ncol)
  sum_h <- matrix(0, length(slice_levels), ncol)
  total_number_slice <- numeric(length(slice_levels))
  
  con  <- file(input_file, open = "r")
  header <- scan(con,what="numeric",nlines=1,sep=',',quiet=TRUE)
  x_location <- grep("x", header)
  slice_location <- grep("slice", header)
  
  while (length(row <- scan(con,what="numeric",nlines=1,sep=',',quiet=TRUE)) > 0 ){
    ## here I print , but you must have a process your line here
    x <- row[x_location] %>% as.numeric(.) %>% matrix(., nrow = 1)
    slice <- row[slice_location]
    suff_stat <- sufficient_stat_each_row(x, slice, slice_levels)
    sum_x_x_t <- sum_x_x_t + suff_stat[[1]]
    sum_h <- sum_h + suff_stat[[2]]
    total_number_slice <- total_number_slice + suff_stat[[3]]
  } 
  
  close(con)
  
  gm_h_original <- sum_h/total_number_slice
  m_x <- colSums(sum_h)/sum(total_number_slice)
  sig_xx_2 <- (sum_x_x_t/sum(total_number_slice) - tcrossprod(m_x)) %>% A1self(.)
  list (sum_x_x_t = sum_x_x_t, sum_h = sum_h, total_number_slice = total_number_slice, sig_xx_2 = sig_xx_2)
}


## self-defined function for SIR, based on Li(1991)
## output: eigenvalues, eigenvectors, matrix v 
## read the data row by rows and calcuate the sufficient statistcs
## based on Zhang (2016)
SIR_rows <- function (input_file, ncol, slice_levels) {
  sufficient_stat <- calculate_sufficient_data_disk(input_file, ncol, slice_levels)
  m_h <- calculate_slice_mean(sufficient_stat)
  result <- calculate_SIR_direction(slice_mean = m_h, sig_xx_2 = sufficient_stat$sig_xx_2, weight = sufficient_stat$total_number_slice/sum(sufficient_stat$total_number_slice) )
  result <- append(result, list(cases = sum(sufficient_stat$total_number_slice), slice_number = length(slice_levels)))
  result
}