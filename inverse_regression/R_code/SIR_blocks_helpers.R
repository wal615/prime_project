library(foreach)
library(tidyverse)


### helper functions for the SIR_rows_by_rows ###
## slice data by y values and sign each row a group category
## return a 2-by-slice matrix 


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


#### Calculate the sufficient data for each block
## assume the data is csv
#### return a list of 3 sufficient statistics 

list_sum <- function(x, y) list (
  sum_x_x_t = x$sum_x_x_t + y$sum_x_x_t,
  sum_h = x$sum_h + y$sum_h,
  total_number_slice = x$total_number_slice + y$total_number_slice
  )

calculate_sufficient_data_block <- function(input_file, slice_levels, header){
  
  result <- foreach::foreach(f = input_file, .verbose = TRUE, .combine = list_sum) %dopar% {
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
SIR_blocks <- function (input_file, slice_levels, header) {
  sufficient_stat <- calculate_sufficient_data_block(input_file, slice_levels, header)
  m_h <- calculate_slice_mean(sufficient_stat)
  result <- calculate_SIR_direction(slice_mean = m_h, sig_xx_2 = sufficient_stat$sig_xx_2, weight = sufficient_stat$total_number_slice/sum(sufficient_stat$total_number_slice) )
  result <- append(result, list(cases = sum(sufficient_stat$total_number_slice), slice_number = length(slice_levels)))
  result 
}