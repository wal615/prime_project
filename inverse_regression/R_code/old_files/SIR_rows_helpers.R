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
slice_data <- function(x, y, slice_number){
  ordered_data <- data.table::data.table(x = x, y = y) %>% 
                  data.table::setorder(., y)
  # using slice break points to generate slice factor
  slice_position <- slice_method(slice_number, length(y))
  slice <- letters[1:slice_number] %>% 
           rep(., (slice_position[2,]-slice_position[1,]+1))
  ordered_data[,slice:= slice] %>% data.frame(.)
}

#### Calculate the sufficient statistics 
## assume that sliced data should be a vector
## assume x is a row vector

sufficient_stat_each_row <- function (x, x_slice, slice_level){
  x_x_t <- crossprod(x)
  sum_h <- matrix(0, nrow = length(slice_level), ncol = length(x))      
  sum_h[match(x_slice, slice_level),] <- x
  # return which slice this rows belong to 
  slice_indicator <- match(slice_level, x_slice, nomatch = 0)
  list(x_x_t = x_x_t, sum_h = sum_h, slice_indicator = slice_indicator)
}

#### Calculate the sufficient data for each row
#### return a list of 3 sufficient statistics 
calculate_sufficient_data <- function(x, slice){
  n <- nrow(x)
  slice_level <- unique(slice)
  sum_x_x_t <- matrix(0, ncol(x), ncol(x))
  sum_h <- matrix(0, length(slice_level), ncol(x))
  total_number_slice <- numeric(length(slice_level))
  
  for (i in 1:n) {
    suff_stat <- sufficient_stat_each_row(x[i,], slice[i], slice_level)
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
calculate_sufficient_data_disk <- function(input_file, ncol, slice_level){
  
  sum_x_x_t <- matrix(0, ncol, ncol)
  sum_h <- matrix(0, length(slice_level), ncol)
  total_number_slice <- numeric(length(slice_level))
  
  con  <- file(input_file, open = "r")
  header <- scan(con,what="numeric",nlines=1,sep=',',quiet=TRUE)
  x_location <- grep("x", header)
  slice_location <- grep("slice", header)
  
  while (length(row <- scan(con,what="numeric",nlines=1,sep=',',quiet=TRUE)) > 0 ){
    ## here I print , but you must have a process your line here
    x <- row[x_location] %>% as.numeric(.) %>% matrix(., nrow = 1)
    slice <- row[slice_location]
    suff_stat <- sufficient_stat_each_row(x, slice, slice_level)
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
## output: eigenvalues, eigenvectors, matrix v 
## read the data row by rows and calcuate the sufficient statistcs
## based on Zhang (2016)
SIR_rows <- function (input_file, ncol, slice_level) {
  sufficient_stat <- calculate_sufficient_data_disk(input_file, ncol, slice_level)
  m_h <- calculate_slice_mean(sufficient_stat)
  result <- calculate_SIR_direction(slice_mean = m_h, sig_xx_2 = sufficient_stat$sig_xx_2, weight = sufficient_stat$total_number_slice/sum(sufficient_stat$total_number_slice) )
  result <- append(result, list(cases = sum(sufficient_stat$total_number_slice), slice_number = length(slice_levels)))

  result
}