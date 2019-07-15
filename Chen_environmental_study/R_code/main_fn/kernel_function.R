GCTA_kernel <- function(...) {
  args <- list(...)
  if(args$decor == TRUE){
    fit <- Yang(y = args$y, 
                x = args$s_final, 
                interact = args$interact)  
  } else {
    fit <- Yang(y = args$y, 
                x = args$b_final, 
                interact = args$interact)
  }
  c(fit$G, fit$RACT)
}
col_names_GCTA <- c("GCTA_main", "GCTA_inter")

EigenPrism_kernel <- function(...){
  args <- list(...)
  if(args$decor == TRUE){
    fit <- EigenPrism(y = args$y, 
                      X = args$s_final)  
  } else {
    fit <- EigenPrism(y = args$y, 
                      X = args$b_final)  
  }
  fit[1:3]
}
col_names_Eigen <- c("EigenPrism_main", "EigenPrism_CI1","EigenPrism_CI2")

least_square_kernel <- function(...){
  args <- list(...)
  if(args$decor == TRUE){
    fit <- least_square(y = args$y, 
                        x = args$s_final)  
  } else {
    fit <- least_square(y = args$y, 
                        x = args$b_final)  
  }
  fit[1]
}
col_names_least_square <- c("least_square_main")

least_square_kernel_e <- function(...){
  args <- list(...)
  if(args$decor == TRUE){
    fit <- least_square_e(y = args$y, 
                        x = args$s_final)  
  } else {
    fit <- least_square_e(y = args$y, 
                        x = args$b_final)  
  }
  fit[1]
}
col_names_least_square_e <- c("least_square_e")



signal_kernel <- function(...){
  args <- list(...)
  fit <- signal(args$b_raw, args$betam, args$betai)
  fit[1:4]
}
col_names_signal <- c("main", "inter", "cov","total")

single_var_kernel <- function(...){
  args <- list(...)
  fit <- single_var(args$b_raw)
  fit[1:3]
}
col_names_single_var <- c("singal_var1","singal_var2", "cov")