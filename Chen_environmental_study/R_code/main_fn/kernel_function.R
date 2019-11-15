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

GCTA_rr_kernel <- function(...){
  args <- list(...)
  if(args$decor == TRUE){
    fit <- GCTA_rr(y = args$y,
                   x = args$s_final) 
  } else {
    fit <- GCTA_rr(y = args$y,
                   x = args$b_final)
  }
  fit
}
col_names_GCTA_rr <- c("GCTA_rr_main")

EigenPrism_kernel <- function(...){
  args <- list(...)
  if(args$decor == TRUE){
    fit <- EigenPrism(y = args$y, 
                      X = args$s_final)  
  } else {
    fit <- EigenPrism(y = args$y, 
                      X = args$b_final)  
  }
  c(fit[1], fit[3]-fit[2])
}
col_names_Eigen <- c("EigenPrism_main", "EigenPrism_CI")


h_GCTA_kernel <- function(...) {
  args <- list(...)
  if(args$decor == TRUE){
    fit <- Yang(y = args$y, 
                x = args$s_final, 
                interact = args$interact,
                target = "heritability")  
  } else {
    fit <- Yang(y = args$y, 
                x = args$b_final, 
                interact = args$interact,
                target = "heritability")
  }
  c(fit$G, fit$RACT)
}
col_names_h_GCTA <- c("h_GCTA_main", "h_GCTA_inter")

h_EigenPrism_kernel <- function(...){
  args <- list(...)
  if(args$decor == TRUE){
    fit <- EigenPrism(y = args$y, 
                      X = args$s_final,
                      target = "heritability")  
  } else {
    fit <- EigenPrism(y = args$y, 
                      X = args$b_final,
                      target = "heritability")  
  }
  fit[1:3]
}
col_names_h_Eigen <- c("h_EigenPrism_main", "h_EigenPrism_CI1","h_EigenPrism_CI2")



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


single_cor_kernel <- function(...){
  args <- list(...)
  fit <- cor(args$b_final[,1], args$b_final[,2])
  fit
}
col_names_single_cor <- c("cor_main")
