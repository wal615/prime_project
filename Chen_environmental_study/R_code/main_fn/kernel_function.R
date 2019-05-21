GCTA_kernel <- function(...) {
  args <- list(...)
  fit <- Yang(y = args$y, x = args$b_final, interact = args$interact)
  fit_sub <- Yang(y = args$y, x = args$s_final, interact = args$interact)
  c(fit$G, fit$RACT, fit_sub$G, fit_sub$RACT)
}
col_names_GCTA <- c("GCTA_main", "GCTA_inter", "prop_main", "prop_inter")

EigenPrism_kernel <- function(b_final, s_final, y, interact){
  fit <- EigenPrism_m(y = args$y, 
                      X = args$b_final)
  fit[1:3]
}
col_names_Eigen <- c("Eigen_main", "CI1", "CI2")

signal_kernel <- function(...){
  args <- list(...)
  fit <- signal(args$b_final, args$betam, args$betai)
  fit[1:4]
}
col_names_signal <- c("main", "inter", "cov","total")