GCTA_kernel <- function(b_final, s_final, y, interact) {
  fit <- Yang(y = y, x = b_final, interact = interact)
  fit_sub <- Yang(y = y, x = s_final, interact = interact)
  c(fit$G, fit$RACT, fit_sub$G, fit_sub$RACT)
}