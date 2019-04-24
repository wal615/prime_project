file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/nhance/hemoglobin/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/nhance/hemoglobin/",.)
file_list <- file_list_0[grep(x = file_list_0, pattern = "imputed_hemoglobin.*total$")]
imputed_hemoglobin_total <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)

by = c("data_name" ,
       "combine", 
       "interaction_m",
       "n",
       "tran_fn_y")

imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total[,  list(y_var = mean(y_var),
                                                                                   GCTA_total = mean(GCTA_total),
                                                                                   proposed_GCTA_total = mean(prop_total),
                                                                                   N_iteration = .N), by = by][,c("est_model") := list(ifelse(interaction_m == 0, "main + inter",""))]
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total_mean_summary[, `:=` (GCTA_precentage = GCTA_total/y_var *100,
                                                                                      proposed_GCTA_precentage = proposed_GCTA_total/y_var*100)]
imputed_hemoglobin_total_mean_summary[,c("combine","interaction_m"):=NULL] 

col_order <- c("data_name", "n", "est_model", "tran_fn_y",
               "y_var","GCTA_total", "GCTA_precentage", "proposed_GCTA_total", "proposed_GCTA_precentage")
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total_mean_summary[, ..col_order]

## combine all the table to final output for prime meeing 
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total_mean_summary %>% setorder(., data_name, tran_fn_y) %>% data.frame(.)

## round to digital 3
col_names <- colnames(imputed_hemoglobin_total_mean_summary)[-(1:4)]
imputed_hemoglobin_total_mean_summary[, -(1:4)] <- imputed_hemoglobin_total_mean_summary[, -(1:4)] %>% round(.,2)
colnames(imputed_hemoglobin_total_mean_summary) <- gsub("_", " ", colnames(imputed_hemoglobin_total_mean_summary))
write.csv(imputed_hemoglobin_total_mean_summary, file = "./reports/proposed_GCTA_paper/nhance/hemolobin/prime_imputed_hemoglobin_total_table_full.csv", row.names = FALSE)

