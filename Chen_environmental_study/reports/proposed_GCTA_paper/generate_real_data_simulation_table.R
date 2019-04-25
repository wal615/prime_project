file_list_0 <- list.files("./result/simulation_proposed_GCTA_paper/nhance/hemoglobin/") %>%
  paste0("./result/simulation_proposed_GCTA_paper/nhance/hemoglobin/",.)
file_list <- file_list_0[grep(x = file_list_0, pattern = "imputed_hemoglobin.*total$")]
imputed_hemoglobin_total <- lapply(file_list, function (x) {readRDS(x) %>% rbindlist(.)}) %>% rbindlist(.)

# change the data_name into 2 groups 
imputed_hemoglobin_total$data_name <- ifelse(imputed_hemoglobin_total$data_name =="imputed_hemoglobin_0", 0,1)

# average the estimation
by = c("data_name" ,
       "interaction_m",
       "tran_fn_y")
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total[,  list(y_var = mean(y_var),
                                                                                   GCTA_total = mean(GCTA_total),
                                                                                   proposed_GCTA_total = mean(prop_total)), by = by][,c("est_model") := list(ifelse(interaction_m == 0, "main + inter",""))]
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total_mean_summary[, `:=` (GCTA_precentage = GCTA_total/y_var *100,
                                                                                      proposed_GCTA_precentage = proposed_GCTA_total/y_var*100)]
imputed_hemoglobin_total_mean_summary[,c("interaction_m"):=NULL] 

col_order <- c("data_name", "est_model", "tran_fn_y",
               "y_var","GCTA_total", "GCTA_precentage", "proposed_GCTA_total", "proposed_GCTA_precentage")
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total_mean_summary[, ..col_order]
imputed_hemoglobin_total_mean_summary <- imputed_hemoglobin_total_mean_summary %>% setorder(., data_name, tran_fn_y) %>% data.frame(.)
names(imputed_hemoglobin_total_mean_summary)[1] <- "imputation"
imputed_hemoglobin_total_mean_summary$imputation <- ifelse(imputed_hemoglobin_total_mean_summary$imputation =="0", "method 1","method 2")

## round to digital 3
col_names <- colnames(imputed_hemoglobin_total_mean_summary)[-(1:3)]
imputed_hemoglobin_total_mean_summary[, -(1:3)] <- imputed_hemoglobin_total_mean_summary[, -(1:3)] %>% round(.,2)
colnames(imputed_hemoglobin_total_mean_summary) <- gsub("_", " ", colnames(imputed_hemoglobin_total_mean_summary))
write.csv(imputed_hemoglobin_total_mean_summary, file = "./reports/proposed_GCTA_paper/nhance/hemolobin/prime_imputed_hemoglobin_total_table_full.csv", row.names = FALSE)

