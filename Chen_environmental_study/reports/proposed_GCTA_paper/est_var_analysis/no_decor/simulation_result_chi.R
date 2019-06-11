options(warn = 1, error = bettertrace::stacktrace)
R.utils::sourceDirectory("~/dev/projects/Chen_environmental_study/R_code/main_fn",modifiedOnly = FALSE)
setwd("~/dev/projects/Chen_environmental_study/result/simulation_proposed_GCTA_paper/var_est/non_decore/")
save_path <- "~/dev/projects/Chen_environmental_study/reports/proposed_GCTA_paper/est_var_analysis/no_decor/"


result_args_generate <- function(long_name){
  long_list <- strsplit(x = long_name, split = "_", fixed = TRUE) %>% unlist(.)
  est_signal <- long_list[length(long_list)] 
  index <- c("GCTA", "EigenPrism", "square") %in% long_list 
  methods <- c("GCTA", "EigenPrism", "least_square")[index]
  name_list <- list()
  for(m in methods){
    if(m == "GCTA" & ("I" %in% long_list)){
      col_names <- paste0("GCTA_", est_signal)
      } else {
      col_names <- paste0("prop_", est_signal)}
    if(m == "EigenPrism"){
      col_names <- c(paste0("EigenPrism_", est_signal), "EigenPrism_CI1","EigenPrism_CI2")  
    }  else if(m == "least_square"){
      col_names <- c(paste0("least_square_", est_signal))
    }
    name_list <- append(name_list, list(col_names))
  }  
  args <- expand.grid(result_list_path = long_name, 
                      method = methods, 
                      est_signal = est_signal,
                      col_names = name_list, stringsAsFactors = FALSE) %>% data.table(.)
  args[,index := grepl(pattern = method,x = col_names), by = seq_len(nrow(args))]
  args <- args[index == TRUE,][,index:=NULL]
  args
}

result_list <- list.files(path = "./",full.names = FALSE)
args_table <- mapply(FUN = result_args_generate, long_name =result_list, SIMPLIFY = FALSE) %>% rbindlist(.)
  

result_list <- mapply(FUN = generate_result, 
                      result_path = args_table[,result_list_path],
                      method = args_table[,method],
                      est_signal = args_table[,est_signal],
                      col_names = args_table[,col_names],
                      MoreArgs = list(upper = 0.9, lower = 0.1), 
                      SIMPLIFY = FALSE) %>% rbindlist(.)
write.csv(file = "./result_list.csv",x = result_list, row.names = FALSE)
