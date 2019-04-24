update_LC_each <- function(PCB, PCB_LC){
  missing <- numeric(length(PCB))
  # if the missing patter does not match then return missing
  if(any(is.na(PCB) != is.na(PCB_LC))) stop("missingn pattern no mattch")
  index <- PCB_LC == 1
  index[is.na(index)] <- FALSE
  PCB[index] <- PCB[index] * sqrt(2) # relace imputation with the lod 
  missing[index] <- 2 # left censoring is 2 
  missing[is.na(PCB)] <- 1 # missing is 1  
  list(PCB = PCB, missing = missing)
}

update_LC <- function(data, PCB_names){
  for(n in PCB_names){
    cat(n, "is done \n")
    selected_col <- colnames(data)[grep(pattern = n, x = colnames(data))]
    update_col <- c(n, paste0(n, "_missing"))
    data[,(update_col):= update_LC_each(PCB = eval(parse(text = selected_col[1])),
                                          PCB_LC = eval(parse(text = selected_col[2])))]
  }
  data
}

missing_indicator <- function(x) {
  is.na(x) %>% ifelse(., 1, 0)
}

add_missing_indicator <- function(data, var_names){
  for(n in var_names){
    cat(n, "is done \n")
    data[,paste0(n,"_missing") := missing_indicator(eval(parse(text = n)))]
  }
}