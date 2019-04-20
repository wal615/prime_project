update_LC_each <- function(PCB, PCB_LC){
  LOD <- numeric(length(PCB))
  # if the missing patter does not match then return missing
  if(any(is.na(PCB) != is.na(PCB_LC))) stop("missingn pattern no mattch")
  index <- PCB_LC == 1
  index[is.na(index)] <- FALSE
  LOD[index] <- PCB[index] * sqrt(2) # get the lod 
  PCB[index] <- NA # remove the imputation values
  list(PCB = PCB, PCB_LC = PCB_LC, LOD = LOD)
}

update_LC <- function(data, PCB_names){
  for(n in PCB_names){
    cat(n, "is done \n")
    selected_col <- colnames(data)[grep(pattern = n, x = colnames(data))]
    selected_col <- c(selected_col, paste0(n, "LOD"))
    data[,(selected_col):= update_LC_each(PCB = eval(parse(text = selected_col[1])),
                                          PCB_LC = eval(parse(text = selected_col[2])))]
  }
  data
}
