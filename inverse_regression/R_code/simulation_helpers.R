library(tidyverse)
library(data.table)
library(dr)

#### order the data by y and append a columan for group 
#### return a data.frame ordered by y and appended a group colomn
#### Using the slice function from dr package
slice_data_with_index_dr <- function(x, y, slice_number){
  slice <- dr.slices(y, slice_number)$slice.indicator %>% letters[.]
  data.frame(x = x, y = y, slice = slice)
}

#### self define slice function, assume y are ordered 
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
slice_data_with_index <- function(x, y, slice_number){
  ordered_data <- data.table::data.table(x = x, y = as.numeric(y)) 
  
  ordered_data[, index:= .I] %>% data.table::setorder(., y)
  # .I is the special value in data.table
  
  # using slice break points to generate slice factor
  slice_position <- slice_method(slice_number, length(y))
  
  slice <- letters[1:slice_number] %>% 
    rep(., (slice_position[2,]-slice_position[1,]+1))
  
  ordered_data[,slice:= slice] %>% 
    data.table::setorder(., index) %>%
    data.table::set(., j = "index", value = NULL) %>%
        data.frame(.) 
}

#### cut data into blocks 
#### using shell commend, testing on mac os

cut_data_mac <- function(input_file, block, root_path) {
  for (f in input_file) {
    paste0("Spliting the inputfile ",f, "\n") %>% cat(.)
    file_path <- paste0(root_path, f, ".csv")
    cmd_header <- "head -n 1 {file_path} > {root_path}header_{f}"
    glue::glue(cmd_header) %>% system(command = .)
    cmd_split <- "gsplit -n l/{block} --numeric-suffixes=1 {file_path} {root_path}{f}_sim_block_ --verbose"
    glue::glue(cmd_split) %>% system(command = .)
    cmd_delete_header <- "sed '1d' {root_path}{f}_sim_block_01 > {root_path}tmpfile; mv {root_path}tmpfile {root_path}{f}_sim_block_01"
    glue::glue(cmd_delete_header) %>% system(command = .)
    cat("Done\n")
  }
  
}

#### cut data into blocks 
#### using shell commend, testing on mac os

cut_data_linux <- function(input_file, block, root_path) {
  for (f in input_file) {
    paste0("Spliting the inputfile ",f, "\n") %>% cat(.)
    file_path <- paste0(root_path, f, ".csv")
    cmd_header <- "head -n 1 {file_path} > {root_path}header_{f}"
    glue::glue(cmd_header) %>% system(command = .)
    cmd_split <- "split -n l/{block} --numeric-suffixes=1 {file_path} {root_path}{f}_sim_block_ --verbose"
    glue::glue(cmd_split) %>% system(command = .)
    cmd_delete_header <- "sed '1d' {root_path}{f}_sim_block_01 > {root_path}tmpfile; mv {root_path}tmpfile {root_path}{f}_sim_block_01"
    glue::glue(cmd_delete_header) %>% system(command = .)
    cat("Done\n")
  }
  
}

#### generate the simulation data based on the given profile 
#### return a list of data_file names

generate_simulation <- function(n = NULL, 
                                p = NULL, 
                                sim_string, 
                                slice_number, 
                                root_path, 
                                slice_method,
                                cut_method,
                                block = 5) {
  exp <- glue::glue(sim_string) %>% parse(text = .) 
  eval(exp) # need to be careful of the envir of "eval" and usage of pipe 
  y_names <- names(y)
  file_path <- paste0(root_path, data_name, "_", y_names, ".csv") # data name from sim_string
  
  cat("write the simulation data into disk...\n")
  
  for(i in 1:length(y)) {
    y_i <- y[[i]] %>% as.numeric(.) # data.table need the y to be a vector instead of matrix
    slice_method(x, y_i, slice_number) %>% 
    write.csv(., file = file_path[i], row.names = FALSE)
  }

  cat("cut data into block...\n")

  cut_method(paste0(data_name, "_", y_names), block, root_path)

  cat("write the mega data into disk...\n")

  data.frame(n = n, p = p, 
             data_name = data_name, 
             slice_number = slice_number, 
             block = block, 
             data_list = paste0(data_name, "_", y_names)) %>% 
  write.csv(., file = paste0(root_path, data_name, "_mega", ".csv"), row.names = FALSE)
  
  cat("Done\n")
  
  
  }

