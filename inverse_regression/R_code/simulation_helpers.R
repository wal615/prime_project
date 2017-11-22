#### get the slice position of the 
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
    set(., j = "index", value = NULL) %>%
        data.frame(.) 
}

#### generate the simulation data based on the given profile 
#### return a list of data_file names

generate_simulation <- function(n, p, name_list) {

}
