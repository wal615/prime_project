slice_level <- letters[1:8]
setwd("~/dev/projects/inverse_regression/R_code/")
root_path <- "../sim_data/"

####################################################################
source("SIR_blocks_helpers.R")
source("SIR_rows_helpers.R")
source("sir_self.R")

#####################################################################
## block
inputfile <- generate_file_list(prefix = "Li_2009_y_1_sim_block", 
                                suffix = formatC(1:5,flag=0,width=2), 
                                sep = "_", location = root_path, 
                                )

header <- read_header(input_file = "../sim_data/mega_Li_2009_y_1")
sir_block <- SIR_blocks(input_file = inputfile, slice_level = 8, header = header)
## rows
inputfile <- paste0(root_path, "Li_2009_y_1.csv")
sir_row <- SIR_rows(inputfile, ncol = 6, slice_level

## Yang 
data <- read.csv(inputfile)
y <- data$y %>% data.matrix(.) 
x <- data[,1:6] %>% data.matrix(.)
sir_Yang <- sirself(y = y, x = x, p = 6, n = nrow(data), slice = 8)
                    