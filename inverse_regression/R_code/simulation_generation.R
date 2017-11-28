# setwd("~/dev/projects/inverse_regression/R_code/")
root_path <- "../sim_data/"

source("./simulation_helpers.R")
source("./simulation_string.R")

n = 1000
p = 6

n_block <- 8

file_list <- c(Li_2009)
for (i in 1:length(file_list)) {
  generate_simulation(n, p, sim_string = file_list[i], slice_number = 8, root_path) %>% print(.)
}

cut_data_mac(input_file = c("Li_2009_y_1", "Li_2009_y_2"), root_path = root_path, block = 5)




