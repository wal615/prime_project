setwd("~/dev/projects/inverse_regression/R_code/")
root_path <- "../sim_data/"

source("./simulation_helpers.R")
source("./simulation_string.R")

n = 1000
p = 6
slice_number <- 8 
block <- 5
file_list <- c(Li_2009, mussles)

sliced_data_name <- mapply(FUN = generate_simulation, sim_string = file_list, n = c(n, numeric(0)), p = c(p, numeric(0)), 
                           MoreArgs = list(root_path = root_path, slice_number = slice_number, slice_method = slice_data_with_index_dr),
                           SIMPLIFY = TRUE)

# for (i in 1:length(file_list)) {
#   generate_simulation(n, p, sim_string = file_list[i], slice_number = slice_number, root_path, slice_method = slice_data_with_index_dr)
# }

for (i in (1:length(sliced_data_name))) {
  cut_data_mac(input_file = sliced_data_name[[i]], root_path = root_path, block = 5)
}



