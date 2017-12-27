root_path <- "../../../../dr_sim_data/"

source("../simulation_helpers.R")
source("../simulation_string.R")
size_seq <- seq(3,4,0.25) 
n <- 10^(size_seq)
slice_number <- 8 
block <- 5
file_list <- c(Li_2009)

mapply(FUN = generate_simulation, n = n, 
       MoreArgs = list(root_path = root_path,
           		      sim_string = file_list,
           		    slice_number = slice_number, 
   				    slice_method = slice_data_with_index_dr, 
       				  cut_method = cut_data_mac,
       				           p = 6),
       SIMPLIFY = TRUE)

# for (i in 1:length(file_list)) {
#   generate_simulation(n, p, sim_string = file_list[i], slice_number = slice_number, root_path, slice_method = slice_data_with_index_dr)
# }

# for (i in (1:length(sliced_data_name))) {
#   cut_data_mac(input_file = sliced_data_name[[i]], root_path = root_path, block = 5)
# }
