## transform to rank
data_path <- "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual/PCB_1999_2004_common.csv"
data <- read.csv(data_path, header = T, stringsAsFactors = T)
rank_data <- apply(data, MARGIN = 2,FUN = rank)
write.csv(rank_data, file = "~/dev/projects/Chen_environmental_study/R_code/data/real_data/NHANES/PCB_99_14/clean/individual/rank_PCB_1999_2004_common.csv", row.names = FALSE)
