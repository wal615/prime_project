# Following simulation is to test if under normal distribution, we can estimate the interaction effect unbaisly by proposed method

setwd("~/dev/projects/Chen_environmental_study/")
source("./R_code/Yang_REML.R")

library(sas7bdat)
library(MASS)
library(tidyverse)
library(foreach)
library(doRNG)
library(doParallel)


test_result <-simulation_fn(n = 1000,
                            p = 34,
                            brep = 200,
                            nrep = 20,
                            interaction = 1, 
                            interaction_m = 1,
                            seed = 123,
                            cores = 10)


save(test_result, file = "./result/simulation_fixed_random_test")
load(file = "./result/simulation_fixed_random_test")

main <- test_result[test_result[,1] != 0, c(1,3,5)] %>% data.frame(.)
tidyr::gather(main, key = "effect", value = "value") %>%
  ggplot(., aes(x = effect, y = value, fill = effect)) +
  geom_violin()+
  geom_boxplot()

inter <- test_result[test_result[,1] != 0, c(2,4,6)] %>% data.frame(.)
  tidyr::gather(inter, key = "effect", value = "value") %>%
  ggplot(., aes(x = effect, y = value, fill = effect)) +
  geom_violin()+
  geom_boxplot()

