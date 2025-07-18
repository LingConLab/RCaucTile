library(tidyverse)
ec_languages <- read_csv("dataset_creation/ec_languages.csv")
save(ec_languages, file="data/ec_languages.RData", compress='xz')
