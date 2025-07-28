ec_languages <- read.csv("dataset_creation/ec_languages.csv", na.strings = "")
save(ec_languages, file="data/ec_languages.rda", compress='xz')
