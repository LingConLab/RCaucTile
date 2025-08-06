ec_languages <- read.csv("dataset_creation/ec_languages.csv", na.strings = "")
ec_languages$morning_greetings <- factor(ec_languages$morning_greetings,
                                         levels = c("Did you wake up?",
                                                    "Good morning",
                                                    "Both"))
save(ec_languages, file="data/ec_languages.rda", compress='xz')
