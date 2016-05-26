setwd('XXXXXXXX')

file.sources = list.files(pattern="*.R")
sapply(paste0(file.sources), source, .GlobalEnv)
