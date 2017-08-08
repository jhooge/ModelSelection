source("helpers.R")

path <- file.path(getwd(), "models/" )
modelFiles <- list.files(path = path, pattern = "*_model.RData", full.names = T)
for (i in 1:length(modelFiles)) {
  load(modelFiles[i])
}


## gets all objects in the current environment that end on "_model"
## and stores them in a named list, for further downstream access.
modelObjectNames <- ls()[grepl("_model", ls())]
models <- lapply(modelObjectNames, get)
names(models) <- sapply(strsplit(modelObjectNames, '_'), '[', 1)
