library(readMLData)
pathData <- "~/DATA/readMLData/UCI_ML_DataFolders"
pathDescription <- "~/DATA/readMLData/UCI_ML_DataDescription"

dsList <- prepareDSList(pathData, pathDescription)

for (ind in which(dsList$available)) {
    dat <- dsRead(dsList, ind)
    # DO SOMETHING WITH dat
}
