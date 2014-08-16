pcaSdev <- function(x) {
    pca <- princomp(x, cor=TRUE)
    cumsum(pca$sdev^2)/sum(pca$sdev^2)
}
dfNumeric <- function(x) {
    # Only keep numeric, non-constant columns
    numCols <- sapply(x, function(y) is.numeric(y))
    y <- na.omit(x[, numCols, drop=FALSE])
    colVar <- sapply(y, var)
    indxConstant <- which(colVar < 1.5e-8)
    if(length(indxConstant) > 0) {
        y <- y[, -indxConstant]
    }
    attr(y, "na.action") <- NULL
    y
}
pcaFromDataObjectOrName <- function(data, dname=NULL) {
    if(is.character(data)) {
        dname <- data
        y <- get(data)
    } else {
        if(is.null(dname)) {
            dname <- deparse(substitute(data))
        }
        y <- data
    }
    z <- NULL
    if(inherits(y, "data.frame")) {
        y <- dfNumeric(y)
        if(dim(y)[2] > 2 && dim(y)[1] > dim(y)[2]) {
            dy <- dim(y)
            cumPercentVar <- pcaSdev(y)
            z <- list(name=dname, n=dy[1], p=dy[2],
                ncomp=seq(along=cumPercentVar), cpv=cumPercentVar)
        }
    }
    z
}

pcaFromListOfSDatasets <- function(dlist, outFile="pca01.csv") {
    for(i in dlist) {
        pcadf <- pcaFromDataObjectOrName(i)
        if(!is.null(pcadf)) {
            write.table(as.data.frame(pcadf), file=outFile, append=TRUE,
                row.names=FALSE, col.name=FALSE, sep=",")
        }
    }
    invisible(NULL)
}

pcaFromListFileNames <- function(flist, outFile="pca01.csv") {
    for(i in flist) {
        data <- read.csv(i, header=TRUE)
        pcadf <- pcaFromDataObjectOrName(data, dname=i)
        if(!is.null(pcadf)) {
            write.table(as.data.frame(pcadf), file=outFile, append=TRUE,
                row.names=FALSE, col.name=FALSE, sep=",")
        }
    }
    invisible(NULL)
}
