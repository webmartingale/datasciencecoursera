corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    corrs <- c()
    ncorrs <- 0

    fileNames <- dir("specdata/")

    #process
    for (iMon in seq_along(fileNames)) {

        if (threshold < complete(directory, iMon)$nobs)
        {
            #read file
            path <- paste(directory, fileNames[iMon], sep = "/")
            data <- read.csv(path)

            #process file
            data <- data[complete.cases(data),]
            corrs[ncorrs + 1] <- cor(data$sulfate, data$nitrate)
            ncorrs <- ncorrs + 1
        }
    }

    #output
    corrs
}
