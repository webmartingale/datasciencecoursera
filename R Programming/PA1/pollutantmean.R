pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!

    #init
    s <- 0
    n <- 0

    #process
    for (iMon in id) {
        #read file
        filecode <- formatC(iMon, width = 3, flag = "0")
        path <- paste(directory, "/", filecode, ".csv", sep = "")
        monData <- read.csv(path)

        #process file
        data <- monData[[pollutant]]
        s <- s + sum(data, na.rm = T)
        n <- n + sum(!is.na(data))
    }

    #return
    s/n
}
