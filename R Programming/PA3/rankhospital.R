#Gets the hospital at #rank order within a selected state for the given outcome
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if (sum(data$State == state) == 0)
    {
        stop("invalid state")
    }
    #also get column index in data for outcome name
    outcomeIndex <- if (outcome == "heart attack"){
        11
    } else if (outcome == "heart failure"){
        17
    } else if (outcome == "pneumonia"){
        23
    } else{
        stop("invalid outcome")
    }

    #filter for state and not missing value
    stateData <- data[data$State == state,]
    stateData[[outcomeIndex]] <- as.numeric(stateData[[outcomeIndex]])
    stateData <- stateData[!is.na(stateData[[outcomeIndex]]),]

    #check rank number
    if(is.character(num))
    {
        if (num == "best")          rank <- 1
        else if (num == "worst")    rank <- nrow(stateData)
        else                        stop("invalid rank")
    }
    if(is.numeric(num))
    {
        if (num >= 1 && num <= nrow(stateData)) rank <- num
        else return(NA)
    }

    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    #order
    ord <- order(stateData[[outcomeIndex]], stateData$Hospital.Name)

    #output
    stateData[ord[rank], "Hospital.Name"]
}
