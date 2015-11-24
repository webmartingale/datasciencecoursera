#Gets the best hospital at within a selected state for the given outcome
best <- function(state, outcome){

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

    ## Return hospital name in that state with lowest 30-day death
    ## rate

    #filter for state
    stateData <- data[data$State == state,]
    stateData[[outcomeIndex]] <- as.numeric(stateData[[outcomeIndex]])

    #get the best rate
    bestNr <- min(stateData[[outcomeIndex]], na.rm = T)

    #output name with best rate
    bestNames <- stateData$Hospital.Name[!is.na(stateData[[outcomeIndex]]) & stateData[[outcomeIndex]] == bestNr]
    bestNames <- sort(bestNames)
    bestNames[1]
}
