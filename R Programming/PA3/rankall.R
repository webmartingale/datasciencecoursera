#Gets the hospital at #rank order for all states for the given outcome
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if (sum(data$State == state) == 0) stop("invalid state")
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

    #filter hospitals with missing values
    data[[outcomeIndex]] <- as.numeric(data[[outcomeIndex]])
    data <- data[!is.na(data[[outcomeIndex]]),]

    ## For each state, find the hospital of the given rank
    splitdata <- split(data, as.factor(data$State), drop = T)
    result <- c()# data.frame(hospital = character(), state = character(), stringsAsFactors = F)
    for (iState in seq_along(splitdata))
    {
        #get current state name
        state <- names(splitdata[iState])

        #filter data for this state
        sdata <- splitdata[[iState]]

        #check rank number
        if(is.character(num))
        {
            if (num == "best")          rank <- 1
            else if (num == "worst")    rank <- nrow(sdata)
            else                        stop("invalid rank")
        }
        if(is.numeric(num))
        {
            if (num >= 1 && num <= nrow(sdata)) rank <- num
            else {
                result <- rbind(result, c(NA, state))
                next()
            }
        }

        #find hospital with given rank and insert to result data frame
        ord <- order(sdata[[outcomeIndex]], sdata$Hospital.Name)
        hosp <- sdata[ord[rank], "Hospital.Name"]
        result <- rbind(result, c(hosp, state))
    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    #names(result) <- c("hospital", "state")
    data.frame(hospital = result[,1], state = result[,2], stringsAsFactors = F)
}
