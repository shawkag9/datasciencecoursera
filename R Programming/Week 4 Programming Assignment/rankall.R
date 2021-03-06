rankall <- function(outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Go through each state and find the hospital of the given num. Put results into a data frame
  state <- unique(data[, "State"])
  state <- state[order(state)]
  hospital <- character(0)
  for(i in state){
    hospital <- c(hospital, rankhospital(i, outcome, num))
  }
  data.frame(hospital, state)
}

simpleCap <- function(str){
  str <- strsplit(str, " ")[[1]]
  paste(toupper(substring(str, 1, 1)), substring(str, 2), sep = "", collapse = " ")
}

rankhospital <- function(state, outcome, rank = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  stateData <- data[data[, "State"] == state,]
  if(length(stateData[[1]]) == 0){
    stop("not a valid state")
  }
  if(outcome != "heart attack" && outcome != "pneumonia" && outcome != "heart failure"){
    stop("not a valid outcome")
  }
  
  ## Read outcome data
  splitOutcome <- as.character(sapply(strsplit(outcome, " ")[[1]], simpleCap))
  if(length(splitOutcome) > 1){
    a <- paste(splitOutcome[1])
    for(i in length(splitOutcome)-1){
      a <- paste(a, ".", splitOutcome[i+1], sep = "")
    }
    splitOutcome <- a
  }
  
  ## Return hospital name with the lowest 30-day death rate
  
  colCheck <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", splitOutcome, sep = "")
  sortedDataValues <- unique(sort(suppressWarnings(as.numeric(stateData[, colCheck])), na.last = TRUE, method = "radix", index.return = TRUE)[[1]])
  sortedDataIndexes <- sort(suppressWarnings(as.numeric(stateData[, colCheck])), na.last = TRUE, method = "radix", index.return = TRUE)[[2]]
  
  ## Order the ties in alphabetical order to create a complete list
  listA <- numeric(0)
  listB <- list(0)
  listIndex <- 1
  for(j in sortedDataValues){
    for(k in sortedDataIndexes){
      if(!is.na(j) && stateData[k, colCheck] == format(round(j, 2), nsmall = 1)){
        listA <- c(listA, stateData[k, "Hospital.Name"])
      }
    }
    listB[[listIndex]] <- listA
    listIndex <- listIndex + 1
    listA <- numeric(0)
  }
  for(i in 1:length(listB)){
    listB[[i]] <- listB[[i]][order(listB[[i]])]
  }
  completeList <- numeric(0)
  for(i in listB){
    completeList <- c(completeList, i)
  }
  
  if(rank == "best"){
    rank <- 1
  } else if(rank == "worst"){
    rank <- length(completeList)
  } else {
    rank
  }
  
  ## Complete list should have all of the hospital names in order
  ## of the lowest death rate of the outcome, with all ties being
  ## in alphabetical order
  
  ## All that's left is to find the rank
  completeList[rank]
}