best <- function(state, outcome){
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
  sortedDataValues <- unique(sort(suppressWarnings(stateData[, colCheck]), na.last = TRUE, method = "radix", index.return = TRUE)[[1]])
  sortedDataIndexes <- sort(suppressWarnings(as.numeric(stateData[, colCheck])), na.last = TRUE, method = "radix", index.return = TRUE)[[2]]
  
  ## Order the ties in alphabetical order to create a complete list
  listA <- numeric(0)
  listB <- list(0)
  listIndex <- 1
  for(j in sortedDataValues){
    for(k in sortedDataIndexes){
      if(!is.na(j) && stateData[k, colCheck] == as.character(j)){
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
  
  ## Complete list should have all of the hospital names in order
  ## of the lowest death rate of the outcome, with all ties being
  ## in alphabetical order
  
  ## All that's left is to find the rank of the hospital
  completeList[1]
}

simpleCap <- function(str){
  str <- strsplit(str, " ")[[1]]
  paste(toupper(substring(str, 1, 1)), substring(str, 2), sep = "", collapse = " ")
}