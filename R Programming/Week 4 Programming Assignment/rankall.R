rankall <- function(outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
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
  states <- unique(data[, "State"])
  states <- states[order(states)]
  stateList <- character(0)
  for(i in states){
    stateData <- data[data[, "State"] == i, ]
    colCheck <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", splitOutcome, sep = "")
    sortedDataValues <- unique(sort(suppressWarnings(as.numeric(stateData[, colCheck])), na.last = TRUE, method = "radix", index.return = TRUE)[[1]])
    sortedDataIndexes <- sort(suppressWarnings(as.numeric(stateData[, colCheck])), na.last = TRUE, method = "radix", index.return = TRUE)[[2]]
    
    ## Order the ties in alphabetical order to create a complete list
    listA <- numeric(0)
    listB <- list(0)
    listIndex <- 1
    for(j in sortedDataValues){
      for(k in sortedDataIndexes){
        if(!is.na(j) && stateData[k, colCheck] == j){
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
    
    num <- if(num == "best"){
          1
        } else if(num == "worst"){
          num <- length(completeList)
        } else {
          num
        }
    stateList <- c(stateList, completeList[num])
  }
  data.frame(stateList, states)
}

simpleCap <- function(str){
  str <- strsplit(str, " ")[[1]]
  paste(toupper(substring(str, 1, 1)), substring(str, 2), sep = "", collapse = " ")
}