corr <- function(directory, threshold = 0){
        fileList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        values <- numeric()
        
        for(i in 1:332){
                data <- read.csv(fileList[i])
                nobs <- sum(complete.cases(data))
                if(nobs > threshold){
                        values <- c(values, cor(data[, "nitrate"], data[, "sulfate"], use = "complete.obs"))
                }
        }
        values
}