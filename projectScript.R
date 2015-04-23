library(caret)
library(ggplot2)

# For parallel processing, include the following packages and set the number of cores.
# WARNING: Do not run registerDoMC(cores=numCores) within RStudio. Run it as a script.
# NOTE: This is unix only.
#library(foreach)
#library(doMC)
#registerDoMC(cores=4)

train <- read.csv("pml-training.csv", na.strings = "NA", strip.white = TRUE)
test <- read.csv("pml-testing.csv", na.strings = "NA", strip.white = TRUE)

n.train <- nrow(train)

# Remove problem_id from test set for now
problemIds <- test$problem_id
test <- test[,-grep("problem_id", colnames(test))]

# Add classe to test as a placeholder
test$classe <- "A"
fullset <- rbind(train, test)

getColumnsWithHighUnknown <- function(dataset, thresh){
  results <- 1:dim(dataset)[2]
  nrows <- dim(dataset)[1]
  for(i in 1:dim(dataset)[2]){
    divZero <- sum(dataset[,i] == "#DIV/0!")
    nas <- sum(is.na(dataset[,i]))
    blanks <- sum(dataset[,i] == "")
    
    if((nas/nrows >= thresh || blanks/nrows >= thresh)){
      results[i] <- i
    }
    else{
      results[i] <- 0
    }
  }
  results <- results[results > 0]
  results
}
removeCols <- getColumnsWithHighUnknown(train, 0.5)
fullset <- fullset[,-removeCols]

summary(fullset)

getNumericColumns <- function(dataset){
  results <- 1:dim(dataset)[2]
  for(i in 1:dim(dataset)[2]){
    if(class(dataset[,i]) == "integer" || class(dataset[,i]) == "numeric"){
      results[i] <- i
    }
    else{
      results[i] <- 0
    }
  }
  results <- results[results > 0]
  results
}
numericCols <- getNumericColumns(fullset)
temp <- nearZeroVar(fullset[1:n.train,numericCols])
temp

set.seed(8947)
model <- train(classe~., 
               data=fullset[1:n.train,-1], 
               preProcess = c("center", "scale", "pca"), 
               method = "rf",
               trControl = trainControl(method = "cv", number = 25, allowParallel=TRUE))
model

prediction <- predict(model, fullset[-c(1:n.train), -1])
prediction