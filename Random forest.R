rm(list = ls())
setwd()

#Load libraries
library(foreign)
library(lubridate)
library(caret)
library(ranger)
library(Metrics)

#Load data
data<-read.csv("Picked lines and predictors final.csv")

#Set to numeric
features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.536",	"Line.949",	"Line.203.lag",	"Line.205",	"Line.206",	"Line.207",	"Line.208",	"Line.211",	"Line.212",	"Line.225",	"Line.226",	"Line.231",	"Line.232",	"Line.275",	"Line.291",	"Line.293",	"Line.301",	"Line.302",	"Line.302.1",	"Line.303",	"Line.305",	"Line.306.lag",	"Line.314",	"Line.315",	"Line.316",	"Line.316.1",	"Line.316.2",	"Line.318",	"Line.319",	"Line.324",	"Line.328",	"Line.357",	"Line.358",	"Line.367",	"Line.368",	"Line.413",	"Line.414.lag",	"Line.415",	"Line.415.1",	"Line.415.2",	"Line.416",	"Line.419",	"Line.420",	"Line.423",	"Line.424",	"Line.438",	"Line.445",	"Line.446",	"Line.454",	"Line.461",	"Line.462",	"Line.463",	"Line.464",	"Line.471",	"Line.472",	"Line.475",	"Line.489",	"Line.490.lag",	"Line.493",	"Line.494",	"Line.495",	"Line.496",	"Line.498",	"Line.504",	"Line.509",	"Line.512",	"Line.513",	"Line.514",	"Line.516",	"Line.517",	"Line.518",	"Line.520",	"Line.522",	"Line.526",	"Line.531",	"Line.532",	"Line.539",	"Line.542",	"Line.543",	"Line.544",	"Line.546",	"Line.547",	"Line.547.1",	"Line.547.2",	"Line.548",	"Line.548.1",	"Line.548.2",	"Line.551",	"Line.552",	"Line.553",	"Line.554",	"Line.555",	"Line.556",	"Line.557",	"Line.558",	"Line.559",	"Line.560",	"Line.562",	"Line.565",	"Line.566",	"Line.567",	"Line.568",	"Line.571",	"Line.572",	"Line.573",	"Line.574",	"Line.575",	"Line.577",	"Line.577.1",	"Line.577.2",	"Line.578",	"Line.578.1",	"Line.578.2",	"Line.585",	"Line.586",	"Line.587",	"Line.588",	"Line.589",	"Line.590",	"Line.591",	"Line.592",	"Line.594",	"Line.903",	"Line.904",	"Line.907",	"Line.908",	"Line.919",	"Line.920",	"Line.921",	"Line.922",	"Line.949.lag",	"Line.961",	"Line.962",	"Line.971",	"Line.972",	"Line.981",	"Line.982",	"Line.991",	"Line.992",	"Line.993",	"Line.994",	"Line.HGUE1")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")
data[features] <- sapply(data[features],as.numeric)
data[features2] <- sapply(data[features2], as.character)
data[features2] <- sapply(data[features2], as.numeric)


#Line 203
data203<-read.csv("Picked lines and predictors final.csv")
data203[features] <- sapply(data203[features],as.numeric)
data203[features2] <- sapply(data203[features2], as.character)
data203[features2] <- sapply(data203[features2], as.numeric)

#Delete other dependent variables
data203$Line.306 <- NULL
data203$Line.414 <- NULL
data203$Line.490 <- NULL
data203$Line.536 <- NULL
data203$Line.949 <- NULL

#Delete variables with too many missing values
data203$Line.306.lag <- NULL
data203$Line.949.lag <- NULL

#Remove missing values
sum(is.na(data203))
data203 <- na.omit(data203)

#Set Time variable to POSIX
data203$Time <- as.POSIXct(data203$Time)

#Create training and test sets
set.seed(123)
trainingRowIndex <- sample(1:nrow(data203), 0.7*nrow(data203))  
trainingData <- data203[trainingRowIndex, ]
testData  <- data203[-trainingRowIndex, ] 

#Build the random forest model
set.seed(123)
rfMod <- train(Line.203 ~ .,
               data = trainingData,
               method = "ranger",
               importance = 'impurity'
)

#Save model
saveRDS(rfMod,".rfMod_203.rds")

#Make predictions
Pred <- predict(rfMod, testData)

#Evaluate the predictions
actuals_preds <- data.frame(cbind(actuals=testData$Line.203, predicteds=Pred))
head(actuals_preds)
rmse(Pred, testData$Line.203) #20.97224



###Line 306
#Load data and set to numeric
data306<-read.csv("Picked lines and predictors final.csv")
data306[features] <- sapply(data306[features],as.numeric)
data306[features2] <- sapply(data306[features2], as.character)
data306[features2] <- sapply(data306[features2], as.numeric)

#Delete other dependent variables
data306$Line.203 <- NULL
data306$Line.414 <- NULL
data306$Line.490 <- NULL
data306$Line.536 <- NULL
data306$Line.949 <- NULL

#Delete variables with too many missing values
data306$Line.314 <- NULL #Line 314 was removed from the 306 data set because it has too many missing values and would have made the data set for 306 even smaller than it already is.
data306$Line.949.lag <- NULL

#Remove missing values
sum(is.na(data306))
data306 <- na.omit(data306)

#Set the Time variable to POSIX
data306$Time <- as.POSIXct(data306$Time)

#Create training and test sets
set.seed(123) 
trainingRowIndex <- sample(1:nrow(data306), 0.7*nrow(data306))
trainingData <- data306[trainingRowIndex, ] 
testData  <- data306[-trainingRowIndex, ] 

#Build the random forest model
set.seed(123)
rfMod <- train(Line.306 ~ .,
                  data = trainingData,
                  method = "ranger",
                  importance = 'impurity'
)

#Save model
saveRDS(rfMod,".rfMod_306.rds")

#Make predictions
Pred <- predict(rfMod, testData)

#Evaluate the predictions
actuals_preds <- data.frame(cbind(actuals=testData$Line.306, predicteds=Pred))
head(actuals_preds)
rmse(Pred, testData$Line.306)  #42.55312



###Line 949
#Load data and set to numeric
data949<-read.csv("Picked lines and predictors final.csv")
data949[features] <- sapply(data949[features],as.numeric)
data949[features2] <- sapply(data949[features2], as.character)
data949[features2] <- sapply(data949[features2], as.numeric)

#Remove other dependent variables
data949$Line.306 <- NULL
data949$Line.414 <- NULL
data949$Line.490 <- NULL
data949$Line.536 <- NULL
data949$Line.203 <- NULL

#Remove variables with too many missing values
data949$Line.306.lag <- NULL

#Remove missing values
sum(is.na(data949))
data949 <- na.omit(data949)

data949$Time <- as.POSIXct(data949$Time)

#Create training and test sets
set.seed(123)  
trainingRowIndex <- sample(1:nrow(data949), 0.7*nrow(data949))  
trainingData <- data949[trainingRowIndex, ]  
testData  <- data949[-trainingRowIndex, ]  

#Build the random forest model
set.seed(123)
rfMod <- train(
  form = Line.949 ~ .,
  data = trainingData,
  method = "ranger",
  importance = 'impurity'
)

#Save model
saveRDS(rfMod,".rfMod_949.rds")

#Make predictions
Pred <- predict(rfMod, testData)

#Evaluate the predictions
actuals_preds <- data.frame(cbind(actuals=testData$Line.949, predicteds=Pred))  
head(actuals_preds)
rmse(Pred, testData$Line.949) #144.0198



###Line 414
#Load data and set to numeric
data414<-read.csv("Picked lines and predictors final.csv")
data414[features] <- sapply(data414[features],as.numeric)
data414[features2] <- sapply(data414[features2], as.character)
data414[features2] <- sapply(data414[features2], as.numeric)

#Delete other dependent variables
data414$Line.306 <- NULL
data414$Line.949 <- NULL
data414$Line.490 <- NULL
data414$Line.536 <- NULL
data414$Line.203 <- NULL

#Delete variables with too many missing values
data414$Line.306.lag <- NULL
data414$Line.949.lag <- NULL

#Remove missing values
sum(is.na(data414))
data414 <- na.omit(data414)

#Set Time variable to POSIX
data414$Time <- as.POSIXct(data414$Time)

#Create training and test sets
set.seed(123)
trainingRowIndex <- sample(1:nrow(data414), 0.7*nrow(data414)) 
trainingData <- data414[trainingRowIndex, ] 
testData  <- data414[-trainingRowIndex, ]  

#Build the random forest model
set.seed(123)
rfMod <- train(
  form = Line.414 ~ .,
  data = trainingData,
  method = "ranger",
  importance = 'impurity'
)

#Save model
saveRDS(rfMod,".rfMod_414.rds")

#Make predictions
Pred <- predict(rfMod, testData)

#Evaluate predictions
actuals_preds <- data.frame(cbind(actuals=testData$Line.414, predicteds=Pred))
head(actuals_preds)
rmse(Pred, testData$Line.414) #145.9523


###Line 490
#Load data and set to numeric
data490<-read.csv("Picked lines and predictors final.csv")
data490[features] <- sapply(data490[features],as.numeric)
data490[features2] <- sapply(data490[features2], as.character)
data490[features2] <- sapply(data490[features2], as.numeric)

#Delete other dependent variables
data490$Line.306 <- NULL
data490$Line.949 <- NULL
data490$Line.414 <- NULL
data490$Line.536 <- NULL
data490$Line.203 <- NULL

#Delete variables with too many missing values
data490$Line.306.lag <- NULL
data490$Line.949.lag <- NULL

#Remove missing values
sum(is.na(data490))
data490 <- na.omit(data490)

#Set Time variable to POSIX
data490$Time <- as.POSIXct(data490$Time)

#Create training and test sets
set.seed(123) 
trainingRowIndex <- sample(1:nrow(data490), 0.7*nrow(data490))  
trainingData <- data490[trainingRowIndex, ]  
testData  <- data490[-trainingRowIndex, ]  

# Build the random forest model
set.seed(123)
rfMod <- train(
  form = Line.490 ~ .,
  data = trainingData,
  method = "ranger",
  importance = 'impurity'
)

#Save model
saveRDS(rfMod,".rfMod_490.rds")

#Make predictions
Pred <- predict(rfMod, testData)

#Evaluate predictions
actuals_preds <- data.frame(cbind(actuals=testData$Line.490, predicteds=Pred))
head(actuals_preds)
rmse(Pred, testData$Line.490) #86.22885
