#This code is for line 203. To get the results for the other dependent variables, swap '203' with 306/414/490/949.

rm(list = ls())
setwd()

library(ggplot2)
library(vip)

#Load models
lmMod203 <- readRDS(".lmMod_203.rds")
marsMod203 <- readRDS(".marsMod_203.rds")
knnMod203 <- readRDS(".knnMod_203.rds")
rfMod203 <- readRDS(".rfMod_203.rds")
svrMod203 <- readRDS(".svrMod_203.rds")

#Load data
setwd()
data203<-read.csv("Picked lines and predictors final.csv")

#Set features to the right format
features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.536",	"Line.949",	"Line.203.lag",	"Line.205",	"Line.206",	"Line.207",	"Line.208",	"Line.211",	"Line.212",	"Line.225",	"Line.226",	"Line.231",	"Line.232",	"Line.275",	"Line.291",	"Line.293",	"Line.301",	"Line.302",	"Line.302.1",	"Line.303",	"Line.305",	"Line.306.lag",	"Line.314",	"Line.315",	"Line.316",	"Line.316.1",	"Line.316.2",	"Line.318",	"Line.319",	"Line.324",	"Line.328",	"Line.357",	"Line.358",	"Line.367",	"Line.368",	"Line.413",	"Line.414.lag",	"Line.415",	"Line.415.1",	"Line.415.2",	"Line.416",	"Line.419",	"Line.420",	"Line.423",	"Line.424",	"Line.438",	"Line.445",	"Line.446",	"Line.454",	"Line.461",	"Line.462",	"Line.463",	"Line.464",	"Line.471",	"Line.472",	"Line.475",	"Line.489",	"Line.490.lag",	"Line.493",	"Line.494",	"Line.495",	"Line.496",	"Line.498",	"Line.504",	"Line.509",	"Line.512",	"Line.513",	"Line.514",	"Line.516",	"Line.517",	"Line.518",	"Line.520",	"Line.522",	"Line.526",	"Line.531",	"Line.532",	"Line.539",	"Line.542",	"Line.543",	"Line.544",	"Line.546",	"Line.547",	"Line.547.1",	"Line.547.2",	"Line.548",	"Line.548.1",	"Line.548.2",	"Line.551",	"Line.552",	"Line.553",	"Line.554",	"Line.555",	"Line.556",	"Line.557",	"Line.558",	"Line.559",	"Line.560",	"Line.562",	"Line.565",	"Line.566",	"Line.567",	"Line.568",	"Line.571",	"Line.572",	"Line.573",	"Line.574",	"Line.575",	"Line.577",	"Line.577.1",	"Line.577.2",	"Line.578",	"Line.578.1",	"Line.578.2",	"Line.585",	"Line.586",	"Line.587",	"Line.588",	"Line.589",	"Line.590",	"Line.591",	"Line.592",	"Line.594",	"Line.903",	"Line.904",	"Line.907",	"Line.908",	"Line.919",	"Line.920",	"Line.921",	"Line.922",	"Line.949.lag",	"Line.961",	"Line.962",	"Line.971",	"Line.972",	"Line.981",	"Line.982",	"Line.991",	"Line.992",	"Line.993",	"Line.994",	"Line.HGUE1")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")

data203[features] <- sapply(data203[features],as.numeric)
data203[features2] <- sapply(data203[features2], as.character)
data203[features2] <- sapply(data203[features2], as.numeric)


data203$Line.306 <- NULL
data203$Line.414 <- NULL
data203$Line.490 <- NULL
data203$Line.536 <- NULL
data203$Line.949 <- NULL

data203$Line.306.lag <- NULL
data203$Line.949.lag <- NULL


#Remove missing values
sum(is.na(data203))
data203 <- na.omit(data203)

data203$Time <- as.POSIXct(data203$Time)

#Create training and test sets
set.seed(123)  
trainingRowIndex <- sample(1:nrow(data203), 0.7*nrow(data203))  
trainingData <- data203[trainingRowIndex, ]  
testData  <- data203[-trainingRowIndex, ]   

#Make predictions
Pred <- predict(svrMod203, testData)

#Make actuals_predicteds dataframe
actuals_preds <- data.frame(cbind(actuals=testData$Line.203, predicteds=Pred))  


## GGplots
## Scatter plot w/ ab-line to visualise results
dev.off()
ggplot(data=actuals_preds,aes(x=actuals,y=predicteds)) + 
  geom_point() + 
  geom_abline(linetype=2,color="darkgrey") + 
  scale_x_continuous(limits=c(0,250)) + 
  scale_y_continuous(limits=c(0,250)) +
  ggtitle("Prediction accuracy SVR model for Line 203") +
  xlab("Actual values (MW)") + ylab("Predicted values (MW)")


#Variable importance plots
p1 <- vip(marsMod203, num_features = 10)
p2 <- vip(lmMod203, num_features = 10)
p3 <- vip(rfMod203, num_features = 10)
p4 <- vip(knnMod203, num_features = 10)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Variable importance line 203")

p1 <- vip(marsMod203, num_features = 20)
p2 <- vip(lmMod203, num_features = 20)
p3 <- vip(rfMod203, num_features = 20)
p4 <- vip(knnMod203, num_features = 20)
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Variable importance line 203")