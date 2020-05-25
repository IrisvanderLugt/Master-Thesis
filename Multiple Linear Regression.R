rm(list = ls())
setwd()

#Load libraries
library(foreign)
library(lubridate)
library(stargazer)

#203
data<-read.csv("Picked lines and predictors SMARD final.csv")

features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.949")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")

data[features] <- sapply(data[features],as.numeric)
data[features2] <- sapply(data[features2], as.character)
data[features2] <- sapply(data[features2], as.numeric)

data$Time <- NULL

data$Line.306 <- NULL
data$Line.414 <- NULL
data$Line.490 <- NULL
data$Line.536 <- NULL
data$Line.949 <- NULL

sum(is.na(data))
data <- na.omit(data)

LMod203 <- lm(Line.203 ~., data=data)
summary(LMod203)


#306
data<-read.csv("Picked lines and predictors SMARD final.csv")

features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.949")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")

data[features] <- sapply(data[features],as.numeric)
data[features2] <- sapply(data[features2], as.character)
data[features2] <- sapply(data[features2], as.numeric)
data$Time <- NULL

data$Line.203 <- NULL
data$Line.414 <- NULL
data$Line.490 <- NULL
data$Line.536 <- NULL
data$Line.949 <- NULL

sum(is.na(data))
data <- na.omit(data)

LMod306 <- lm(Line.306 ~., data=data)
summary(LMod306)

#414
data<-read.csv("Picked lines and predictors SMARD final.csv")

features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.949")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")

data[features] <- sapply(data[features],as.numeric)
data[features2] <- sapply(data[features2], as.character)
data[features2] <- sapply(data[features2], as.numeric)
data$Time <- NULL

data$Line.203 <- NULL
data$Line.306 <- NULL
data$Line.490 <- NULL
data$Line.536 <- NULL
data$Line.949 <- NULL

sum(is.na(data))
data <- na.omit(data)

LMod414 <- lm(Line.414 ~., data=data)
summary(LMod414)



#490
data<-read.csv("Picked lines and predictors SMARD final.csv")

features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.949")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")

data[features] <- sapply(data[features],as.numeric)
data[features2] <- sapply(data[features2], as.character)
data[features2] <- sapply(data[features2], as.numeric)
data$Time <- NULL

data$Line.203 <- NULL
data$Line.414 <- NULL
data$Line.306 <- NULL
data$Line.536 <- NULL
data$Line.949 <- NULL

sum(is.na(data))
data <- na.omit(data)

LMod490 <- lm(Line.490 ~., data=data)
summary(LMod490)


#949
data<-read.csv("Picked lines and predictors SMARD final.csv")

features <- c("Line.203",	"Line.306",	"Line.414",	"Line.490",	"Line.949")
features2 <- c("Price_day_ahead",	"Generation_forecast",	"Wind_offshore_forecast",	"Wind_onshore_forecast",	"Solar_forecast",	"Other_gen_forecast",	"Consumption_forecast",	"Net.exports",	"DK_Net_Exports", "PL_Net_Exports", "CZ_Net_Exports")

data[features] <- sapply(data[features],as.numeric)
data[features2] <- sapply(data[features2], as.character)
data[features2] <- sapply(data[features2], as.numeric)
data$Time <- NULL

data$Line.203 <- NULL
data$Line.414 <- NULL
data$Line.490 <- NULL
data$Line.536 <- NULL
data$Line.306 <- NULL

sum(is.na(data))
data <- na.omit(data)

LMod949 <- lm(Line.949 ~., data=data)
summary(LMod949)


stargazer(LMod203,LMod306,LMod414,LMod490,LMod949,
          type='html',title="Multiple Linear Regression", out="Multiple Linear Regression")

