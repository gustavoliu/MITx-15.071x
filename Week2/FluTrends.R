# Setup:
setwd("~/Desktop/Programming/MITx 15.071x/Week2")
Sys.setlocale("LC_ALL", "C")

FluTrain = read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")

# Model Visits to Health Service because of Influenza-Like-Illness, according to Training data:
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)

# Prediction using model FluTrend1.
# 'exp()' undoes what 'log()' does. 
PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

# Root Mean Squared Error:
# Sum of squared errors: sum of(predicted - found)^2)
SSE = sum((PredTest1 - FluTest$ILI)^2)
# RMSE = sqrt( SSE/nrow(FluTest) )

# OR:
RMSE = sqrt(mean((PredTest1 - FluTest$ILI)^2))

# Download and instal package 'zoo':

install.packages("zoo")
library(zoo)

# Add lag of 2 weeks of ILI as variable to the Training Set: 
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

# Model of linear regression with ILILag2 as a coefficient:
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)

# Add lag of 2 weeks of ILI as variable to the Test Set: 
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# Fill in missing values of ILILag2 in FluTest (first two):
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

# Predict values of ILI in Test Data using model FluTrend2:
PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

# RMSE for PredTest2:
RMSE2 = sqrt(mean((PredTest2 - FluTest$ILI)^2))