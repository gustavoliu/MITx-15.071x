setwd("~/Desktop/Programming/MITx 15.071x/Week2")
Sys.setlocale("LC_ALL", "C")
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# remove NA's
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# Set reference level of factor "Race/Ethnicity" to "White" (the most common one)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# build a linear regression model using the training set to predict readingScore
  # using all the remaining variables.
lmScore = lm(pisaTrain$readingScore ~ ., data = pisaTrain)

# Root-mean squared error (RMSE) of the model
sqrt(mean(residuals(lmScore)^2))

predTest = predict(lmScore, newdata = pisaTest)
baseline = mean(pisaTrain$readingScore)
SST = sum((baseline - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST
