# load packages and data
library(rpart)
library(rpart.plot)
library(haven)
MLSData <- read_sav("MLSData_Means.sav")

# split training (75%) and test (25%) data
data_set_size = floor(nrow(MLSData)*0.75)
index <- sample(1:nrow(MLSData), size = data_set_size)
train <- MLSData[index,]
test <- MLSData[-index,]

# Mean Absolute Error function
MAE <- function (actual, predicted) {
  mean(abs(actual - predicted))
}

# train model
m <- rpart(danceability ~ SIB_Mean + ER_Mean + FAC_Mean + CAMP_Mean + PA_Mean, data = train, method = "anova")
rpart.plot(m1, type = 5, digits = 3, fallen.leaves = TRUE)

# test model
p <- predict(m, test)
round(MAE(MLSData$danceability, p), 3)
