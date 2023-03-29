# load packages and data
library(randomForest)
library(haven)
MLSData <- read_sav("MLSData_Means.sav")

# split training and test data
data_set_size = floor(nrow(MLSData)*0.75)
index <- sample(1:nrow(MLSData), size = data_set_size)
train <- MLSData[index,]
test <- MLSData[-index,]

# train random forest
set.seed(150)
energy.rf <- randomForest(energy ~ SIB_Mean + ER_Mean + FAC_Mean + CAMP_Mean + PA_Mean, data = train, mtry = 5, importance = TRUE)
print(energy.rf)

# importance of predictors
round(importance(energy.rf), 3)
plot(randomForest(energy ~ SIB_Mean + ER_Mean + FAC_Mean + CAMP_Mean + PA_Mean, data = work_study.df, keep.forest = TRUE), log = "y")
