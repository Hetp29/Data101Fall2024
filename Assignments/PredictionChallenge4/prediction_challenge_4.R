# Load necessary libraries
library(dplyr)
library(caret)

# Set working directory
setwd("/Users/hetpatel/PredictionChallenge4")

# Load datasets
train <- read.csv("UniPredTrain_Students.csv")
test <- read.csv("UniPredTest_Students.csv")
universityData <- read.csv("UniversityData.csv")
submission <- read.csv("submission4.csv")

# Merge train and universityData on 'Uni'
train <- merge(train, universityData, by = "Uni", all.x = TRUE)
test <- merge(test, universityData, by = "Uni", all.x = TRUE)

# Convert categorical variables to factors
train$Private <- as.factor(train$Private)
test$Private <- as.factor(test$Private)

# Check for missing values and impute if necessary
train[is.na(train)] <- lapply(train, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))
test[is.na(test)] <- lapply(test, function(x) ifelse(is.numeric(x), mean(x, na.rm = TRUE), x))

# Normalize continuous variables (if needed)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
train$GPA <- normalize(train$GPA)
train$Tuition <- normalize(train$Tuition)
test$GPA <- normalize(test$GPA)
test$Tuition <- normalize(test$Tuition)

# Train-test split for validation
set.seed(123)
trainIndex <- createDataPartition(train$Salary, p = 0.8, list = FALSE)
trainSet <- train[trainIndex, ]
valSet <- train[-trainIndex, ]

# Train a linear regression model with cross-validation
set.seed(123)
lm_model <- train(Salary ~ GPA + Grad + Tuition + Private, 
                  data = trainSet, 
                  method = "lm", 
                  trControl = trainControl(method = "cv", number = 10))

# Evaluate lm on validation set
predictions_val <- predict(lm_model, newdata = valSet)
MAE_val <- mean(abs(predictions_val - valSet$Salary))
RMSE_val <- sqrt(mean((predictions_val - valSet$Salary)^2))
R2_val <- 1 - (sum((valSet$Salary - predictions_val)^2) / 
                 sum((valSet$Salary - mean(valSet$Salary))^2))

print(paste("Validation MAE:", MAE_val))
print(paste("Validation RMSE:", RMSE_val))
print(paste("Validation R²:", R2_val))

# Predict on the test dataset
predictions_test <- predict(lm_model, newdata = test)

# Round predictions to 4 decimal places
submission$Salary <- round(predictions_test, 4)

# Save the submission file
write.csv(submission, "submission4.csv", row.names = FALSE)