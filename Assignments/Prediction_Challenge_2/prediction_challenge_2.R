library(rpart)
devtools::install_github("devanshagr/CrossValidation")
getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_2")
train <- read.csv("couplesTrain.csv")
test <- read.csv("CouplesTestStudents.csv")
submission <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionC.csv')
str(train)
summary(train)
colnames(train)
table(train$Outcome)

boxplot(GroomInc ~ Outcome, data = train, main = "Groom's Income by Outcome")
boxplot(BrideInc ~ Outcome, data = train, main = "Bride's Income by Outcome")
table(train$GroomMB, train$Outcome)






train$IncomeDiff <- train$GroomInc - train$BrideInc
boxplot(IncomeDiff ~ Outcome, data = train, main = "Income Difference by Outcome")


train$TotalInc <- train$GroomInc + train$BrideInc
boxplot(TotalInc ~ Outcome, data = train, main = "Total Income by Outcome")



tree <- rpart(Outcome ~ GroomMB + BrideMB + GroomInc + BrideInc + IncomeDiff + TotalInc,
              data = train, 
              method = "class",
              control = rpart.control(minsplit = 5, minbucket = 5, cp = 0.0002))

prediction <- predict(tree, train, type = "class")
accuracy <- mean(prediction == train$Outcome)
print(paste("Updated Training Accuracy:", accuracy))

test_predictions <- predict(tree, test, type = "class")
test$Outcome <- test_predictions

submission$Outcome <- test$Outcome
write.csv(submission, "submission.csv", row.names = FALSE)


# Cross-validation function
cross_validate <- function(data, formula, n_iter = 10, split_ratio = 0.8, minsplit = 5, minbucket = 5, cp = 0.0002) {
  accuracies <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    train_idx <- sample(seq_len(nrow(data)), size = floor(split_ratio * nrow(data)))
    train_subset <- data[train_idx, ]
    val_subset <- data[-train_idx, ]
    
    tree <- rpart(formula, 
                  data = train_subset, 
                  method = "class",
                  control = rpart.control(minsplit = minsplit, minbucket = minbucket, cp = cp))
    
    val_prediction <- predict(tree, val_subset, type = "class")
    val_accuracy <- mean(val_prediction == val_subset$Outcome)
    accuracies[i] <- val_accuracy
  }
  
  list(mean_accuracy = mean(accuracies), accuracies = accuracies)
}

# Perform cross-validation
formula <- Outcome ~ GroomMB + BrideMB + GroomInc + BrideInc + IncomeDiff + TotalInc
cv_results <- cross_validate(data = train, formula = formula, n_iter = 10, split_ratio = 0.8, minsplit = 5, minbucket = 5, cp = 0.0002)

print(paste("Cross-Validation Mean Accuracy:", cv_results$mean_accuracy))
print("Accuracies from each iteration:")
print(cv_results$accuracies)


# Split train data into train and validation sets
set.seed(123)  # For reproducibility
train_indices <- sample(seq_len(nrow(train)), size = 0.8 * nrow(train))
train_subset <- train[train_indices, ]
val_subset <- train[-train_indices, ]

# Train on the subset of training data
tree <- rpart(Outcome ~ GroomMB + BrideMB + GroomInc + BrideInc + IncomeDiff + TotalInc,
              data = train_subset, 
              method = "class",
              control = rpart.control(minsplit = 5, minbucket = 5, cp = 0.0002))

# Predict on validation set
val_predictions <- predict(tree, val_subset, type = "class")
val_accuracy <- mean(val_predictions == val_subset$Outcome)

print(paste("Validation Accuracy:", val_accuracy))

# Check the distribution of predictions
table(test_predictions)

# Compare with training outcome distribution
table(train$Outcome)

pruned_tree <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(pruned_tree)

# Predict on validation set again with pruned tree
val_predictions_pruned <- predict(pruned_tree, val_subset, type = "class")
val_accuracy_pruned <- mean(val_predictions_pruned == val_subset$Outcome)

print(paste("Validation Accuracy (Pruned Tree):", val_accuracy_pruned))
