library(rpart)
getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_2")
train <- read.csv("couplesTrain.csv")
test <- read.csv("CouplesTestStudents.csv")
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
              control = rpart.control(minsplit = 5, minbucket = 5, cp = 0.005))

prediction <- predict(tree, train, type = "class")
accuracy <- mean(prediction == train$Outcome)
print(paste("Updated Training Accuracy:", accuracy))







cross_validate <- function(data, formula, n_iter = 10, split_ratio = 0.8, minsplit = 5, minbucket = 5, cp = 0.005) {
  accuracies <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    # Split data into training and validation subsets
    train_idx <- sample(seq_len(nrow(data)), size = floor(split_ratio * nrow(data)))
    train_subset <- data[train_idx, ]
    val_subset <- data[-train_idx, ]
    
    # Train the decision tree
    tree <- rpart(formula, 
                  data = train_subset, 
                  method = "class",
                  control = rpart.control(minsplit = minsplit, minbucket = minbucket, cp = cp))
    
    # Predict on validation subset
    val_prediction <- predict(tree, val_subset, type = "class")
    val_accuracy <- mean(val_prediction == val_subset$Outcome)
    
    # Store accuracy
    accuracies[i] <- val_accuracy
  }
  
  # Return the mean accuracy and accuracies from each iteration
  list(mean_accuracy = mean(accuracies), accuracies = accuracies)
}

# Perform cross-validation
formula <- Outcome ~ GroomMB + BrideMB + GroomInc + BrideInc + IncomeDiff + TotalInc
cv_results <- cross_validate(data = train, formula = formula, n_iter = 10, split_ratio = 0.8, minsplit = 5, minbucket = 5, cp = 0.005)

# Print results
print(paste("Cross-Validation Mean Accuracy:", cv_results$mean_accuracy))
print("Accuracies from each iteration:")
print(cv_results$accuracies)

# Train the final tree on the full training data
final_tree <- rpart(formula, 
                    data = train, 
                    method = "class",
                    control = rpart.control(minsplit = 5, minbucket = 5, cp = 0.005))

# Predict on the full training data to check training accuracy
final_prediction <- predict(final_tree, train, type = "class")
final_accuracy <- mean(final_prediction == train$Outcome)
print(paste("Final Training Accuracy:", final_accuracy))

