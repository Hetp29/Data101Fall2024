getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_1")
train <- read.csv("MoodyTrain2024.csv")
test <- read.csv("MoodyTestStudents24.csv")
submission <- read.csv("submission2024.csv")

#structure of data
str(train)
summary(train)

#boxplots to visualize distribution of Score across Grade and identify overlap areas
boxplot(Score ~ Grade, data = train, main = "Score Distribution by Grade", xlab = "Grade", ylab = "Score")


table(train$Major, train$Grade)
table(train[train$Score > 80, ]$Grade)

#score-based decision model
decision <- rep('F', nrow(train))
decision[train$Score > 50] <- 'D'
decision[train$Score > 60] <- 'C'
decision[train$Score > 75] <- 'B'
decision[train$Score > 90] <- 'A'

#refine decision rules
subset_data <- subset(train, Score > 75 & Score <= 90)
table(subset_data$Grade, subset_data$Major)

accuracy <- mean(decision == train$Grade)
print(paste("Accuracy of basic model:", accuracy))

test_decision <- rep('F', nrow(test))
test_decision[test$Score > 50] <- 'D'
test_decision[test$Score > 60] <- 'C'
test_decision[test$Score > 75] <- 'B'
test_decision[test$Score > 90] <- 'A'

#Adjust using refined rules
test_decision[test$Score > 75 & test$Score <= 90 & test$Major == 'Math'] <- 'B'
test_decision[test$Score > 75 & test$Score <= 90 & test$Major == 'CS' & test$Attendance > 0.9] <- 'A'

submission$Grade <- test_decision
write.csv(submission, "submission2024.csv", row.names = FALSE)

#find classes where predictions do not match grades
misclassified <- train[decision != train$Grade, ]
table(misclassified$Score, misclassified$Grade)

#grades below 60
table(train[train$Score < 60, ]$Attendance, train[train$Score < 60, ]$Grade)
table(train[train$Score < 60, ]$Questions, train[train$Score < 60, ]$Grade)

subset_high <- subset(train, Score > 85)
table(subset_high$Grade, subset_high$Major)

misclassified <- train[decision != train$Grade, ]
table(misclassified$Score, misclassified$Grade)
table(misclassified$Attendance, misclassified$Grade)
table(misclassified$Questions, misclassified$Grade)
table(misclassified$Major, misclassified$Grade)

subset_low <- subset(train, Score <= 60 & decision != train$Grade)
subset_mid <- subset(train, Score > 60 & Score <= 90 & decision != train$Grade)
subset_high <- subset(train, Score > 90 & decision != train$Grade)

summary(subset_low)
summary(subset_mid)
summary(subset_high)

table(misclassified$Major, misclassified$Grade)

summary(misclassified$Attendance)
summary(misclassified$Questions)



# Initialize all as F (default class)
decision <- rep('F', nrow(train))

# Attendance <= 0.33
decision[train$Attendance <= 0.33 & train$Score <= 50.50 & train$Score <= 47.00] <- 'F'
decision[train$Attendance <= 0.33 & train$Score <= 50.50 & train$Score > 47.00 & train$Row <= 8.50] <- 'F'
decision[train$Attendance <= 0.33 & train$Score <= 50.50 & train$Score > 47.00 & train$Row > 8.50] <- 'C'

# Attendance <= 0.33 and Score > 50.50
decision[train$Attendance <= 0.33 & train$Score > 50.50 & train$Attendance <= 0.09 & train$Row <= 3.00] <- 'A'
decision[train$Attendance <= 0.33 & train$Score > 50.50 & train$Attendance <= 0.09 & train$Row > 3.00 & train$Score <= 76.00 & train$Score <= 60.00 & train$Row <= 10.50] <- 'D'
decision[train$Attendance <= 0.33 & train$Score > 50.50 & train$Attendance <= 0.09 & train$Row > 3.00 & train$Score <= 76.00 & train$Score <= 60.00 & train$Row > 10.50] <- 'C'
decision[train$Attendance <= 0.33 & train$Score > 50.50 & train$Attendance <= 0.09 & train$Row > 3.00 & train$Score <= 76.00 & train$Score > 60.00] <- 'C'
decision[train$Attendance <= 0.33 & train$Score > 50.50 & train$Attendance <= 0.09 & train$Row > 3.00 & train$Score > 76.00 & train$Score <= 85.00] <- 'B'
decision[train$Attendance <= 0.33 & train$Score > 50.50 & train$Attendance > 0.09 & train$Row > 8.50] <- 'C'


# Add more rules based on the Python tree outputs
accuracy <- mean(decision == train$Grade)
print(paste("Accuracy:", accuracy))













boxplot(Score ~ Grade, data = train, main = "Score Distribution by Grade")

# Example: Check overlaps in Score and use subset + table
overlap <- subset(train, Score > 60 & Score <= 75)
table(overlap$Grade, overlap$Attendance)
table(overlap$Grade, overlap$Questions)

# Function to evaluate model accuracy on training data
evaluate_model <- function(data, score_cutpoints, attendance_cutpoints, question_cutpoints) {
  decision <- rep('F', nrow(data))
  
  # Base decision using Score thresholds
  decision[data$Score > score_cutpoints[1]] <- 'D'
  decision[data$Score > score_cutpoints[2]] <- 'C'
  decision[data$Score > score_cutpoints[3]] <- 'B'
  decision[data$Score > score_cutpoints[4]] <- 'A'
  
  # Tie-breaker rules based on Attendance and Questions
  decision[data$Score > 60 & data$Score <= 75 & data$Attendance > attendance_cutpoints[1]] <- 'C'
  decision[data$Score > 75 & data$Score <= 90 & data$Questions > question_cutpoints[1]] <- 'B'
  
  # Calculate accuracy
  accuracy <- mean(decision == data$Grade)
  return(list(accuracy = accuracy, decision = decision))
}

# Iterate over ranges to find the best cutpoints
best_accuracy <- 0
best_params <- list()
best_decision <- NULL

score_ranges <- list(c(50, 60, 75, 90))  # Example: Modify as needed
attendance_ranges <- seq(5, 15, 1)  # Adjust range as needed
question_ranges <- seq(1, 10, 1)  # Adjust range as needed

for (score in score_ranges) {
  for (attendance in attendance_ranges) {
    for (questions in question_ranges) {
      result <- evaluate_model(train, score, c(attendance), c(questions))
      if (result$accuracy > best_accuracy) {
        best_accuracy <- result$accuracy
        best_params <- list(score, attendance, questions)
        best_decision <- result$decision
      }
    }
  }
}

# Display best parameters and accuracy
print(best_accuracy)
print(best_params)

# Apply the best model to test data
decision <- rep('F', nrow(test))
decision[test$Score > best_params[[1]][1]] <- 'D'
decision[test$Score > best_params[[1]][2]] <- 'C'
decision[test$Score > best_params[[1]][3]] <- 'B'
decision[test$Score > best_params[[1]][4]] <- 'A'

# Tie-breaker rules for test data
decision[test$Score > 60 & test$Score <= 75 & test$Attendance > best_params[[2]]] <- 'C'
decision[test$Score > 75 & test$Score <= 90 & test$Questions > best_params[[3]]] <- 'B'

# Populate submission file with predictions
submission$Grade <- decision

# Write the submission file
write.csv(submission, "submission.csv", row.names = FALSE)


