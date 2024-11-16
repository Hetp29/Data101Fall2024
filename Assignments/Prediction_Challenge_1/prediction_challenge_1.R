getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_1")
train <- read.csv("MoodyTrain2024.csv")
test <- read.csv("MoodyTestStudents24.csv")
submission <- read.csv("submission2024.csv")

#structure of data
str(train)
summary(train)
colnames(train)

#boxplots to visualize distribution of Score across Grade and identify overlap areas
boxplot(Score ~ Grade, data = train, main = "Score Distribution by Grade", xlab = "Grade", ylab = "Score")


table(train$Major, train$Grade)
table(train[train$Score > 80, ]$Grade)

#score-based decision model
#decision <- rep('F', nrow(train))
#decision[train$Score > 50] <- 'D'
#decision[train$Score > 60] <- 'C'
#decision[train$Score > 75] <- 'B'
#decision[train$Score > 90] <- 'A'

#refine decision rules
subset_data <- subset(train, Score > 75 & Score <= 90)
table(subset_data$Grade, subset_data$Major)

#accuracy <- mean(decision == train$Grade)
#print(paste("Accuracy of basic model:", accuracy))

test_decision <- rep('F', nrow(test))
test_decision[test$Score > 50] <- 'D'
test_decision[test$Score > 60] <- 'C'
test_decision[test$Score > 75] <- 'B'
test_decision[test$Score > 90] <- 'A'

#Adjust using refined rules
#test_decision[test$Score > 75 & test$Score <= 90 & test$Major == 'Math'] <- 'B'
#test_decision[test$Score > 75 & test$Score <= 90 & test$Major == 'CS' & test$Attendance > 0.9] <- 'A'

#submission$Grade <- test_decision
#write.csv(submission, "submission2024.csv", row.names = FALSE)

#find classes where predictions do not match grades
#misclassified <- train[decision != train$Grade, ]
#table(misclassified$Score, misclassified$Grade)

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


# Initialize all predictions to 'F'
decision <- rep('F', nrow(train))

# Score <= 75.50
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance <= 0.66 & train$Questions <= 5.50] <- 'F'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance <= 0.66 & train$Questions > 5.50 & train$Attendance <= 0.33] <- 'F'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance <= 0.66 & train$Questions > 5.50 & train$Attendance > 0.33 & train$Score <= 25.50] <- 'F'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance <= 0.66 & train$Questions > 5.50 & train$Attendance > 0.33 & train$Score > 25.50] <- 'C'

decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions <= 4.50 & train$Score <= 29.50] <- 'F'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions <= 4.50 & train$Score > 29.50 & train$Questions <= 2.50] <- 'F'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions <= 4.50 & train$Score > 29.50 & train$Questions > 2.50] <- 'C'

decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions > 4.50 & train$Questions <= 5.50 & train$Score <= 16.00] <- 'F'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions > 4.50 & train$Questions <= 5.50 & train$Score > 16.00] <- 'C'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions > 4.50 & train$Questions > 5.50 & train$Score <= 7.00] <- 'C'
decision[train$Score <= 75.50 & train$Score <= 42.50 & train$Attendance > 0.66 & train$Questions > 4.50 & train$Questions > 5.50 & train$Score > 7.00] <- 'C'

decision[train$Score <= 75.50 & train$Score > 42.50 & train$Score <= 50.50 & train$Questions <= 2.50] <- 'F'
decision[train$Score <= 75.50 & train$Score > 42.50 & train$Score <= 50.50 & train$Questions > 2.50 & train$Attendance <= 0.25 & train$Row <= 8.50] <- 'F'
decision[train$Score <= 75.50 & train$Score > 42.50 & train$Score <= 50.50 & train$Questions > 2.50 & train$Attendance <= 0.25 & train$Row > 8.50] <- 'C'
decision[train$Score <= 75.50 & train$Score > 42.50 & train$Score <= 50.50 & train$Questions > 2.50 & train$Attendance > 0.25] <- 'C'

decision[train$Score <= 75.50 & train$Score > 50.50 & train$Score <= 70.50 & train$Score <= 60.50 & train$Questions <= 1.50] <- 'D'
decision[train$Score <= 75.50 & train$Score > 50.50 & train$Score <= 70.50 & train$Score <= 60.50 & train$Questions > 1.50] <- 'C'
decision[train$Score <= 75.50 & train$Score > 50.50 & train$Score <= 70.50 & train$Score > 60.50] <- 'C'

decision[train$Score <= 75.50 & train$Score > 70.50 & train$Major <= 1.50 & train$Major <= 0.50] <- 'C'
decision[train$Score <= 75.50 & train$Score > 70.50 & train$Major <= 1.50 & train$Major > 0.50] <- 'B'
decision[train$Score <= 75.50 & train$Score > 70.50 & train$Major > 1.50] <- 'C'

# Score > 75.50
decision[train$Score > 75.50 & train$Score <= 90.50 & train$Row <= 3.50 & train$Score <= 85.50] <- 'B'
decision[train$Score > 75.50 & train$Score <= 90.50 & train$Row <= 3.50 & train$Score > 85.50 & train$Score <= 89.50] <- 'A'
decision[train$Score > 75.50 & train$Score <= 90.50 & train$Row <= 3.50 & train$Score > 85.50 & train$Score > 89.50] <- 'B'

decision[train$Score > 75.50 & train$Score <= 90.50 & train$Row > 3.50 & train$Row <= 4.50 & train$Score <= 86.00] <- 'B'
decision[train$Score > 75.50 & train$Score <= 90.50 & train$Row > 3.50 & train$Row <= 4.50 & train$Score > 86.00] <- 'A'
decision[train$Score > 75.50 & train$Score <= 90.50 & train$Row > 4.50] <- 'B'

decision[train$Score > 90.50] <- 'A'

# Calculate accuracy
accuracy <- mean(decision == train$Grade)
print(paste("Updated accuracy:", accuracy))


misclassified <- train[decision != train$Grade, ]
table(misclassified$Score, misclassified$Grade)
#91% accuracy

test_decision <- rep('F', nrow(test))

test_decision[test$Score <= 42 & test$Attendance <= 0.6 & test$Questions <= 5.5] <- 'F'
test_decision[test$Score <= 42 & test$Attendance <= 0.6 & test$Questions > 5.5] <- 'F'
test_decision[test$Score <= 42 & test$Attendance > 0.6 & test$Questions <= 4.5] <- 'F'
test_decision[test$Score <= 42 & test$Attendance > 0.6 & test$Questions > 4.5] <- 'C'
test_decision[test$Score > 42.5 & test$Score <= 50.5 & test$Questions <= 2.5] <- 'F'
test_decision[test$Score > 42.5 & test$Score <= 50.5 & test$Questions > 2.5] <- 'C'
test_decision[test$Score > 50.5 & test$Score <= 70.5] <- 'C'
test_decision[test$Score > 70.5 & test$Score <= 75.5] <- 'C'
test_decision[test$Score > 75.5 & test$Score <= 90.5 & test$Row <= 3.5 & test$Score <= 85.5] <- 'B'
test_decision[test$Score > 75.5 & test$Score <= 90.5 & test$Row <= 3.5 & test$Score > 85.5] <- 'A'
test_decision[test$Score > 75.5 & test$Score <= 90.5 & test$Row > 3.5 & test$Row <= 4.5] <- 'B'
test_decision[test$Score > 75.5 & test$Score <= 90.5 & test$Row > 4.5] <- 'B'
test_decision[test$Score > 90.5] <- 'A'

submission$Grade <- test_decision
write.csv(submission, "submission2024.csv", row.names = FALSE)
