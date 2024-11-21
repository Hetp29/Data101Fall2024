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

# test_decision <- rep('F', nrow(test))
# test_decision[test$Score > 50] <- 'D'
# test_decision[test$Score > 60] <- 'C'
# test_decision[test$Score > 75] <- 'B'
# test_decision[test$Score > 90] <- 'A'

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

# misclassified <- train[decision != train$Grade, ]
# table(misclassified$Score, misclassified$Grade)
# table(misclassified$Attendance, misclassified$Grade)
# table(misclassified$Questions, misclassified$Grade)
# table(misclassified$Major, misclassified$Grade)

# subset_low <- subset(train, Score <= 60 & decision != train$Grade)
# subset_mid <- subset(train, Score > 60 & Score <= 90 & decision != train$Grade)
# subset_high <- subset(train, Score > 90 & decision != train$Grade)

# summary(subset_low)
# summary(subset_mid)
# summary(subset_high)
# 
# table(misclassified$Major, misclassified$Grade)

# summary(misclassified$Attendance)
# summary(misclassified$Questions)


decision <- rep('F', nrow(train))
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance <= 0.6 & train$Questions <= 5] <- 'F'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance <= 0.6 & train$Questions > 5 & train$Attendance <= 0.3] <- 'F'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance <= 0.6 & train$Questions > 5 & train$Attendance > 0.3 & train$Score <= 25] <- 'F'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance <= 0.6 & train$Questions > 5 & train$Attendance > 0.3 & train$Score > 25] <- 'C'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions <= 4 & train$Score <= 29] <- 'F'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions <= 4 & train$Score > 29 & train$Questions <= 2] <- 'F'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions <= 4 & train$Score > 29 & train$Questions > 2] <- 'C'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions > 4 & train$Questions <= 5 & train$Score <= 16] <- 'F'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions > 4 & train$Questions <= 5 & train$Score > 16] <- 'C'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions > 4 & train$Questions > 5 & train$Score <= 7] <- 'C'
decision[train$Score <= 75 & train$Score <= 42 & train$Attendance > 0.6 & train$Questions > 4 & train$Questions > 5 & train$Score > 7] <- 'C'
decision[train$Score <= 75 & train$Score > 42 & train$Score <= 50 & train$Questions <= 2] <- 'F'
decision[train$Score <= 75 & train$Score > 42 & train$Score <= 50 & train$Questions > 2 & train$Attendance <= 0.2 & train$Row <= 8] <- 'F'
decision[train$Score <= 75 & train$Score > 42 & train$Score <= 50 & train$Questions > 2 & train$Attendance <= 0.2 & train$Row > 8] <- 'C'
decision[train$Score <= 75 & train$Score > 42 & train$Score <= 50 & train$Questions > 2 & train$Attendance > 0.2] <- 'C'
decision[train$Score <= 75 & train$Score > 50 & train$Score <= 70 & train$Score <= 60 & train$Questions <= 1] <- 'D'
decision[train$Score <= 75 & train$Score > 50 & train$Score <= 70 & train$Score <= 60 & train$Questions > 1] <- 'C'
decision[train$Score <= 75 & train$Score > 50 & train$Score <= 70 & train$Score > 60] <- 'C'
decision[train$Score <= 75 & train$Score > 70 ] <- 'C'
decision[train$Score <= 75 & train$Score > 70 ] <- 'B'
decision[train$Score <= 75 & train$Score > 70 ] <- 'C'
decision[train$Score > 75 & train$Score <= 90 & train$Row <= 3 & train$Score <= 85] <- 'B'
decision[train$Score > 75 & train$Score <= 90 & train$Row <= 3 & train$Score > 85 & train$Score <= 89] <- 'A'
decision[train$Score > 75 & train$Score <= 90 & train$Row <= 3 & train$Score > 85 & train$Score > 89] <- 'B'
decision[train$Score > 75 & train$Score <= 90 & train$Row > 3 & train$Row <= 4 & train$Score <= 86] <- 'B'
decision[train$Score > 75 & train$Score <= 90 & train$Row > 3 & train$Row <= 4 & train$Score > 86] <- 'A'
decision[train$Score > 75 & train$Score <= 90 & train$Row > 4] <- 'B'
decision[train$Score > 90] <- 'A'
accuracy <- round(mean(decision == train$Grade), 2)
accuracy_exact <- mean(decision == train$Grade)
print(paste("Updated accuracy:", accuracy))
print(paste("Exact accuracy:", accuracy_exact))
misclassified <- train[decision != train$Grade, ]
table(misclassified$Score, misclassified$Grade)
#96% accuracy 

test_decision <- rep('F', nrow(test))
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance <= 0.6 & test$Questions <= 5] <- 'F'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance <= 0.6 & test$Questions > 5 & test$Attendance <= 0.3] <- 'F'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance <= 0.6 & test$Questions > 5 & test$Attendance > 0.3 & test$Score <= 25] <- 'F'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance <= 0.6 & test$Questions > 5 & test$Attendance > 0.3 & test$Score > 25] <- 'C'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions <= 4 & test$Score <= 29] <- 'F'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions <= 4 & test$Score > 29 & test$Questions <= 2] <- 'F'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions <= 4 & test$Score > 29 & test$Questions > 2] <- 'C'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions > 4 & test$Questions <= 5 & test$Score <= 16] <- 'F'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions > 4 & test$Questions <= 5 & test$Score > 16] <- 'C'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions > 4 & test$Questions > 5 & test$Score <= 7] <- 'C'
test_decision[test$Score <= 75 & test$Score <= 42 & test$Attendance > 0.6 & test$Questions > 4 & test$Questions > 5 & test$Score > 7] <- 'C'
test_decision[test$Score <= 75 & test$Score > 42 & test$Score <= 50 & test$Questions <= 2] <- 'F'
test_decision[test$Score <= 75 & test$Score > 42 & test$Score <= 50 & test$Questions > 2 & test$Attendance <= 0.2 & test$Row <= 8] <- 'F'
test_decision[test$Score <= 75 & test$Score > 42 & test$Score <= 50 & test$Questions > 2 & test$Attendance <= 0.2 & test$Row > 8] <- 'C'
test_decision[test$Score <= 75 & test$Score > 42 & test$Score <= 50 & test$Questions > 2 & test$Attendance > 0.2] <- 'C'
test_decision[test$Score <= 75 & test$Score > 50 & test$Score <= 70 & test$Score <= 60 & test$Questions <= 1] <- 'D'
test_decision[test$Score <= 75 & test$Score > 50 & test$Score <= 70 & test$Score <= 60 & test$Questions > 1] <- 'C'
test_decision[test$Score <= 75 & test$Score > 50 & test$Score <= 70 & test$Score > 60] <- 'C'
test_decision[test$Score <= 75 & test$Score > 70 ] <- 'C'
test_decision[test$Score <= 75 & test$Score > 70 ] <- 'B'
test_decision[test$Score <= 75 & test$Score > 70 ] <- 'C'
test_decision[test$Score > 75 & test$Score <= 90 & test$Row <= 3 & test$Score <= 85] <- 'B'
test_decision[test$Score > 75 & test$Score <= 90 & test$Row <= 3 & test$Score > 85 & test$Score <= 89] <- 'A'
test_decision[test$Score > 75 & test$Score <= 90 & test$Row <= 3 & test$Score > 85 & test$Score > 89] <- 'B'
test_decision[test$Score > 75 & test$Score <= 90 & test$Row > 3 & test$Row <= 4 & test$Score <= 86] <- 'B'
test_decision[test$Score > 75 & test$Score <= 90 & test$Row > 3 & test$Row <= 4 & test$Score > 86] <- 'A'
test_decision[test$Score > 75 & test$Score <= 90 & test$Row > 4] <- 'B'
test_decision[test$Score > 90] <- 'A'

submission$Grade <- test_decision
write.csv(submission, "submission2024.csv", row.names = FALSE)

