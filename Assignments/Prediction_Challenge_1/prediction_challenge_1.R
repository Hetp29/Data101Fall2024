getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_1")
train <- read.csv("MoodyTrain2024.csv")
test <- read.csv("MoodyTestStudents24.csv")
submission <- read.csv("submission2024.csv")

#structure of data
str(MoodyTrain2024)
summary(MoodyTrain2024)

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
write.csv(submission, "submission.csv", row.names = FALSE)

