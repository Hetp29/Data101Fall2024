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



decision <- rep('F', nrow(train))
decision[train$Score <= 50 & train$Attendance > 0.6 & train$Questions > 5] <- 'C'
decision[train$Score <= 50 & train$Attendance <= 0.6 & train$Questions <= 5] <- 'F'
decision[train$Score > 50 & train$Score <= 75] <- 'C'
decision[train$Score > 50 & train$Score <= 75 & train$Attendance > 0.8 & train$Questions > 7] <- 'B'
decision[train$Score > 50 & train$Score <= 75 & train$Major == 'Stats' & train$Attendance > 0.75 & train$Questions > 7] <- 'B'
decision[train$Score > 50 & train$Score <= 75 & train$Major == 'Econ' & train$Attendance > 0.8] <- 'B'
decision[train$Score > 75 & train$Score <= 90] <- 'B'
decision[train$Score > 75 & train$Score <= 90 & train$Attendance > 0.85 & train$Questions > 8] <- 'A'
decision[train$Score > 90] <- 'A'
decision[train$Score > 90 & train$Attendance <= 0.5 & train$Questions <= 6] <- 'B'
decision[train$Row %in% c(1, 2, 3) & train$Score > 60 & train$Attendance > 0.7] <- 'C'
decision[train$Row %in% c(10, 11, 12) & train$Score > 90] <- 'A'
accuracy <- mean(decision == train$Grade)
print(paste("Updated accuracy:", accuracy))
misclassified <- train[decision != train$Grade, ]
table(misclassified$Score, misclassified$Grade)




# 1. Summary Statistics
cat("Summary of Misclassified Cases:\n")
summary(misclassified)
cat("\nSummary of Correctly Classified Cases:\n")
summary(train[decision == train$Grade, ])

# 2. Class-wise Error Analysis
cat("\nClass-wise Error Analysis (Actual vs Predicted Grades):\n")
table(train$Grade, decision)

# 3. Feature-Wise Comparison
cat("\nFeature-Wise Averages for Misclassified Cases:\n")
aggregate(cbind(Score, Attendance, Questions) ~ Grade, data = misclassified, FUN = mean)
cat("\nFeature-Wise Averages for Correctly Classified Cases:\n")
aggregate(cbind(Score, Attendance, Questions) ~ Grade, data = train[decision == train$Grade, ], FUN = mean)

# 4. Focus on Problematic Grades
cat("\nMisclassified Counts by Grade:\n")
table(misclassified$Grade)

# 5. Cross-tabulation of Features
cat("\nCross-tabulation of Score vs Misclassified Grade:\n")
table(cut(misclassified$Score, breaks = c(0, 50, 75, 90, 100)), misclassified$Grade)
cat("\nCross-tabulation of Attendance vs Misclassified Grade:\n")
table(cut(misclassified$Attendance, breaks = seq(0, 1, 0.2)), misclassified$Grade)

# 6. Analyze Prediction Errors by Major
cat("\nMisclassified Cases by Major:\n")
table(misclassified$Major, misclassified$Grade)

# 7. Inspect Boundary Conditions
boundary_cases <- train[train$Score %in% c(50, 75, 90), ]
cat("\nBoundary Cases (Score = 50, 75, 90):\n")
table(boundary_cases$Score, boundary_cases$Grade)

# 8. Investigate Individual Cases
cat("\nFirst Few Misclassified Cases:\n")
head(misclassified)

# 9. Precision and Recall Analysis
precision <- prop.table(table(decision, train$Grade), margin = 1)
recall <- prop.table(table(decision, train$Grade), margin = 2)
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("\nPrecision for Each Grade:\n")
print(precision)
cat("\nRecall for Each Grade:\n")
print(recall)
cat("\nF1 Score for Each Grade:\n")
print(f1_score)

# 10. Overlapping Ranges
cat("\nOverlapping Ranges of Features for Misclassified Cases:\n")
overlap <- aggregate(cbind(Score, Attendance, Questions) ~ Grade, data = misclassified, FUN = range)
print(overlap)

# 11. Correlation Between Features in Misclassified Cases
cat("\nCorrelation Between Features in Misclassified Cases:\n")
cor(misclassified[, c("Score", "Attendance", "Questions")], use = "complete.obs")

# 12. Check New Rules or Adjust Thresholds
cat("\nCases with Score > 90 and Attendance > 0.5 in Misclassified:\n")
table(misclassified$Score > 90 & misclassified$Attendance > 0.5)

