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

tree <- rpart(Outcome ~ GroomMB + BrideMB + GroomInc + BrideInc, data = train, method = "class")
summary(tree)
prediction <- predict(tree, train, type="class")
accuracy <- mean(prediction == train$Outcome)
print(paste("Training Accuracy:", accuracy))
misclassified <- train[prediction != train$Outcome, ]
table(misclassified$GroomMB, misclassified$BrideMB)




# Add Income Difference column to the training data
train$IncomeDiff <- abs(train$GroomInc - train$BrideInc)

# Boxplot to visualize how IncomeDiff varies by Outcome
boxplot(IncomeDiff ~ Outcome, data = train, main = "Income Difference by Outcome")

# Summary statistics for IncomeDiff
aggregate(IncomeDiff ~ Outcome, data = train, summary)



# Add Total Income column to the training data
train$TotalInc <- train$GroomInc + train$BrideInc

# Boxplot to visualize how TotalInc varies by Outcome
boxplot(TotalInc ~ Outcome, data = train, main = "Total Income by Outcome")

# Summary statistics for TotalInc
aggregate(TotalInc ~ Outcome, data = train, summary)



# Create a new column for the MBTI pair
train$MBPair <- paste(train$GroomMB, train$BrideMB, sep = "_")

# Frequency table of MBTI pairs and outcomes
table(train$MBPair, train$Outcome)

# Subset analysis for common misclassified MBTI pairs
misclassified_pairs <- train[prediction != train$Outcome, ]
table(misclassified_pairs$MBPair)


# Train the decision tree with new features
tree <- rpart(Outcome ~ GroomMB + BrideMB + GroomInc + BrideInc + IncomeDiff + TotalInc + MBPair,
              data = train, method = "class",
              control = rpart.control(minsplit = 50, minbucket = 25, cp = 0.01))

# Check the training accuracy
prediction <- predict(tree, train, type = "class")
accuracy <- mean(prediction == train$Outcome)
print(paste("Updated Training Accuracy:", accuracy))