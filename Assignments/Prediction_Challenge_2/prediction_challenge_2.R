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
              data = train, method = "class",
              control = rpart.control(minsplit = 50, minbucket = 25, cp = 0.01))

prediction <- predict(tree, train, type = "class")
accuracy <- mean(prediction == train$Outcome)
print(paste("Updated Training Accuracy:", accuracy))

