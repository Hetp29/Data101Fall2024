getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_3")

airbnb_train <- read.csv("airbnbTrain.csv")
airbnb_test <- read.csv("airbnbTestStudents.csv")
boroughs <- read.csv("Boroughs.csv")

head(airbnb_train)
head(airbnb_test)
head(boroughs)

colnames(airbnb_train)[colnames(airbnb_train) == "neighbourhood"] <- "neighborhood"
colnames(airbnb_test)[colnames(airbnb_test) == "neighbourhood"] <- "neighborhood"

airbnb_train$neighborhood <- trimws(tolower(airbnb_train$neighborhood))
airbnb_test$neighborhood <- trimws(tolower(airbnb_test$neighborhood))
boroughs$neighborhood <- trimws(tolower(boroughs$neighborhood))

airbnb_train <- merge(airbnb_train, boroughs, by = "neighborhood", all.x = TRUE)
airbnb_test <- merge(airbnb_test, boroughs, by = "neighborhood", all.x = TRUE)

head(airbnb_train)
head(airbnb_test)

airbnb_train$price_per_sqft <- airbnb_train$price / airbnb_train$footage
airbnb_test$price_per_sqft <- airbnb_test$price / airbnb_test$footage

set.seed(123)
split <- 0.8 * nrow(airbnb_train)
validation_train <- airbnb_train[1:split, ]
validation_test <- airbnb_train[(split + 1):nrow(airbnb_train), ]

library(rpart)
library(rpart.plot)

colnames(validation_train)[colnames(validation_train) == "Deal"] <- "deal"
colnames(validation_test)[colnames(validation_test) == "Deal"] <- "deal"

validation_train$deal <- as.factor(validation_train$deal)
validation_test$deal <- as.factor(validation_test$deal)

tree_model <- rpart(
  deal ~ price_per_sqft + avgreview + borough, 
  data = validation_train, 
  method = "class",
  control = rpart.control(cp = 0.001, minsplit = 5, maxdepth = 30)
)
rpart.plot(tree_model)
tree_predictions <- predict(tree_model, newdata = validation_test, type = "class")
tree_accuracy <- mean(tree_predictions == validation_test$deal)
print(paste("Decision Tree Accuracy with Hyperparameter Tuning:", tree_accuracy))
