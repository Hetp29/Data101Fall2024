library(rpart)
library(rpart.plot)
library(ModelMetrics)

getwd()
setwd("/Users/hetpatel/Data101Fall2024/Data101Fall2024/Assignments/Prediction_Challenge_3")

airbnb_train <- read.csv("airbnbTrain.csv")
airbnb_test <- read.csv("airbnbTestStudents.csv")
boroughs <- read.csv("Boroughs.csv")

colnames(airbnb_train)[colnames(airbnb_train) == "neighbourhood"] <- "neighborhood"
colnames(airbnb_test)[colnames(airbnb_test) == "neighbourhood"] <- "neighborhood"

airbnb_train$neighborhood <- trimws(tolower(airbnb_train$neighborhood))
airbnb_test$neighborhood <- trimws(tolower(airbnb_test$neighborhood))
boroughs$neighborhood <- trimws(tolower(boroughs$neighborhood))

airbnb_train <- merge(airbnb_train, boroughs, by = "neighborhood", all.x = TRUE)
airbnb_test <- merge(airbnb_test, boroughs, by = "neighborhood", all.x = TRUE)

airbnb_train$price_per_sqft <- airbnb_train$price / airbnb_train$footage
airbnb_test$price_per_sqft <- airbnb_test$price / airbnb_test$footage

airbnb_train$price_per_sqft_sq <- airbnb_train$price_per_sqft^2
airbnb_test$price_per_sqft_sq <- airbnb_test$price_per_sqft^2
airbnb_train$avgreview_sq <- airbnb_train$avgreview^2
airbnb_test$avgreview_sq <- airbnb_test$avgreview^2
airbnb_train$price_avgreview <- airbnb_train$price_per_sqft * airbnb_train$avgreview
airbnb_test$price_avgreview <- airbnb_test$price_per_sqft * airbnb_test$avgreview

set.seed(123)
split <- 0.8 * nrow(airbnb_train)
validation_train <- airbnb_train[1:split, ]
validation_test <- airbnb_train[(split + 1):nrow(airbnb_train), ]

colnames(validation_train)[colnames(validation_train) == "Deal"] <- "deal"
colnames(validation_test)[colnames(validation_test) == "Deal"] <- "deal"

validation_train$deal <- as.factor(validation_train$deal)
validation_test$deal <- as.factor(validation_test$deal)

class_weights <- as.numeric(table(validation_train$deal))
class_weights <- sum(class_weights) / (length(class_weights) * class_weights)

tree_model <- rpart(
  deal ~ price_per_sqft + avgreview + borough + price_per_sqft_sq + avgreview_sq + price_avgreview, 
  data = validation_train, 
  method = "class",
  parms = list(prior = c(0.25, 0.25, 0.50), split = "gini"), # Adjust class distribution
  control = rpart.control(cp = 0.0003, minsplit = 5, maxdepth = 30)
)

opt_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
tree_model <- prune(tree_model, cp = opt_cp)

rpart.plot(tree_model)

tree_train_predictions <- predict(tree_model, newdata = validation_train, type = "class")
tree_test_predictions <- predict(tree_model, newdata = validation_test, type = "class")
tree_train_accuracy <- mean(tree_train_predictions == validation_train$deal) * 100
tree_test_accuracy <- mean(tree_test_predictions == validation_test$deal) * 100
print(paste("Decision Tree Training Accuracy:", tree_train_accuracy, "%"))
print(paste("Decision Tree Test Accuracy:", tree_test_accuracy, "%"))

lm_model <- lm(price ~ price_per_sqft + avgreview + borough + price_per_sqft_sq + avgreview_sq, 
               data = validation_train)

summary(lm_model)

lm_train_predictions <- predict(lm_model, newdata = validation_train)
lm_test_predictions <- predict(lm_model, newdata = validation_test)

lm_train_mse <- mse(validation_train$price, lm_train_predictions)
lm_test_mse <- mse(validation_test$price, lm_test_predictions)
print(paste("Linear Regression Training MSE:", lm_train_mse))
print(paste("Linear Regression Test MSE:", lm_test_mse))

lm_train_accuracy <- 1 - sqrt(lm_train_mse) / mean(validation_train$price)
lm_test_accuracy <- 1 - sqrt(lm_test_mse) / mean(validation_test$price)
print(paste("Linear Regression Training Accuracy:", lm_train_accuracy * 100, "%"))
print(paste("Linear Regression Test Accuracy:", lm_test_accuracy * 100, "%"))

if (!"id" %in% colnames(airbnb_test)) {
  airbnb_test$id <- 1:nrow(airbnb_test)  
}
 
submission <- data.frame(
  id = airbnb_test$id,  
  deal = predict(tree_model, newdata = airbnb_test, type = "class")  
)

write.csv(submission, "submission_airbnb.csv", row.names = FALSE)