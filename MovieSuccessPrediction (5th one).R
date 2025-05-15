# load necessary libraries 
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(pROC)
library(ROCR)
library(randomForest)
library(randomForestExplainer)

library(rpart)
library(glmnet)
library(tidyverse)
library(Metrics)
library(class)

# read data 
movies_data <- read.csv('dataset4.csv')

# data analysis 
str(movies_data)
summary(movies_data)

# select relevant columns
movies_data <- movies_data %>% 
  select(averageRating, releaseYear, runtimeMinutes, numVotes, num_regions, num_languages, genre1, genre2, actor1, actress1, director1, writer1)

# check null percentages for all columns
null_percentages <- colMeans(is.na(movies_data)) * 100
null_summary <- data.frame(Column = names(movies_data), Null_Percentage = null_percentages)
print(null_summary)

# remove outlier values
numeric_cols <- c('runtimeMinutes','numVotes','num_regions','num_languages')
for (col_name in numeric_cols) {
  percentile_01 <- quantile(movies_data[[col_name]], 0.001, na.rm = TRUE)
  percentile_99 <- quantile(movies_data[[col_name]], 0.999, na.rm = TRUE)
  movies_data <- movies_data[movies_data[, col_name] > percentile_01, ]
  movies_data <- movies_data[movies_data[, col_name] < percentile_99, ]
}

summary(movies_data)

# transform averageRating column to movieSuccess
movies_data$movieSuccess <- ifelse(movies_data$averageRating >= 7, 1, 0)
summary(movies_data)

movies_data$averageRating <- NULL

movies_data$genre1 <- as.numeric(factor(movies_data$genre1))
movies_data$genre2 <- as.numeric(factor(movies_data$genre2))
movies_data$actor1 <- as.numeric(factor(movies_data$actor1))
movies_data$actress1 <- as.numeric(factor(movies_data$actress1))
movies_data$director1 <- as.numeric(factor(movies_data$director1))
movies_data$writer1 <- as.numeric(factor(movies_data$writer1))


# Display the result
head(movies_data)
str(movies_data)

# Split data into training and testing sets
train_indices <- sample(1:nrow(movies_data), 0.7 * nrow(movies_data))  # 70% training data
train_data <- movies_data[train_indices, ]
test_data <- movies_data[-train_indices, ]

# ==============================logistic regression model==============================

logistic_model <- glm(movieSuccess ~ ., data = train_data, family = binomial)
summary(logistic_model)

# Predict on the testing data
predictions <- predict(logistic_model, newdata = test_data, type = 'response')
predictions_prob <- as.factor(if_else(predictions > 0.28, 1, 0))

# Compute confusion matrix
conf_matrix <- confusionMatrix(predictions_prob, as.factor(test_data$movieSuccess))
print("Confusion Matrix:")
print(conf_matrix)

true_positive <- conf_matrix$table["1", "1"]
true_negative <- conf_matrix$table["0", "0"]
false_positive <- conf_matrix$table["1", "0"]
false_negative <- conf_matrix$table["0", "1"]

# Calculate Precision, Recall, and F1-Score
precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", round(precision, 2)))
print(paste("Recall:", round(recall, 2)))
print(paste("F1-Score:", round(f1_score, 2)))

# ROC Curve and AUC
roc_curve <- suppressMessages(roc(test_data$movieSuccess, predictions))

# Calculate AUC
auc_value <- roc_curve$auc
print(paste("AUC:", round(auc_value, 2)))

plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

# Plot p-values (for the training data model)
p_values <- summary(logistic_model)$coefficients[, "Pr(>|z|)"]
p_values_df <- data.frame(Predictor = names(p_values), P_Value = p_values)

ggplot(p_values_df, aes(x = reorder(Predictor, P_Value), y = P_Value)) +
  geom_point(size = 3, color = "blue") +
  coord_flip() +
  labs(x = "Predictor Variable", y = "P-Value") +
  ggtitle("P-Values of Predictor Variables in Linear Regression Model") +
  theme_minimal()

# ============================== Random Forest model ===================================

# Convert movieSuccess to a factor
train_data$movieSuccess <- as.factor(train_data$movieSuccess)
test_data$movieSuccess <- as.factor(test_data$movieSuccess)

rf_model <- randomForest(movieSuccess ~ ., data = train_data, ntree = 100)
predictions_prob <- predict(rf_model, newdata = test_data, type = 'prob')[, "1"]

# Predict on the test data
threshold <- 0.33  # Lower the threshold
predictions <- as.factor(ifelse(predictions_prob > threshold, "1", "0"))

# Create confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$movieSuccess)
print("Confusion Matrix:")
print(confusion_matrix)

true_positive <- confusion_matrix$table["1", "1"]
true_negative <- confusion_matrix$table["0", "0"]
false_positive <- confusion_matrix$table["1", "0"]
false_negative <- confusion_matrix$table["0", "1"]

# Calculate Precision, Recall, and F1-Score
precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", round(precision, 2)))
print(paste("Recall:", round(recall, 2)))
print(paste("F1-Score:", round(f1_score, 2)))

# Create the ROC curve and calculate AUC
roc_curve <- roc(test_data$movieSuccess, predictions_prob)
auc_value <- roc_curve$auc
print(paste("AUC:", round(auc_value, 2)))

# Plot ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

################## SVM Model ########################

library(e1071)

svm_model <- svm(movieSuccess ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1, probability = TRUE)

# Predict probabilities on the test data
svm_probabilities <- attr(predict(svm_model, newdata = test_data, probability = TRUE), "probabilities")[, "1"]

# Apply the 0.24 threshold to classify predictions
threshold <- 0.19
svm_predictions <- as.factor(ifelse(svm_probabilities > threshold, "1", "0"))

# Create confusion matrix
conf_matrix <- confusionMatrix(svm_predictions, test_data$movieSuccess)

# Print the confusion matrix and calculate metrics
print("Confusion Matrix:")
print(conf_matrix)

true_positive <- conf_matrix$table["1", "1"]
true_negative <- conf_matrix$table["0", "0"]
false_positive <- conf_matrix$table["1", "0"]
false_negative <- conf_matrix$table["0", "1"]

precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", round(precision, 2)))
print(paste("Recall:", round(recall, 2)))
print(paste("F1-Score:", round(f1_score, 2)))

# Create the ROC curve and calculate AUC
roc_curve <- roc(test_data$movieSuccess, svm_probabilities)
auc_value <- roc_curve$auc
print(paste("AUC:", round(auc_value, 2)))

# Plot ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

# ======================================== KNN Model ===============================

train_data$movieSuccess <- as.factor(train_data$movieSuccess)
test_data$movieSuccess <- as.factor(test_data$movieSuccess)

train_data$genre1 <- as.numeric(factor(train_data$genre1))
test_data$genre1 <- as.numeric(factor(test_data$genre1))

# Perform k-NN
k <- 3
knn_predictions <- knn(train = train_data[ , -which(names(train_data) == 'movieSuccess')],
                       test = test_data[ , -which(names(test_data) == 'movieSuccess')],
                       cl = train_data$movieSuccess,
                       k = k)

# Convert knn_predictions to factor with the same levels as test_data$movieSuccess
knn_predictions <- as.factor(knn_predictions)
levels(knn_predictions) <- levels(test_data$movieSuccess)

# Create confusion matrix
confusion_matrix <- confusionMatrix(knn_predictions, test_data$movieSuccess)
print("Confusion Matrix:")
print(confusion_matrix)

true_positive <- confusion_matrix$table["1", "1"]
true_negative <- confusion_matrix$table["0", "0"]
false_positive <- confusion_matrix$table["1", "0"]
false_negative <- confusion_matrix$table["0", "1"]

# Calculate Precision, Recall, and F1-Score
precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", round(precision, 2)))
print(paste("Recall:", round(recall, 2)))
print(paste("F1-Score:", round(f1_score, 2)))

actual <- as.numeric(test_data$movieSuccess) - 1  # Subtract 1 to get 0 and 1
predicted_probs <- as.numeric(knn_predictions) - 1

# Compute ROC curve and AUC
roc_curve <- roc(actual, predicted_probs)
auc_value <- roc_curve$auc
print(paste("AUC:", round(auc_value, 2)))

# Plot ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)


ctrl <- trainControl(method = "repeatedcv", repeats = 2)
k_grid <- expand.grid(k = seq(1, 20))

knn_model <- train(movieSuccess ~ ., data = train_data, method = "knn", trControl = ctrl, tuneGrid = k_grid)
plot(knn_model)

k_values <- seq(1, 20, by = 2)
accuracy_values <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  knn_model <- knn(train = train_data[, -which(names(train_data) == 'movieSuccess')],
                   test = test_data[, -which(names(test_data) == 'movieSuccess')],
                   cl = train_data$movieSuccess,
                   k = k_values[i])
  
  predictions <- as.factor(knn_model)
  accuracy_values[i] <- sum(predictions == test_data$movieSuccess) / length(predictions)
}

plot(k_values, accuracy_values, type = "o", pch = 19, col = "blue", ylim = c(0, 1), xlab = "Number of Neighbors (k)", ylab = "Accuracy", main = "Accuracy vs. Number of Neighbors (k)")

best_k <- k_values[which.max(accuracy_values)]
points(best_k, max(accuracy_values), col = "red", pch = 19)
legend("bottomright", legend = c("Accuracy", paste("Best k =", best_k)), col = c("blue", "red"), pch = c(19, 19))
