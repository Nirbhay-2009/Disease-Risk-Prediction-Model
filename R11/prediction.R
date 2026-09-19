# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Check the structure of the dataset
str(data)

# Convert categorical variables into factors
data$gender <- as.factor(data$gender)
data$smoking_history <- as.factor(data$smoking_history)
str(data)

set.seed(123)
train_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index,]
str(test_data)

# Build Logistic Regression Model

model <- glm(
  diabetes ~ gender + age + hypertension + bmi +
    HbA1c_level + blood_glucose_level +
    smoking_history + heart_disease,
  data = train_data,
  family = binomial
)

# Display model summary
summary(model)


# Predict diabetes probability for test data

probability <- predict(
  model,
  newdata = test_data,
  type = "response"
)

# Convert probability into 0 or 1

predicted <- ifelse(probability >= 0.5, 1, 0)

# Create confusion matrix

confusion_matrix <- table(
  Predicted = predicted,
  Actual = test_data$diabetes
)

print(confusion_matrix)

# Calculate model performance

TN <- confusion_matrix["0", "0"]
FP <- confusion_matrix["1", "0"]
FN <- confusion_matrix["0", "1"]
TP <- confusion_matrix["1", "1"]


# Accuracy
accuracy <- (TP + TN) / sum(confusion_matrix)

# Sensitivity
sensitivity <- TP / (TP + FN)

# Specificity
specificity <- TN / (TN + FP)

# Precision
precision <- TP / (TP + FP)


# Display results

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")

# Save the trained model
save(model, file = "outputs11/diabetes_model.RData")
