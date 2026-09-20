# Disease Risk Prediction Using Statistical Modelling

# Load dataset
data <- read.csv("data/diabetes.csv")

# --------------------------------------------------
# 1. Split data into Training and Testing sets
# --------------------------------------------------

set.seed(123)

train_index <- c(
  sample(which(data$diabetes == 0),
         size = 0.8 * sum(data$diabetes == 0)),
  
  sample(which(data$diabetes == 1),
         size = 0.8 * sum(data$diabetes == 1))
)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Check sizes
dim(train_data)
dim(test_data)

# Check diabetes distribution
table(train_data$diabetes)
table(test_data$diabetes)


# --------------------------------------------------
# 2. Build Logistic Regression Model
# --------------------------------------------------

logistic_model <- glm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level +
    hypertension + heart_disease + gender + smoking_history,
  data = train_data,
  family = binomial
)

summary(logistic_model)


# --------------------------------------------------
# 3. Predict probabilities on Test Data
# --------------------------------------------------

predicted_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

head(predicted_probability)


# --------------------------------------------------
# 4. Convert probabilities into classes
# --------------------------------------------------

predicted_class <- ifelse(
  predicted_probability >= 0.5,
  1,
  0
)

head(predicted_class)

# --------------------------------------------------
# 5. Confusion Matrix
# --------------------------------------------------

confusion_matrix <- table(
  Actual = test_data$diabetes,
  Predicted = predicted_class
)

confusion_matrix

# --------------------------------------------------
# 6. Model Performance
# --------------------------------------------------

TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]
TP <- confusion_matrix[2, 2]

# Accuracy
accuracy <- (TP + TN) / (TP + TN + FP + FN)

# Sensitivity
sensitivity <- TP / (TP + FN)

# Specificity
specificity <- TN / (TN + FP)

# Precision
precision <- TP / (TP + FP)

# F1 Score
f1_score <- 2 * (precision * sensitivity) /
  (precision + sensitivity)

# Display results
accuracy
sensitivity
specificity
precision
f1_score

# --------------------------------------------------
# 7. ROC Curve and AUC
# --------------------------------------------------

# Install package once if needed:
# install.packages("pROC")

library(pROC)

# Create ROC curve
roc_model <- roc(
  test_data$diabetes,
  predicted_probability
)

# Display AUC
auc(roc_model)

# Plot ROC curve
plot(
  roc_model,
  main = "ROC Curve - Diabetes Risk Prediction"
)

# Save ROC curve
png(
  "graphs/roc_curve.png",
  width = 800,
  height = 600
)

plot(
  roc_model,
  main = "ROC Curve - Diabetes Risk Prediction"
)

dev.off()