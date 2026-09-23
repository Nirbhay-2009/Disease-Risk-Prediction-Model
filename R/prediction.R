# Disease Risk Prediction Using Statistical Modelling
# Prediction and Model Evaluation


# --------------------------------------------------
# 1. Load Dataset
# --------------------------------------------------

data <- read.csv("data/diabetes.csv")


# --------------------------------------------------
# 2. Split Data into Training and Testing Sets
# --------------------------------------------------

set.seed(123)

train_index <- c(
  sample(
    which(data$diabetes == 0),
    size = 0.8 * sum(data$diabetes == 0)
  ),
  
  sample(
    which(data$diabetes == 1),
    size = 0.8 * sum(data$diabetes == 1)
  )
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
# 3. Build Logistic Regression Model
# --------------------------------------------------

logistic_model <- glm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level +
    hypertension + heart_disease + gender + smoking_history,
  data = train_data,
  family = binomial
)

summary(logistic_model)


# --------------------------------------------------
# 4. Predict Probabilities on Test Data
# --------------------------------------------------

predicted_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

head(predicted_probability)


# --------------------------------------------------
# 5. Convert Probabilities into Classes
# --------------------------------------------------

predicted_class <- ifelse(
  predicted_probability >= 0.5,
  1,
  0
)

head(predicted_class)


# --------------------------------------------------
# 6. Confusion Matrix
# --------------------------------------------------

confusion_matrix <- table(
  Actual = test_data$diabetes,
  Predicted = predicted_class
)

confusion_matrix


# --------------------------------------------------
# 7. Model Performance
# --------------------------------------------------

TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]
TP <- confusion_matrix[2, 2]


# Accuracy
accuracy <- (TP + TN) /
  (TP + TN + FP + FN)


# Sensitivity
sensitivity <- TP /
  (TP + FN)


# Specificity
specificity <- TN /
  (TN + FP)


# Precision
precision <- TP /
  (TP + FP)


# F1 Score
f1_score <- 2 * (precision * sensitivity) /
  (precision + sensitivity)


# Display Results
accuracy

sensitivity

specificity

precision

f1_score


# --------------------------------------------------
# 8. ROC Curve and AUC
# --------------------------------------------------

# Install pROC once if required:
# install.packages("pROC")

library(pROC)


# Create ROC curve
roc_model <- roc(
  test_data$diabetes,
  predicted_probability
)


# Calculate AUC
auc_value <- auc(roc_model)

auc_value


# Display ROC curve
plot(
  roc_model,
  main = "ROC Curve - Diabetes Risk Prediction"
)


# Create graphs directory if required
if (!dir.exists("graphs")) {
  dir.create("graphs")
}


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


# --------------------------------------------------
# 9. User Input Prediction Function
# --------------------------------------------------

predict_diabetes_risk <- function(
    age,
    bmi,
    HbA1c_level,
    blood_glucose_level,
    hypertension,
    heart_disease,
    gender,
    smoking_history
) {
  
  # Create new patient data
  new_patient <- data.frame(
    age = age,
    bmi = bmi,
    HbA1c_level = HbA1c_level,
    blood_glucose_level = blood_glucose_level,
    hypertension = hypertension,
    heart_disease = heart_disease,
    gender = factor(
      gender,
      levels = logistic_model$xlevels$gender
    ),
    smoking_history = factor(
      smoking_history,
      levels = logistic_model$xlevels$smoking_history
    )
  )
  
  
  # Calculate prediction probability
  probability <- predict(
    logistic_model,
    newdata = new_patient,
    type = "response"
  )
  
  
  # Convert probability into prediction
  prediction <- ifelse(
    probability >= 0.5,
    1,
    0
  )
  
  
  # Return result
  return(
    list(
      probability = probability,
      prediction = prediction
    )
  )
}

