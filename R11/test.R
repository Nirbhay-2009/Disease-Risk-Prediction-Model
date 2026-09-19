# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Convert categorical variables into factors
data$gender <- as.factor(data$gender)
data$smoking_history <- as.factor(data$smoking_history)


# Load the trained model
load("outputs11/diabetes_model.RData")


# Create a new patient record
new_patient <- data.frame(
  gender = factor("Female", levels = levels(data$gender)),
  age = 50,
  hypertension = 1,
  bmi = 30,
  HbA1c_level = 6.5,
  blood_glucose_level = 180,
  smoking_history = factor("never", levels = levels(data$smoking_history)),
  heart_disease = 0
)


# Predict diabetes probability
new_probability <- predict(
  model,
  newdata = new_patient,
  type = "response"
)


# Convert probability into prediction
new_prediction <- ifelse(new_probability >= 0.5, 1, 0)


# Display prediction
cat("Predicted Probability:", new_probability, "\n")
cat("Predicted Diabetes:", new_prediction, "\n")