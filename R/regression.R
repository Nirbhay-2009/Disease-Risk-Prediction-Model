# Disease Risk Prediction Using Statistical Modelling
# Regression Analysis

# Load dataset
data <- read.csv("data/diabetes.csv")

# Simple Linear Regression
model <- lm(diabetes ~ blood_glucose_level, data = data)

# Display model summary
summary(model)

# Multiple Linear Regression
multiple_model <- lm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level,
  data = data
)

# Display model summary
summary(multiple_model)

# Logistic Regression
logistic_model <- glm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level +
    hypertension + heart_disease + gender + smoking_history,
  data = data,
  family = binomial
)

# Display model summary
summary(logistic_model)

# Odds Ratios
exp(coef(logistic_model))

# Odds Ratio with 95% Confidence Interval
exp(cbind(
  Odds_Ratio = coef(logistic_model),
  confint(logistic_model)
))