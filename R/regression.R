# Disease Risk Prediction Using Statistical Modelling
# Regression Analysis


# --------------------------------------------------
# 1. Load Dataset
# --------------------------------------------------

data <- read.csv("data/diabetes.csv")


# --------------------------------------------------
# 2. Simple Linear Regression
# --------------------------------------------------

simple_model <- lm(
  diabetes ~ blood_glucose_level,
  data = data
)

# Display model summary
summary(simple_model)


# --------------------------------------------------
# 3. Multiple Linear Regression
# --------------------------------------------------

multiple_model <- lm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level,
  data = data
)

# Display model summary
summary(multiple_model)


# --------------------------------------------------
# 4. Logistic Regression
# --------------------------------------------------

logistic_model <- glm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level +
    hypertension + heart_disease + gender + smoking_history,
  data = data,
  family = binomial
)

# Display model summary
summary(logistic_model)


# --------------------------------------------------
# 5. Odds Ratios
# --------------------------------------------------

exp(coef(logistic_model))


# --------------------------------------------------
# 6. Odds Ratios with 95% Confidence Interval
# --------------------------------------------------

exp(
  cbind(
    Odds_Ratio = coef(logistic_model),
    confint(logistic_model)
  )
)