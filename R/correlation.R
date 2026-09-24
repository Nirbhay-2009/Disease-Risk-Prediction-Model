# Disease Risk Prediction Using Statistical Modelling
# Correlation Analysis

# Load dataset
data <- read.csv("data/diabetes.csv")


# --------------------------------------------------
# 1. Pearson Correlation Matrix
# --------------------------------------------------

cor_matrix <- cor(
  data[, c(
    "age",
    "bmi",
    "HbA1c_level",
    "blood_glucose_level"
  )]
)

print(cor_matrix)


# --------------------------------------------------
# 2. Correlation of Variables with Diabetes
# --------------------------------------------------

cor_age_diabetes <- cor(
  data$age,
  data$diabetes
)

cor_bmi_diabetes <- cor(
  data$bmi,
  data$diabetes
)

cor_hba1c_diabetes <- cor(
  data$HbA1c_level,
  data$diabetes
)

cor_glucose_diabetes <- cor(
  data$blood_glucose_level,
  data$diabetes
)

print(cor_age_diabetes)
print(cor_bmi_diabetes)
print(cor_hba1c_diabetes)
print(cor_glucose_diabetes)


# --------------------------------------------------
# 3. Significance Tests for Correlation with Diabetes
# --------------------------------------------------

cor.test(
  data$age,
  data$diabetes
)

cor.test(
  data$bmi,
  data$diabetes
)

cor.test(
  data$HbA1c_level,
  data$diabetes
)

cor.test(
  data$blood_glucose_level,
  data$diabetes
)