# Disease Risk Prediction Using Statistical Modelling
# Descriptive Statistical Analysis


# --------------------------------------------------
# 1. Load Dataset
# --------------------------------------------------

data <- read.csv("data/diabetes.csv")


# --------------------------------------------------
# 2. Display Summary
# --------------------------------------------------

summary(data)


# --------------------------------------------------
# 3. Mean
# --------------------------------------------------

mean(data$age)

mean(data$bmi)

mean(data$HbA1c_level)

mean(data$blood_glucose_level)


# --------------------------------------------------
# 4. Median
# --------------------------------------------------

median(data$age)

median(data$bmi)

median(data$HbA1c_level)

median(data$blood_glucose_level)


# --------------------------------------------------
# 5. Standard Deviation
# --------------------------------------------------

sd(data$age)

sd(data$bmi)

sd(data$HbA1c_level)

sd(data$blood_glucose_level)


# --------------------------------------------------
# 6. Mode
# --------------------------------------------------

mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_value(data$age)

mode_value(data$bmi)

mode_value(data$HbA1c_level)

mode_value(data$blood_glucose_level)


# --------------------------------------------------
# 7. Quartiles
# --------------------------------------------------

quantile(data$age)

quantile(data$bmi)

quantile(data$HbA1c_level)

quantile(data$blood_glucose_level)


# --------------------------------------------------
# 8. Variance
# --------------------------------------------------

var(data$age)

var(data$bmi)

var(data$HbA1c_level)

var(data$blood_glucose_level)


# --------------------------------------------------
# 9. Range
# --------------------------------------------------

range(data$age)

range(data$bmi)

range(data$HbA1c_level)

range(data$blood_glucose_level)