# Disease Risk Prediction Using Statistical Modelling
# Descriptive Statistical Analysis

# Load dataset
data <- read.csv("data/diabetes.csv")

# Display summary
summary(data)

# Mean
mean(data$age)
mean(data$bmi)
mean(data$HbA1c_level)
mean(data$blood_glucose_level)

# Median
median(data$age)
median(data$bmi)
median(data$HbA1c_level)
median(data$blood_glucose_level)

# Standard deviation
sd(data$age)
sd(data$bmi)
sd(data$HbA1c_level)
sd(data$blood_glucose_level)

# Mode
mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_value(data$age)
mode_value(data$bmi)
mode_value(data$HbA1c_level)
mode_value(data$blood_glucose_level)

# Quartiles
quantile(data$age)
quantile(data$bmi)
quantile(data$HbA1c_level)
quantile(data$blood_glucose_level)

# Percentiles
quantile(data$age, probs = c(0.25, 0.50, 0.75))
quantile(data$bmi, probs = c(0.25, 0.50, 0.75))
quantile(data$HbA1c_level, probs = c(0.25, 0.50, 0.75))
quantile(data$blood_glucose_level, probs = c(0.25, 0.50, 0.75))

# Variance
var(data$age)
var(data$bmi)
var(data$HbA1c_level)
var(data$blood_glucose_level)

# Range
range(data$age)
range(data$bmi)
range(data$HbA1c_level)
range(data$blood_glucose_level)