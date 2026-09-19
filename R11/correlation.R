# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Correlation Analysis

cat("----- CORRELATION ANALYSIS -----\n\n")


# Age and BMI
cat("Age and BMI:", cor(data$age, data$bmi), "\n")


# Age and HbA1c
cat("Age and HbA1c Level:", cor(data$age, data$HbA1c_level), "\n")


# Age and Blood Glucose
cat("Age and Blood Glucose Level:",
    cor(data$age, data$blood_glucose_level), "\n")


# BMI and HbA1c
cat("BMI and HbA1c Level:",
    cor(data$bmi, data$HbA1c_level), "\n")


# BMI and Blood Glucose
cat("BMI and Blood Glucose Level:",
    cor(data$bmi, data$blood_glucose_level), "\n")


# HbA1c and Blood Glucose
cat("HbA1c and Blood Glucose Level:",
    cor(data$HbA1c_level, data$blood_glucose_level), "\n")