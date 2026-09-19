

# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Mean
cat("----- MEAN -----\n")

cat("Age:", mean(data$age), "\n")
cat("BMI:", mean(data$bmi), "\n")
cat("HbA1c Level:", mean(data$HbA1c_level), "\n")
cat("Blood Glucose Level:", mean(data$blood_glucose_level), "\n\n")


# Median
cat("----- MEDIAN -----\n")

cat("Age:", median(data$age), "\n")
cat("BMI:", median(data$bmi), "\n")
cat("HbA1c Level:", median(data$HbA1c_level), "\n")
cat("Blood Glucose Level:", median(data$blood_glucose_level), "\n\n")


# Minimum and Maximum
cat("----- MINIMUM AND MAXIMUM -----\n")

cat("Age - Min:", min(data$age), " Max:", max(data$age), "\n")
cat("BMI - Min:", min(data$bmi), " Max:", max(data$bmi), "\n")
cat("HbA1c Level - Min:", min(data$HbA1c_level),
    " Max:", max(data$HbA1c_level), "\n")
cat("Blood Glucose Level - Min:", min(data$blood_glucose_level),
    " Max:", max(data$blood_glucose_level), "\n\n")


# Standard Deviation
cat("----- STANDARD DEVIATION -----\n")

cat("Age:", sd(data$age), "\n")
cat("BMI:", sd(data$bmi), "\n")
cat("HbA1c Level:", sd(data$HbA1c_level), "\n")
cat("Blood Glucose Level:", sd(data$blood_glucose_level), "\n")