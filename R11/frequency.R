# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Frequency Analysis

cat("----- GENDER FREQUENCY -----\n")
print(table(data$gender))
cat("\n")


cat("----- HYPERTENSION FREQUENCY -----\n")
print(table(data$hypertension))
cat("\n")


cat("----- HEART DISEASE FREQUENCY -----\n")
print(table(data$heart_disease))
cat("\n")


cat("----- SMOKING HISTORY FREQUENCY -----\n")
print(table(data$smoking_history))
cat("\n")


cat("----- DIABETES FREQUENCY -----\n")
print(table(data$diabetes))
cat("\n")


# Diabetes frequency by gender
cat("----- DIABETES BY GENDER -----\n")
print(table(data$gender, data$diabetes))