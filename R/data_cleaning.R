# Disease Risk Prediction Using Statistical Modelling
# Data Cleaning and Validation


# --------------------------------------------------
# 1. Load Dataset
# --------------------------------------------------

data <- read.csv("data/diabetes.csv")


# --------------------------------------------------
# 2. Check Dataset Size
# --------------------------------------------------

dim(data)


# --------------------------------------------------
# 3. Check Data Types
# --------------------------------------------------

str(data)


# --------------------------------------------------
# 4. Check Missing Values
# --------------------------------------------------

colSums(is.na(data))


# --------------------------------------------------
# 5. Check Unique Values in Categorical Variables
# --------------------------------------------------

unique(data$gender)
unique(data$smoking_history)


# --------------------------------------------------
# 6. Check Diabetes Distribution
# --------------------------------------------------

table(data$diabetes)


# --------------------------------------------------
# 7. Summary of Numerical Variables
# --------------------------------------------------

summary(data)


# --------------------------------------------------
# 8. Check for Duplicate Records
# --------------------------------------------------

sum(duplicated(data))


# --------------------------------------------------
# 9. Check Range of Important Numerical Variables
# --------------------------------------------------

range(data$age)

range(data$bmi)

range(data$HbA1c_level)

range(data$blood_glucose_level)