# Disease Risk Prediction Using Statistical Modelling
# Correlation Analysis

# Load dataset
data <- read.csv("data/diabetes.csv")

# Pearson correlation between Age and Blood Glucose
cor(data$age, data$blood_glucose_level)

# Pearson correlation between BMI and Blood Glucose
cor(data$bmi, data$blood_glucose_level)

# Pearson correlation between HbA1c and Blood Glucose
cor(data$HbA1c_level, data$blood_glucose_level)

# Pearson correlation between Age and BMI
cor(data$age, data$bmi)

# Correlation matrix
cor(data[, c("age",
             "bmi",
             "HbA1c_level",
             "blood_glucose_level")])

# Correlation of variables with diabetes
cor(data$age, data$diabetes)
cor(data$bmi, data$diabetes)
cor(data$HbA1c_level, data$diabetes)
cor(data$blood_glucose_level, data$diabetes)

cor.test(data$HbA1c_level, data$blood_glucose_level)
cor.test(data$bmi, data$blood_glucose_level)

cor(data$age, data$blood_glucose_level)
cor(data$bmi, data$blood_glucose_level)
cor(data$HbA1c_level, data$blood_glucose_level)
cor(data$age, data$bmi)

cor(data[, c("age",
             "bmi",
             "HbA1c_level",
             "blood_glucose_level")])

# Correlation matrix
cor_matrix <- cor(data[, c("age",
                           "bmi",
                           "HbA1c_level",
                           "blood_glucose_level")])

print(cor_matrix)

# Correlation with diabetes
cor(data$age, data$diabetes)
cor(data$bmi, data$diabetes)
cor(data$HbA1c_level, data$diabetes)
cor(data$blood_glucose_level, data$diabetes)

# Correlation significance tests
cor.test(data$age, data$diabetes)
cor.test(data$bmi, data$diabetes)
cor.test(data$HbA1c_level, data$diabetes)
cor.test(data$blood_glucose_level, data$diabetes)