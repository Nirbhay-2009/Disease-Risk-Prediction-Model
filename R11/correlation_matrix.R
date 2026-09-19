# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Correlation Matrix

cat("----- CORRELATION MATRIX -----\n")

cor_matrix <- cor(data[, c("age",
                           "bmi",
                           "HbA1c_level",
                           "blood_glucose_level")])

print(cor_matrix)


# Scatter Plot

plot(data$HbA1c_level,
     data$blood_glucose_level,
     main = "HbA1c Level vs Blood Glucose Level",
     xlab = "HbA1c Level",
     ylab = "Blood Glucose Level",
     pch = 19)