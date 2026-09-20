#statistics
hist(data$age,
     main = "Distribution of Age",
     xlab = "Age",
     ylab = "Frequency")

hist(data$bmi,
     main = "Distribution of BMI",
     xlab = "BMI",
     ylab = "Frequency")

hist(data$HbA1c_level,
     main = "Distribution of HbA1c Level",
     xlab = "HbA1c Level",
     ylab = "Frequency")

hist(data$blood_glucose_level,
     main = "Distribution of Blood Glucose Level",
     xlab = "Blood Glucose Level",
     ylab = "Frequency")

barplot(table(data$diabetes),
        main = "Diabetes Distribution",
        xlab = "Diabetes Status",
        ylab = "Number of Patients")

barplot(table(data$gender),
        main = "Gender Distribution",
        xlab = "Gender",
        ylab = "Number of Patients")

barplot(table(data$smoking_history),
        main = "Smoking History Distribution",
        xlab = "Smoking History",
        ylab = "Number of Patients",
        las = 2)

barplot(table(data$hypertension),
        main = "Hypertension Distribution",
        xlab = "Hypertension Status",
        ylab = "Number of Patients")

barplot(table(data$heart_disease),
        main = "Heart Disease Distribution",
        xlab = "Heart Disease Status",
        ylab = "Number of Patients")

plot(data$age, data$blood_glucose_level,
     main = "Age vs Blood Glucose Level",
     xlab = "Age",
     ylab = "Blood Glucose Level")

plot(data$bmi, data$blood_glucose_level,
     main = "BMI vs Blood Glucose Level",
     xlab = "BMI",
     ylab = "Blood Glucose Level")

plot(data$HbA1c_level, data$blood_glucose_level,
     main = "HbA1c Level vs Blood Glucose Level",
     xlab = "HbA1c Level",
     ylab = "Blood Glucose Level")

boxplot(bmi ~ diabetes,
        data = data,
        main = "BMI by Diabetes Status",
        xlab = "Diabetes Status",
        ylab = "BMI")

boxplot(blood_glucose_level ~ diabetes,
        data = data,
        main = "Blood Glucose Level by Diabetes Status",
        xlab = "Diabetes Status",
        ylab = "Blood Glucose Level")

barplot(table(data$smoking_history, data$diabetes),
        beside = TRUE,
        main = "Diabetes Status by Smoking History",
        xlab = "Smoking History",
        ylab = "Number of Patients",
        legend = TRUE,
        las = 2)

barplot(table(data$gender, data$diabetes),
        beside = TRUE,
        main = "Diabetes Status by Gender",
        xlab = "Gender",
        ylab = "Number of Patients",
        legend = TRUE)

