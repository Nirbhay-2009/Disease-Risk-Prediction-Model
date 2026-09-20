# Check dataset size
dim(data)

# Check data types
str(data)

# Check missing values
colSums(is.na(data))

# Check unique values in categorical columns
unique(data$gender)
unique(data$smoking_history)

# Check diabetes distribution
table(data$diabetes)

# Summary of numerical variables
summary(data)

sum(duplicated(data))

range(data$age)
range(data$bmi)
range(data$HbA1c_level)
range(data$blood_glucose_level)