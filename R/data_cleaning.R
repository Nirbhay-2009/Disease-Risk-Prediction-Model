# Read the raw dataset from your data folder
raw_data <- read.csv("data/diabetes.csv")

# Filter out missing categorical values like 'No Info' to keep the data clean
cleaned_data <- subset(raw_data, smoking_history != "No Info")

# Convert categorical character variables to Factors for statistical modeling
cleaned_data$gender <- as.factor(cleaned_data$gender)
cleaned_data$smoking_history <- as.factor(cleaned_data$smoking_history)

# Save the polished data file back to the folder for downstream scripts
write.csv(cleaned_data, "data/diabetes_cleaned.csv", row.names = FALSE)