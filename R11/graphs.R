# Load the diabetes dataset
data <- read.csv("data11/diabetes.csv")


# Create graphs folder if it does not exist
dir.create("graphs11", showWarnings = FALSE)


# 1. Diabetes Distribution

png("graphs11/diabetes_distribution.png",
    width = 1000, height = 700)

diabetes_count <- table(data$diabetes)

bar_positions <- barplot(
  diabetes_count,
  main = "Diabetes Distribution",
  xlab = "Diabetes",
  ylab = "Number of Records",
  names.arg = c("No Diabetes", "Diabetes"),
  ylim = c(0, 5500),
  col = "lightblue",
  border = "black",
  las = 1,
  cex.main = 1.5,
  cex.lab = 1.2,
  cex.axis = 1.1
)

# Display values above bars
text(
  x = bar_positions,
  y = diabetes_count,
  labels = diabetes_count,
  pos = 3,
  cex = 1.2,
  font = 2
)

dev.off()
# 2. HbA1c Level vs Blood Glucose Level

png("graphs11/hba1c_vs_glucose.png",
    width = 800, height = 600)

plot(
  data$HbA1c_level,
  data$blood_glucose_level,
  main = "HbA1c Level vs Blood Glucose Level",
  xlab = "HbA1c Level",
  ylab = "Blood Glucose Level",
  pch = 19
)

dev.off()


# 3. Age vs Diabetes

png("graphs11/age_vs_diabetes.png",
    width = 800, height = 600)

boxplot(
  age ~ diabetes,
  data = data,
  main = "Age Distribution by Diabetes",
  xlab = "Diabetes",
  ylab = "Age",
  names = c("No Diabetes", "Diabetes")
)

dev.off()


cat("All graphs created successfully.\n")