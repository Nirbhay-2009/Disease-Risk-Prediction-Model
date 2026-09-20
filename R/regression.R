library(ggplot2)

df <- read.csv("data/diabetes_cleaned.csv")

# Fit the line using Least Squares: y = a + bx
regression_model <- lm(blood_glucose_level ~ bmi, data = df)

intercept_a <- coef(regression_model)[1]
slope_b     <- coef(regression_model)[2]

print("===== REGRESSION LINE MODEL =====")
print(paste("Equation: Glucose = ", round(intercept_a, 2), " + (", round(slope_b, 2), " * BMI)"))

# Export Graph to graphs/ folder
png("graphs/03_regression_line.png", width = 800, height = 600)
ggplot(df, aes(x = bmi, y = blood_glucose_level)) +
  geom_point(color = "#7f8c8d", alpha = 0.4) +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
  labs(title = "Linear Regression Model Curve Fitting", x = "BMI", y = "Blood Glucose Level") +
  theme_minimal()
dev.off()
