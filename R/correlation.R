library(ggplot2)

# Load data (Assuming Member 1 saved it here)
df <- read.csv("data/diabetes_cleaned.csv")

# 1. Compute Coefficients
pearson_coef  <- cor(df$age, df$blood_glucose_level, method = "pearson")
spearman_coef <- cor(df$age, df$blood_glucose_level, method = "spearman")

print("===== ASSOCIATIONS MATRIX =====")
print(paste("Pearson Coefficient (r):", round(pearson_coef, 4)))
print(paste("Spearman Rank Coefficient (rho):", round(spearman_coef, 4)))

# 2. Export Graph to graphs/ folder
png("graphs/02_correlation_scatterplot.png", width = 800, height = 600)
ggplot(df, aes(x = age, y = blood_glucose_level)) +
  geom_point(color = "#34495e", alpha = 0.4, size = 2) +
  labs(title = "Age vs Blood Glucose Association", x = "Age", y = "Blood Glucose Level") +
  theme_minimal()
dev.off()
