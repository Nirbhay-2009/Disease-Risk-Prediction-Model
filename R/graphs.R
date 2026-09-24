install.packages("ggplot2")
library(ggplot2)

data <- read.csv("data/diabetes.csv")

head(data)

# GRAPH 1: DIABETES DISTRIBUTION

diabetes_count <- table(data$diabetes)

diabetes_df <- data.frame(
  diabetes = names(diabetes_count),
  count = as.numeric(diabetes_count)
)

diabetes_df$diabetes <- factor(
  diabetes_df$diabetes,
  levels = c("0", "1"),
  labels = c("No Diabetes", "Diabetes")
)

ggplot(diabetes_df, aes(x = diabetes, y = count)) +
  geom_bar(stat = "identity") +
  
  # Display values above the bars
  geom_text(
    aes(label = count),
    vjust = -0.3,
    size = 5,
    fontface = "bold"
  ) +
  
  labs(
    title = "Diabetes Distribution",
    x = "Diabetes Status",
    y = "Number of Patients"
  ) +
  
  theme_minimal()

ggsave(
  "graphs/diabetes_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# 2. Gender Distribution

gender_count <- table(data$gender)

gender_df <- data.frame(
  gender = names(gender_count),
  count = as.numeric(gender_count)
)

ggplot(gender_df, aes(x = gender, y = count)) +
  geom_bar(
    stat = "identity",
    fill = c("#E69F00", "#0072B2")
  ) +
  geom_text(
    aes(label = count),
    vjust = -0.3,
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Gender Distribution",
    x = "Gender",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/gender_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# 3. Hypertension Distribution

hypertension_count <- table(data$hypertension)

hypertension_df <- data.frame(
  hypertension = names(hypertension_count),
  count = as.numeric(hypertension_count)
)

hypertension_df$hypertension <- factor(
  hypertension_df$hypertension,
  levels = c("0", "1"),
  labels = c("No Hypertension", "Hypertension")
)

ggplot(hypertension_df, aes(x = hypertension, y = count)) +
  geom_bar(
    stat = "identity",
    fill = "#7B61A8"
  ) +
  geom_text(
    aes(label = count),
    vjust = -0.3,
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Hypertension Distribution",
    x = "Hypertension Status",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/hypertension_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 4: HEART DISEASE DISTRIBUTION

heart_count <- table(data$heart_disease)

heart_df <- data.frame(
  heart_disease = names(heart_count),
  count = as.numeric(heart_count)
)

heart_df$heart_disease <- factor(
  heart_df$heart_disease,
  levels = c("0", "1"),
  labels = c("No Heart Disease", "Heart Disease")
)

ggplot(heart_df, aes(x = heart_disease, y = count)) +
  geom_bar(stat = "identity", fill = "#D55E00") +
  geom_text(aes(label = count), vjust = -0.3, size = 5, fontface = "bold") +
  labs(
    title = "Heart Disease Distribution",
    x = "Heart Disease Status",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/heart_disease_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 5: SMOKING HISTORY DISTRIBUTION

smoking_count <- table(data$smoking_history)

smoking_df <- data.frame(
  smoking_history = names(smoking_count),
  count = as.numeric(smoking_count)
)

ggplot(smoking_df, aes(x = smoking_history, y = count, fill = smoking_history)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.3, size = 4, fontface = "bold") +
  labs(
    title = "Smoking History Distribution",
    x = "Smoking History",
    y = "Number of Patients"
  ) +
  scale_fill_manual(
    values = c(
      "current" = "#E69F00",
      "ever" = "#56B4E9",
      "former" = "#009E73",
      "never" = "#F0E442",
      "No Info" = "#CC79A7",
      "not current" = "#0072B2"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  "graphs/smoking_history_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)


# GRAPH 6: AGE DISTRIBUTION

ggplot(data, aes(x = age)) +
  geom_histogram(
    binwidth = 5,
    fill = "#009E73",
    color = "black"
  ) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/age_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 7: BMI DISTRIBUTION

ggplot(data, aes(x = bmi)) +
  geom_histogram(
    binwidth = 2,
    fill = "#CC79A7",
    color = "black"
  ) +
  labs(
    title = "BMI Distribution",
    x = "BMI",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/bmi_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 8: HbA1c LEVEL DISTRIBUTION

ggplot(data, aes(x = HbA1c_level)) +
  geom_histogram(
    binwidth = 0.25,
    fill = "#56B4E9",
    color = "black"
  ) +
  labs(
    title = "HbA1c Level Distribution",
    x = "HbA1c Level",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/hba1c_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 9: BLOOD GLUCOSE LEVEL DISTRIBUTION

ggplot(data, aes(x = blood_glucose_level)) +
  geom_histogram(
    binwidth = 10,
    fill = "#F0E442",
    color = "black"
  ) +
  labs(
    title = "Blood Glucose Level Distribution",
    x = "Blood Glucose Level",
    y = "Number of Patients"
  ) +
  theme_minimal()

ggsave(
  "graphs/blood_glucose_distribution.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 10: HbA1c LEVEL VS BLOOD GLUCOSE LEVEL

ggplot(
  data,
  aes(x = HbA1c_level, y = blood_glucose_level)
) +
  geom_point(
    color = "#0072B2",
    alpha = 0.5
  ) +
  labs(
    title = "HbA1c Level vs Blood Glucose Level",
    x = "HbA1c Level",
    y = "Blood Glucose Level"
  ) +
  theme_minimal()

ggsave(
  "graphs/hba1c_vs_glucose.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 11: BMI VS BLOOD GLUCOSE LEVEL

ggplot(
  data,
  aes(x = bmi, y = blood_glucose_level)
) +
  geom_point(
    color = "#D55E00",
    alpha = 0.5
  ) +
  labs(
    title = "BMI vs Blood Glucose Level",
    x = "BMI",
    y = "Blood Glucose Level"
  ) +
  theme_minimal()

ggsave(
  "graphs/bmi_vs_glucose.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 12: AGE VS BLOOD GLUCOSE LEVEL

ggplot(
  data,
  aes(x = age, y = blood_glucose_level)
) +
  geom_point(
    color = "#7B61A8",
    alpha = 0.5
  ) +
  labs(
    title = "Age vs Blood Glucose Level",
    x = "Age",
    y = "Blood Glucose Level"
  ) +
  theme_minimal()

ggsave(
  "graphs/age_vs_glucose.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 13: AGE VS HbA1c LEVEL

ggplot(
  data,
  aes(x = age, y = HbA1c_level)
) +
  geom_point(
    color = "#E69F00",
    alpha = 0.5
  ) +
  labs(
    title = "Age vs HbA1c Level",
    x = "Age",
    y = "HbA1c Level"
  ) +
  theme_minimal()

ggsave(
  "graphs/age_vs_hba1c.png",
  width = 8,
  height = 6,
  dpi = 300
)

# GRAPH 14: 3D SCATTER PLOT
#  Age vs HbA1c Level vs Blood Glucose Level

library(plotly)

plot_3d <- plot_ly(
  data = data,
  x = ~age,
  y = ~HbA1c_level,
  z = ~blood_glucose_level,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    color = "#009E73",
    size = 3
  )
) %>%
  layout(
    title = "Age vs HbA1c Level vs Blood Glucose Level",
    scene = list(
      xaxis = list(title = "Age"),
      yaxis = list(title = "HbA1c Level"),
      zaxis = list(title = "Blood Glucose Level")
    )
  )

plot_3d

# Save the interactive graph as HTML
htmlwidgets::saveWidget(
  plot_3d,
  "graphs/age_hba1c_glucose_3d.html",
  selfcontained = TRUE
)