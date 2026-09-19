data<- read.csv("data11/diabetes.csv")

head(data)

str(data)

dim(data)

names(data)

colSums(is.na(data))
sum(duplicated(data))

#checking values
table(data$hypertension)

table(data$diabetes)

table(data$heart_disease)

save(data, file = "data11/diabetes_data.RData")

summary(data)