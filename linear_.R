install.packages(c("readr", "ggplot2"))
library(readr)
library(ggplot2)

# Read the data
setwd("C:/Users/Surface/Documents/Machine Learning 1")
data <- read.csv("Diwali_Sales_Data.csv")

# Data Preparation
model_data <- data[, c("User_ID", "Product_ID")]

# Convert categorical variables into factor
model_data$Product_ID <- as.factor(model_data$Product_ID)

# Build a linear model
model <- lm(User_ID ~ Product_ID, data = model_data)

# Display model summary
print("Model Summary:")
summary(model)

# Visualize a model
ggplot(data = model_data, aes(x = fitted(model), y = User_ID)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatterplot of Observed vs. Predicted Values", x = "Predicted User_ID", y = "Observed User_ID") +
  theme_minimal()

# Boxplot of user_id by product_id
ggplot(data = model_data, aes(x = Product_ID, y = User_ID)) +
  geom_boxplot() +
  labs(title = "Boxplot of User_ID by Product_ID", x = "Product_ID", y = "User_ID") +
  theme_minimal()


