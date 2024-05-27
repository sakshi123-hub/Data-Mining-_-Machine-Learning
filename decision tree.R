install.packages(c("rpart", "ggplot2"))
library(rpart)
library(ggplot2)

# Read the data and the set working directory
setwd("C:/Users/Surface/Documents/Machine Learning 1")
data <- read.csv("mortality_life_expectancy.csv")

# Summary Statistics for further evaluation
str(data)
summary(data)

# Extraction process
predictors <- data[, c("infant_mortality", "infant_mortality_male")]
target <- data$infant_mortality_female

# Fit the decision tree model
decision_tree_model <- rpart(infant_mortality_female ~ infant_mortality + infant_mortality_male, data = data)

# Creating data frame
new_data <- data.frame(infant_mortality = c(3.76, 3.61), infant_mortality_male = c(4.14, 3.93))
predictions <- predict(decision_tree_model, new_data)

# Print Model with details
printcp(decision_tree_model)
print(new_data)
print(predictions)

# Actual and predicted values
actual_values <- data$infant_mortality_female
predicted_values <- predict(decision_tree_model)

# Calculate Z-scores
z_scores <- scale(actual_values)

# Define threshold
outlier_threshold <- 3
outliers <- abs(z_scores) > outlier_threshold

# Handle Outliers
data$infant_mortality_female[outliers] <- NA

# Evaluation

ggplot(data, aes(x = actual_values, y = predicted_values)) +
  geom_point(aes(color = ifelse(outliers, "Outlier", "Not Outlier")), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Actual vs. Predicted Values with Outliers", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Distribution of z-scores
ggplot(data, aes(x = z_scores)) +
  geom_histogram(fill = "blue", bins = 30, color = "white") +
  labs(title = "Distribution of Z-scores", x = "Z-score") +
  theme_minimal()

# Boxplot without outliers
boxplot(data$infant_mortality_female, main = "Actual Values Without Outliers", outline = FALSE)


