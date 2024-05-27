library(class)
library(caret)

# Read the data
setwd("C:/Users/Surface/Documents/Machine Learning 1")
data <- read.csv("mortality_life_expectancy.csv")
str(data)  
summary(data)  

# Data Preparation (CRISP-DM)
data <- na.omit(data)
predictor_variable <- data$infant_mortality_male
target_variable <- data$infant_mortality_female

# Data Transformation (CRISP-DM)
degree <- 2
poly_terms <- poly(predictor_variable, degree)
polynomial_model <- lm(target_variable ~ poly_terms)

# Get model summary
summary(polynomial_model)

# Calculate residuals
residuals <- residuals(polynomial_model)

# Calculate Z-scores
z_scores <- scale(residuals)

# Set a threshold 
outlier_threshold <- 2
outliers <- which(abs(z_scores) > outlier_threshold)

# Visualize Box Plot of Residuals
boxplot(residuals, main = "Box Plot of Residuals", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)  # A red dashed line at y = 0

# Mark outliers in red
points(outliers, residuals[outliers], col = "red", pch = 16)

# Handle outliers 
clean_data <- data[-outliers, ]

# Step 5: Modeling (CRISP-DM)
clean_predictor_variable <- clean_data$infant_mortality_male
clean_target_variable <- clean_data$infant_mortality_female

clean_poly_terms <- poly(clean_predictor_variable, degree)
clean_polynomial_model <- lm(clean_target_variable ~ clean_poly_terms)

# Visualize the new model fit
plot(clean_predictor_variable, clean_target_variable, pch = 16, col = "blue", main = "Cleaned Data and Model Fit")
lines(sort(clean_predictor_variable), predict(clean_polynomial_model, newdata = data.frame(clean_predictor_variable = sort(clean_predictor_variable))), col = "blue", lwd = 2)

# Example: Scatter plot with regression line
plot(clean_predictor_variable, clean_target_variable, pch = 16, col = "blue", main = "Scatter Plot with Regression Line")

# Residuals vs. Fitted Values Plot
plot(clean_polynomial_model, which = 1, col = "blue", pch = 16, main = "Residuals vs. Fitted Values Plot")

# QQ Plot
qqnorm(clean_polynomial_model$residuals, col = "blue", main = "QQ Plot of Residuals")
qqline(clean_polynomial_model$residuals, col = "red")
