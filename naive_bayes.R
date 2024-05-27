library(readr)
library(e1071)
library(caret)

# Read the data
setwd("C:/Users/Surface/Documents/Machine Learning 1")
data <- read.csv("Diwali_Sales_Data.csv")

# Display summary statistics 
summary(data)
str(data)

# 3. Data Preparation
predictors <- data[, c("User_ID", "Product_ID", "Age")]
outliers <- function(x, threshold = 3) {
  z <- scale(x)
  return(which(abs(z) > threshold))
}

outliers_age <- outliers(data$Age)
cat("Outliers in Age column:", outliers_age, "\n")
data <- data[-outliers_age, ]

# Data Modeling

set.seed(22219340)
sample_size <- floor(0.7 * nrow(data))
train_indices <- sample(nrow(data), size = sample_size)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build the model after handling outliers
model <- naiveBayes(Zone ~ ., data = train_data)
print(model)

# Make predictions
predictions <- predict(model, newdata = test_data)
print(predictions)

# 5. Evaluation
confusion_matrix <- table(Actual = test_data$Zone, Predicted = predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy: ", accuracy, "\n")
print(confusion_matrix)

#  exploration and evaluation
par(mfrow = c(2, 2))

# Boxplot before handling outliers
boxplot(data$Age, main = "Boxplot - Before Outlier Handling", col = "lightblue")

# Boxplot after handling outliers
boxplot(train_data$Age, main = "Boxplot - After Outlier Handling", col = "lightgreen")

# Confusion matrix plot
heatmap(confusion_matrix, col = terrain.colors(7), 
        main = "Confusion Matrix Heatmap", 
        xlab = "Predicted", ylab = "Actual")

# Histogram of Age
hist(data$Age, main = "Histogram of Age", xlab = "Age", col = "lightblue", border = "black")

# Bar plot of Zone
barplot(table(data$Zone), main = "Bar Plot of Zone", col = "lightgreen", border = "black")

# Density plot of Age
plot(density(data$Age), main = "Density Plot of Age", col = "purple", lwd = 2)

