install.packages(c("cluster", "ggplot2", "dplyr"))
library(cluster)
library(ggplot2)
library(dplyr)

# Read the data and set the working directory
setwd("C:/Users/Surface/Documents/Machine Learning 1")
data <- read.csv("deliverytime.csv")

# Data Preparation
features <- data[, c("Restaurant_latitude", "Restaurant_longitude")]

# Handle outliers for further process
handle_outliers <- function(data, threshold = 3) {
  z_scores <- scale(data)
  outliers <- which(abs(z_scores) > threshold, arr.ind = TRUE)

  data <- data[-outliers[,1], ]
  
  return(data)
}

# Handle outliers in the features
features_no_outliers <- handle_outliers(features)

k <- 3

# Run k-means on data without outliers
kmeans_result <- kmeans(features_no_outliers, centers = k)

# Evaluation (CRISP-DM)
print("Cluster Centers:")
print(kmeans_result$centers)

# Display cluster assignments 
print("Cluster Assignments:")
print(kmeans_result$cluster)

# Boxplot to visualize the distribution 
ggplot(data = as.data.frame(features_no_outliers), aes(x = factor(kmeans_result$cluster), y = Restaurant_latitude, fill = factor(kmeans_result$cluster))) +
  geom_boxplot() +
  labs(title = "Distribution of Restaurant Latitude by Cluster", x = "Cluster", y = "Restaurant Latitude") +
  theme_minimal()

# Scatterplot Matrix to visualize relationships between variables
pairs(features_no_outliers, col = kmeans_result$cluster, pch = 16, main = "Scatterplot Matrix with Cluster Colors")


