# Load necessary libraries
library(e1071)   # For SVR
library(caret)   # For data partitioning
library(ggplot2) # For visualization

# Load the dataset
titanic_data <- read.csv("Titanic_Cleaned.csv")

# Select relevant columns
titanic_data <- titanic_data[, c("Survived", "Pclass", "Age")]

# Handle missing values (remove rows with NA)
titanic_data <- na.omit(titanic_data)

# Split the dataset into training (80%) and testing (20%) sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(titanic_data$Survived, p = 0.8, list = FALSE)
train_data <- titanic_data[trainIndex, ]
test_data <- titanic_data[-trainIndex, ]

# Train SVR model
svr_model <- svm(Survived ~ Pclass + Age, data = train_data, type = "eps-regression", kernel = "radial")

# Make predictions
predictions <- predict(svr_model, test_data)

# Evaluate model performance
mse <- mean((test_data$Survived - predictions)^2)
cat("Mean Squared Error:", mse, "\n")

# Combine actual and predicted data
results <- data.frame(Actual = test_data$Survived, Predicted = predictions, Age = test_data$Age, Pclass = test_data$Pclass)

# Scatter plot: Actual vs. Predicted survival values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Survival (SVR)",
       x = "Actual Survival",
       y = "Predicted Survival") +
  theme_minimal()

# Regression plot: Age vs Predicted survival
ggplot(results, aes(x = Age, y = Predicted, color = as.factor(Pclass))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  labs(title = "Age vs Predicted Survival (SVR)",
       x = "Age",
       y = "Predicted Survival",
       color = "Pclass") +
  theme_minimal()
