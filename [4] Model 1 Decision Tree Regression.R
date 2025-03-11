packages <- c("rpart", "caret")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(rpart)  # For Decision Tree Regression
library(caret)  # For dataset splitting and evaluation

# Load the dataset
titanic_data <- read.csv("Titanic_Cleaned.csv")

# Remove unnecessary column (Passenger No.)
titanic_data$No. <- NULL  

titanic_data$Sex <- as.factor(titanic_data$Sex)

# Define dependent (y) and independent (X) variables
X <- titanic_data[, !(names(titanic_data) %in% c("Survived"))]  # Features
y <- titanic_data$Survived  # Target variable

set.seed(42)  # For reproducibility

# Create train index (80%)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- titanic_data[trainIndex, ]
temp_data <- titanic_data[-trainIndex, ]

# Split remaining 20% into test (10%) and validation (10%)
testIndex <- createDataPartition(temp_data$Survived, p = 0.5, list = FALSE)
test_data <- temp_data[testIndex, ]
validation_data <- temp_data[-testIndex, ]

# Train Decision Tree Regression model
dt_model <- rpart(Survived ~ ., data = train_data, method = "anova")

# Plot the tree (optional)
plot(dt_model)
text(dt_model, pretty = 0)

# Predict on test set
dt_preds <- predict(dt_model, newdata = test_data)

# Convert continuous predictions to binary (0 or 1) for classification-style comparison
dt_class_preds <- ifelse(dt_preds > 0.5, 1, 0)

# Calculate Mean Squared Error (MSE)
mse <- mean((dt_preds - test_data$Survived)^2)
print(paste("Mean Squared Error:", mse))

# Calculate R-squared (coefficient of determination)
ss_total <- sum((test_data$Survived - mean(test_data$Survived))^2)
ss_residual <- sum((test_data$Survived - dt_preds)^2)
r2 <- 1 - (ss_residual / ss_total)
print(paste("R-squared:", r2))

# Predict on validation set
val_preds <- predict(dt_model, newdata = validation_data)

# Convert to binary for evaluation
val_class_preds <- ifelse(val_preds > 0.5, 1, 0)

# Calculate validation accuracy
val_acc <- mean(val_class_preds == validation_data$Survived)
print(paste("Validation Accuracy:", val_acc))



