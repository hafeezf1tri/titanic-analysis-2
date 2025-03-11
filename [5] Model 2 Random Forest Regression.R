packages <- c("randomForest", "caret")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the necessary libraries
library(randomForest)  # For Random Forest Regression
library(caret)  # For dataset splitting and evaluation

titanic_data <- read.csv("Titanic_Cleaned.csv")

# Remove unnecessary column (Passenger No.)
titanic_data$No. <- NULL  

# Convert categorical variable 'Sex' to factor (if not already encoded)
titanic_data$Sex <- as.factor(titanic_data$Sex)

# Define dependent (y) and independent (X) variables
X <- titanic_data[, !(names(titanic_data) %in% c("Survived"))]  # Features
y <- titanic_data$Survived  # Target variable

#Step 3: Split Dataset into Train (80%), Test (10%), Validation (10%)
set.seed(42)  # For reproducibility

# Create train index (80%)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- titanic_data[trainIndex, ]
temp_data <- titanic_data[-trainIndex, ]

# Split remaining 20% into test (10%) and validation (10%)
testIndex <- createDataPartition(temp_data$Survived, p = 0.5, list = FALSE)
test_data <- temp_data[testIndex, ]
validation_data <- temp_data[-testIndex, ]

#Step 4: Train a Random Forest Regression Model
set.seed(42)  # Ensure reproducibility
rf_model <- randomForest(Survived ~ ., data = train_data, ntree = 500, importance = TRUE)

#Step 5: Evaluate Model Performance
# Predict on test set
rf_preds <- predict(rf_model, newdata = test_data)

# Convert continuous predictions to binary (0 or 1) for classification-like evaluation
rf_class_preds <- ifelse(rf_preds > 0.5, 1, 0)

# Calculate Mean Squared Error (MSE)
mse <- mean((rf_preds - test_data$Survived)^2)
print(paste("Mean Squared Error:", mse))

# Calculate R-squared (coefficient of determination)
ss_total <- sum((test_data$Survived - mean(test_data$Survived))^2)
ss_residual <- sum((test_data$Survived - rf_preds)^2)
r2 <- 1 - (ss_residual / ss_total)
print(paste("R-squared:", r2))

#Step 6: Validate the Model
# Predict on validation set
val_preds <- predict(rf_model, newdata = validation_data)

# Convert to binary for evaluation
val_class_preds <- ifelse(val_preds > 0.5, 1, 0)

# Calculate validation accuracy
val_acc <- mean(val_class_preds == validation_data$Survived)
print(paste("Validation Accuracy:", val_acc))

#Step 7: Feature Importance
# Show which features contribute the most to predictions
importance(rf_model)
varImpPlot(rf_model)  # Plot feature importance
