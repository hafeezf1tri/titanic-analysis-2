# Define required packages
packages <- c("randomForest", "caret", "ggplot2", "dplyr")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the required libraries
library(randomForest)  # Random Forest Regression
library(caret)   # Data splitting and preprocessing
library(ggplot2) # Visualization
library(dplyr)   # Data manipulation

# Load dataset (Ensure Titanic_Cleaned.csv is in the working directory)
titanic_data <- read.csv("Titanic_Cleaned.csv")

# Convert Pclass into an ordered factor with proper labels
titanic_data$Pclass <- factor(titanic_data$Pclass, levels = c(1, 2, 3), 
                              labels = c("1st Class", "2nd Class", "3rd Class"))

# Convert Sex into numeric
titanic_data$Sex <- ifelse(titanic_data$Sex == "male", 1, 0)  # Male = 1, Female = 0

# Remove unnecessary columns (if any)
titanic_data$No. <- NULL  

# Remove rows with missing Age values
titanic_data <- titanic_data[!is.na(titanic_data$Age), ]

# Store original Age values before scaling (for later use in visualization)
titanic_data$Original_Age <- titanic_data$Age  

# Normalize features (except Pclass) for consistency in training
titanic_data$Age <- scale(titanic_data$Age)  # Standardize Age

# Define independent (X) and dependent (y) variables
X <- titanic_data[, c("Pclass", "Sex", "Age")]  # Features
y <- titanic_data$Survived  # Target variable

# Combine features, target variable, and original Age for later use
titanic_data_prepared <- cbind(X, Survived = y, Original_Age = titanic_data$Original_Age)

set.seed(42)  # For reproducibility

# Create train index (80%)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
train_data <- titanic_data_prepared[trainIndex, ]
test_data <- titanic_data_prepared[-trainIndex, ]

# Train a Random Forest model to predict survival probability
set.seed(42)
rf_model <- randomForest(Survived ~ Pclass + Sex + Age, data = train_data, ntree = 500, mtry = 3, importance = TRUE)

# Print model summary
print(rf_model)

# Predict survival probability on test data
rf_preds <- predict(rf_model, newdata = test_data)

# Cap predictions between 0 and 1 (Survival probability must be valid)
rf_preds <- pmax(pmin(rf_preds, 1.0), 0.0)

# Print first few predictions
print(head(rf_preds))

# Create a dataframe for visualization with original Age values
scatter_data <- data.frame(Age = test_data$Original_Age, Pclass = test_data$Pclass, PredictedSurvival = rf_preds)

# Scatter plot: Age vs. Predicted Survival Probability (Colored by Pclass)
ggplot(scatter_data, aes(x = Age, y = PredictedSurvival, color = Pclass)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Predicted Survival Probability Based on Age and Pclass (Random Forest)",
       x = "Age (Years)",
       y = "Predicted Survival Probability",
       color = "Passenger Class") +
  scale_color_manual(values = c("red", "blue", "green")) +  # Proper class colors
  scale_y_continuous(limits = c(0, 1)) +  # Cap Y-axis at 1.0
  scale_x_continuous(breaks = seq(floor(min(scatter_data$Age)), ceiling(max(scatter_data$Age)), by = 5))  # Proper Age Labels



