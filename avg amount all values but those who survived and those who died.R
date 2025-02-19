# ============================================
# Titanic Dataset: Calculate and Print Average Values for Survivors and Non-Survivors
# ============================================

# Install required packages if not already installed
if (!require(dplyr)) install.packages("dplyr")

# Load libraries
library(dplyr)

# ============================================
# 1. Load the Dataset
# ============================================
# Ensure that Titanic_processed.csv is in your working directory
titanic_data <- read.csv("Titanic_processed.csv")

# ============================================
# 2. Data Preprocessing
# ============================================
# Remove 'No.' column if it exists
if ("No." %in% colnames(titanic_data)) {
  titanic_data <- titanic_data %>% select(-No.)
}

# Convert categorical variables to factors
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Survived <- as.factor(titanic_data$Survived)

# ============================================
# 3. Apply Value Ranges (Filtering)
# ============================================
# Limit Age from 1 to 80
titanic_data <- subset(titanic_data, Age >= 1 & Age <= 80)

# Limit Fare from 1 to 600
titanic_data <- subset(titanic_data, Fare >= 1 & Fare <= 600)

# ============================================
# 4. Function to Calculate Averages
# ============================================
calculate_averages <- function(data) {
  list(
    Average_Age = round(mean(data$Age, na.rm = TRUE), 2),
    Average_Fare = round(mean(data$Fare, na.rm = TRUE), 2),
    Average_Pclass = round(mean(as.numeric(data$Pclass), na.rm = TRUE), 2),
    Average_Sex = round(mean(as.numeric(data$Sex == "female"), na.rm = TRUE), 2), # Female = 1, Male = 0
    Average_SibSp = round(mean(data$SibSp, na.rm = TRUE), 2),
    Average_PrCh = round(mean(data$PrCh, na.rm = TRUE), 2)
  )
}

# ============================================
# 5. Calculate and Print Averages for Each Group
# ============================================
cat("\n=================================\n")
cat("   Average Values of Variables\n")
cat("=================================\n")

# For Survivors
cat("\n--- Survivors ---\n")
survivors <- titanic_data %>% filter(Survived == 1)
averages_survivors <- calculate_averages(survivors)
print(averages_survivors)

# For Non-Survivors
cat("\n--- Non-Survivors ---\n")
non_survivors <- titanic_data %>% filter(Survived == 0)
averages_non_survivors <- calculate_averages(non_survivors)
print(averages_non_survivors)

# ============================================
# END OF SCRIPT
# ============================================
