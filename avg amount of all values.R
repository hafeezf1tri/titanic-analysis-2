# ============================================
# Titanic Dataset: Calculate and Print Average Values
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

# ============================================
# 3. Apply Value Ranges (Filtering)
# ============================================
# Limit Age from 1 to 80
titanic_data <- subset(titanic_data, Age >= 1 & Age <= 80)

# Limit Fare from 1 to 600
titanic_data <- subset(titanic_data, Fare >= 1 & Fare <= 600)

# ============================================
# 4. Calculate Averages of Variables
# ============================================
cat("\n=================================\n")
cat("  Average Values of Variables\n")
cat("=================================\n")

average_age <- mean(titanic_data$Age, na.rm = TRUE)
average_fare <- mean(titanic_data$Fare, na.rm = TRUE)
average_pclass <- mean(as.numeric(titanic_data$Pclass), na.rm = TRUE)
average_sex <- mean(as.numeric(titanic_data$Sex == "female"), na.rm = TRUE) # Female = 1, Male = 0
average_sibsp <- mean(titanic_data$SibSp, na.rm = TRUE)
average_prch <- mean(titanic_data$PrCh, na.rm = TRUE)

cat("Average Age: ", round(average_age, 2), "\n")
cat("Average Fare: ", round(average_fare, 2), "\n")
cat("Average Pclass: ", round(average_pclass, 2), "\n")
cat("Average Sex (Female = 1, Male = 0): ", round(average_sex, 2), "\n")
cat("Average Number of Siblings/Spouses (SibSp): ", round(average_sibsp, 2), "\n")
cat("Average Number of Parents/Children (PrCh): ", round(average_prch, 2), "\n")

# ============================================
# END OF SCRIPT
# ============================================
