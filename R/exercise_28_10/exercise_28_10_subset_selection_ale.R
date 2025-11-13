# Exercise on subset selection for linear regression

# In this exercise we use the Hitters dataset from the ISLR library. We wish to 
# predict a baseball player’s Salary on the basis of statistics associated 
# with the player's performance during the year.
# Remove from the dataset the features that are not numeric, as well as any 
# observation with NA values.

# a. Split the dataset in a train part and a test part. Use 75% of the 
#    observations for training, and the remaining 25% for test. 
# b. Compute the regression model, using the training set, relating the Salary 
#    variable to the available features. For this task apply best subset selection
#    se the "regsubsets" function, which is part of the leaps library. Check the
#    summary to understand how the features are selected.
# c. Choose the best subset of features using estimates of the test error based 
#    on C_p, AIC, BIC and adjusted R^2. Produce suitable plots showing the value
#    of the performance indicators vs the number of features. Additionally, use 
#    the builtin "plot.regsubsets" function and comment on the plot it produces. 
# d. Compute the regression model using the forward stepwise selection. 
#    Compare the obtained model against the best subset selection one, verifying
#    if there are any differences in the two approaches for a given number of 
#    features (use the "coef" function to look at the coefficients of the model).
#    As in point c, choose the best subset using the test error estimates. 
# e. Compute the regression model using the step() function, which applies the 
#    BIC to choose the best subset (use "extractAIC" to compute the BIC of the 
#    selected model).
# f. For all the subsets computed using best subset selection and forward 
#    stepwise, compute the MSE on the test set. Check if the results are 
#    consistent with the ones from the analysis using the test MSE estimates.

# HOMEWORK: add to the comparison the backward stepwise method, and use also the function "stepAIC".

# ------------------------------------------------------------------------------
# SETUP AND DATA CLEANING
# ------------------------------------------------------------------------------

# install.packages("ISLR") 
library(ISLR)  # Load the library containing the Hitters dataset
library(leaps) # Load the library for subset selection (regsubsets)

# Check for missing values
sum(is.na(Hitters)) # There are 59 NAs in the original dataset

# Remove rows with NA values
data_raw <- na.omit(Hitters) 

# The prompt explicitly asks to remove non-numeric features.
# We filter columns to keep only numeric ones (and the target 'Salary').
nums <- sapply(data_raw, is.numeric)
data_clean <- data_raw[, nums]

# Quick check of the cleaned data
dim(data_clean)
head(data_clean)

# ------------------------------------------------------------------------------
# a. Dataset Splitting (Train/Test)
# ------------------------------------------------------------------------------

set.seed(1) # Set seed for reproducibility of the random split
n <- nrow(data_clean) 

# Calculate 75% of the sample size
n_train <- floor(0.75 * n) 

# Randomly sample indices for the training set
train_index <- sample(seq_len(n), size = n_train) 

train <- data_clean[train_index, ]   # Training set (75%)
test  <- data_clean[-train_index, ]  # Test set (25%)

# Verify dimensions
dim(train)
dim(test)

# ------------------------------------------------------------------------------
# b. Best Subset Selection (Training Set)
# ------------------------------------------------------------------------------

# We use regsubsets to find the best model for each number of variables.
# IMPORTANT: nvmax must be set high enough to include all predictors (default is 8).
# Here we have approx 16 predictors after removing non-numeric cols.
best_subset <- regsubsets(Salary ~ ., data = train, nvmax = 16) 

# The summary object contains the metrics (RSS, R2, Cp, BIC) for each model size
stats <- summary(best_subset) 
# print(stats)

# ------------------------------------------------------------------------------
# c. Model Selection Plots (Cp, AIC, BIC, Adj-R2)
# ------------------------------------------------------------------------------

n_train_val <- nrow(train)

# AIC is not provided by regsubsets, so we calculate it manually.
# Formula: AIC ~ n * log(RSS/n) + 2 * p
p_values <- 1:16 + 1 # Number of predictors + 1 for intercept
aic_values <- n_train_val * log(stats$rss / n_train_val) + 2 * p_values

# Setup plotting area (2x2 grid)
par(mfrow = c(2, 2)) 

# Plot 1: Adjusted R-squared (Higher is better)
plot(stats$adjr2, xlab = "No. of Variables", ylab = "Adjusted R2", type = "l", main = "Best Subset: Adj R2")
best_adjr2 <- which.max(stats$adjr2)
points(best_adjr2, stats$adjr2[best_adjr2], col = "red", cex = 2, pch = 20)

# Plot 2: Mallows' Cp (Lower is better)
plot(stats$cp, xlab = "No. of Variables", ylab = "Cp", type = "l", main = "Best Subset: Cp")
best_cp <- which.min(stats$cp)
points(best_cp, stats$cp[best_cp], col = "red", cex = 2, pch = 20)

# Plot 3: BIC (Lower is better)
plot(stats$bic, xlab = "No. of Variables", ylab = "BIC", type = "l", main = "Best Subset: BIC")
best_bic <- which.min(stats$bic)
points(best_bic, stats$bic[best_bic], col = "red", cex = 2, pch = 20)

# Plot 4: AIC (Lower is better)
plot(aic_values, xlab = "No. of Variables", ylab = "AIC", type = "l", main = "Best Subset: AIC")
best_aic <- which.min(aic_values)
points(best_aic, aic_values[best_aic], col = "red", cex = 2, pch = 20)

par(mfrow = c(1, 1)) # Reset plotting area

# Built-in plot for regsubsets (visualizes selected variables)
# Black squares indicate which variables are included in the best model for a given stat.
plot(best_subset, scale = "bic", main = "Variables Selected (ordered by BIC)")

# ------------------------------------------------------------------------------
# d. Forward Stepwise Selection
# ------------------------------------------------------------------------------

# Apply forward selection using method = "forward"
forward_model <- regsubsets(Salary ~ ., data = train, method = "forward", nvmax = 16)
forward_stats <- summary(forward_model)

# Compare coefficients for a specific model size (e.g., 7 predictors)
# This checks if Forward Selection missed the optimal combination found by Best Subset.
print("Coefficients for Best Subset (Size 7):")
coef(best_subset, 7) 

print("Coefficients for Forward Selection (Size 7):")
coef(forward_model, 7) 

# ------------------------------------------------------------------------------
# e. Stepwise selection using step() (BIC based)
# ------------------------------------------------------------------------------

# Define full and null models for the step function
full_model <- lm(Salary ~ ., data = train)  
null_model <- lm(Salary ~ 1, data = train)  

# Perform stepwise selection. 
# k = log(n) implies we are using BIC as the criterion.
step_model_bic <- step(full_model, direction = "both", k = log(n_train_val), trace = 0) # trace=0 hides output

print("Coefficients selected by step() with BIC:")
coef(step_model_bic)

# Calculate the BIC value for the final model
bic_val <- extractAIC(step_model_bic, k = log(n_train_val))[2]
print(paste("BIC of the selected model:", bic_val))

# ------------------------------------------------------------------------------
# f. Compute Test MSE for all subsets (The "Custom" Loop)
# ------------------------------------------------------------------------------

# regsubsets() does not have a native predict() function. 
# We must manually calculate predictions for each model size (1 to 16).

# 1. Create the model matrix from the TEST data (transforms data for matrix multiplication)
test_mat <- model.matrix(Salary ~ ., data = test)

# 2. Initialize a vector to store MSE errors for each size
val_errors <- rep(NA, 16)

# 3. Loop through each subset size (i = 1 to 16)
for (i in 1:16) {
  # Extract coefficients for the best model of size 'i'
  coefi <- coef(best_subset, id = i)
  
  # Select only the columns in test_mat that correspond to these coefficients
  pred <- test_mat[, names(coefi)] %*% coefi
  
  # Calculate Mean Squared Error
  val_errors[i] <- mean((test$Salary - pred)^2)
}

# Plot Test MSE
plot(val_errors, type = "b", xlab = "Number of Variables", ylab = "Test MSE", 
     main = "Test MSE vs Model Size (Best Subset)")
points(which.min(val_errors), min(val_errors), col = "red", pch = 19)

print(paste("Lowest Test MSE is at size:", which.min(val_errors)))

# ------------------------------------------------------------------------------
# HOMEWORK: Backward Stepwise & stepAIC
# ------------------------------------------------------------------------------

# 1. Backward Stepwise Selection
# ------------------------------
backward_model <- regsubsets(Salary ~ ., data = train, method = "backward", nvmax = 16)
bwd_summary <- summary(backward_model)

# Quick visual check of BIC for Backward
plot(bwd_summary$bic, type="b", main="Backward Selection: BIC")

# 2. Using stepAIC (from MASS library)
# ------------------------------------
library(MASS)

# stepAIC defaults to k=2 (which is AIC).
# direction = "both" tries adding and removing variables.
step_aic_model <- stepAIC(full_model, direction = "both", trace = 0)

print("Model selected by stepAIC:")
summary(step_aic_model)

# Compare number of variables selected:
print(paste("Variables in BIC model (step):", length(coef(step_model_bic))))
print(paste("Variables in AIC model (stepAIC):", length(coef(step_aic_model))))

# Note: AIC usually selects larger models (more variables) than BIC because 
# BIC penalizes model complexity more heavily.