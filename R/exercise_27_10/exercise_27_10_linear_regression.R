# Exercise on linear regression with R

# In this exercise you will estimate the coefficients of a linear regression model by using 
# the "Boston Housing dataset". You can have access to this dataset after loading the library MASS
# (look for the "Boston" object). For this exercise, focus only on the first ten columns, plus
# the last one (that is called medv), which will represent our response variable "y".

# a. Compute the correlation coefficient among the dataset features, and comment on the degree of 
#    correlation between medv and the other features. Use the functions "cor" and "corrplot". 
#    Check also the pairwise scatter plots, computed over the columns of the dataset, and comment 
#    on their shape in terms of the corresponding correlation coefficients.
# b. Find the coefficients of a simple linear regression using the "lm" function. Use medv as the
#    response variable and nox as the feature. Read and comment on the summary information relative 
#    to the the obtained model (use "summary"). Produce the scatter plot relating nox to medv, and 
#    the corresponding regression line obtained with lm. 
# c. Solve a multiple linear regression problem using three freatures: nox, crim, and rm.
#    Compute the regression coefficients using the formulas seen during the lectures, and compare 
#    the result with the one from lm. Comment on the obtained model.
#    NOTE: be careful with the shape of your dataset when using the lecture formulas!
# d. Remove one of the features from point c, solve again the regression problem and plot the 
#    regression plane computed with lm. Check the documentation on "scatterplot3d" for help.

# ---------------------------------------------------------
# Exercise on Linear Regression with R: Boston Housing Dataset
# ---------------------------------------------------------

# OBJECTIVE: 
# Estimate coefficients of a linear regression model using the "Boston" dataset.
# Focus: First 10 features + 'medv' (Median Value of owner-occupied homes) as the target.

# ---------------------------------------------------------
# 1. SETUP & DATA LOADING
# ---------------------------------------------------------

# install.packages("corrplot")
# install.packages("scatterplot3d")

library(MASS)   # Loads the library containing the 'Boston' dataset
data("Boston")  # Loads the dataset into the environment
head(Boston)    # Preview the first few rows
str(Boston)     # Inspect data structure (variable types)
summary(Boston) # Statistical summary (Mean, Median, Quartiles)

# ---------------------------------------------------------
# Data Preparation
# ---------------------------------------------------------

# We create a subset 'ds' containing only the first 10 columns (predictors) 
# and the 14th column ('medv', the response variable).
ds <- Boston[, c(1:10, 14)] 

# attach() allows us to access columns simply by name (e.g., 'medv') 
# instead of using 'ds$medv'. 
# NOTE: Use with caution in larger projects to avoid namespace conflicts.
attach(ds) 

# Data Integrity Check
head(ds)        # Verify the subset creation
sum(is.na(ds))  # Count missing values (Result is 0, so no cleaning needed)

# ---------------------------------------------------------
# a. Correlation Analysis
# ---------------------------------------------------------
# Goal: Identify which features are strongly related to 'medv'.

cor_matrix <- cor(ds) # Compute the correlation matrix for all variables

library(corrplot) 
# Visualizing the correlation matrix.
# 'addCoef.col' adds numeric values to the plot for precision.
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7) 

# Direct correlation check: How 'medv' correlates with all other variables
cor(medv, ds) 

# Pairwise Scatter Plots
# Visualizes the relationship between variables. We look for linear patterns.
pairs(ds[, 1:11], main = "Pairwise Scatter Plots") 

# ---------------------------------------------------------
# b. Simple Linear Regression (medv ~ nox)
# ---------------------------------------------------------
# Goal: Model 'medv' using only 'nox' (Nitrogen oxides concentration).

# Fit the linear model using Ordinary Least Squares (OLS).
# Syntax: lm(response ~ predictor, data = dataframe)
model <- lm(medv ~ nox, data = ds) 

# Summary provides: Coefficients (Intercept, Slope), R-squared, p-values (Significance)
summary(model)

# Visualization
plot(nox, medv, main = "Regression: MEDV vs NOX", pch = 19, col = "gray") 
abline(model, col = "red", lwd = 2) # Adds the regression line defined by the model

# ---------------------------------------------------------
# c. Multiple Linear Regression & Manual Calculation
# ---------------------------------------------------------
# Goal: Model 'medv' using three features: 'nox', 'crim' (crime rate), 'rm' (rooms).
# Comparison: Built-in lm() vs. Manual Matrix Calculation.

# 1. Automatic calculation with lm()
model_multi <- lm(medv ~ nox + crim + rm, data = ds)
summary(model_multi)

# 2. Manual calculation using Matrix Algebra: Beta = (X^T X)^-1 X^T Y

# Construct the Design Matrix (X)
# We combine a column of 1s (for the intercept) with the predictor columns.
Mx = as.matrix(cbind(1, ds$nox, ds$crim, ds$rm)) 

# Construct the Response Vector (Y)
Vy = as.matrix(ds$medv) 

# Apply the OLS Matrix Formula
# t(Mx) = Transpose of X
# solve(...) = Inverse of the matrix
# %*% = Matrix multiplication
beta = solve(t(Mx) %*% Mx) %*% t(Mx) %*% Vy

# Display calculated coefficients
beta 

# 3. Comparison
# We create a dataframe to side-by-side compare manual results vs lm() results.
confronto = cbind(beta_manuale = as.vector(beta), beta_lm = coef(model_multi))
print(confronto) # The values should be identical

# ---------------------------------------------------------
# d. 3D Visualization (Removing 'crim')
# ---------------------------------------------------------
# Goal: Visualize the regression plane using 2 predictors ('nox' and 'rm').

# install.packages("scatterplot3d") 
library(scatterplot3d)

# Fit the model with 2 predictors
model_3d <- lm(medv ~ nox + rm, data = ds)
summary(model_3d)

# Create the 3D Scatter Plot
# x = nox, y = rm, z = medv (response)
plot3d <- scatterplot3d(ds$nox, ds$rm, ds$medv, 
                        main = "3D Regression Plane",
                        xlab = "NOX", ylab = "RM", zlab = "MEDV",
                        pch = 19, color = "blue")

# Overlay the regression plane estimated by the model
plot3d$plane3d(model_3d, col = "red", lty = "dotted")