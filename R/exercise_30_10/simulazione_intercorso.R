# Esercitazione riassuntiva regressione lineare 

# Si consideri il dataset "exercise_30_10.csv", disponibile sulla piattaforma di 
# e-learning, contenente 200 osservazioni della variabile indipendente Y e di 12 
# feature X_1, X_2, ..., X_12. Nel dataset non sono presenti osservazioni con
# valori NA. Si vuole determinare un modello di regressione lineare che leghi Y 
# alle feature. Dividere il dataset in una parte di train (80% delle osservazioni) 
# e una parte di test (20% delle osservazioni). 

# a. Analizzare la correlazione tra le variabili del dataset. In particolare,
#    commentare quali feature potrebbero essere più influenti ai fini del calcolo 
#    del modello di regressione. 
# b. Calcolare i coefficienti del modello di regressione, utilizzando tutte le 12
#    feature, usando sia le funzioni builtin di R che la relazione esplicita per
#    il calcolo dei coefficienti. Verificare che i risultati siano coerenti. 
#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori di Y predetti dal modello di regressione stimato.
# c. Effettuare una selezione delle feature applicando la backward stepwise 
#    selection, utilizzare C_p come metrica per scegliere il numero di feature.
#    Calcolare l'MSE di training del modello ottenuto, calcolando esplicitamente
#    i valori di Y predetti dal modello di regressione stimato.
#    Si commenti sulle feature scelte in relazione all'analisi di correlazione 
#    effettuata al punto a. 
# d. Calcolare l'MSE di test ottenuto con i due modelli di regressione calcolati
#    ai punti b e c. La differenza tra i due risultati è significativa? 
#    Commentare opportunamente il comportamento osservato.
# 
# Comprehensive Linear Regression Exercise

# Dataset: "exercise_30_10.csv" (200 observations, 12 features).
# Goal: Build a linear regression model to predict Y based on X_1...X_12.
#       Compare a full model against a feature-selected model (Backward Stepwise).

# ---------------------------------------------------------
# SETUP & LIBRARIES
# ---------------------------------------------------------

# Install necessary packages if not already present:
# install.packages("corrplot")
# install.packages("leaps")

library(corrplot) # For visualizing the correlation matrix
library(leaps)    # For stepwise regression (regsubsets)

# Set working directory (Adjust path as needed)
# setwd("/home/emanueletocci/Documents/GitHub/unisa-data_science/R/exercise_30_10")

# Load the dataset
dataset = read.csv("exercise_30_10.csv", header = TRUE, sep = ",")
n = nrow(dataset)

# ---------------------------------------------------------
# DATA SPLITTING
# ---------------------------------------------------------
# We split the data to evaluate the model's performance on unseen data (generalization).
set.seed(42) # Set seed for reproducibility
sample_indices = sample(1:n, size = 0.80 * n)

trainSet <- dataset[sample_indices, ]      # 80% Training set (used to fit the model)
testSet <- dataset[-sample_indices, ]      # 20% Test set (used to evaluate the model)

# ---------------------------------------------------------
# A. CORRELATION ANALYSIS
# ---------------------------------------------------------
# Analyze the linear relationship between variables. 
# We look for high correlation between features (Xi) and the target (Y).

cor_matrix <- cor(trainSet)
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)

# OBSERVATION:
# Look at the 'y' row/column. Features with high absolute values (close to 1 or -1)
# are likely to be the most significant predictors (e.g., x_1, x_2, x_3, x_7).

# ---------------------------------------------------------
# B. FULL MODEL COEFFICIENTS (Auto vs Manual)
# ---------------------------------------------------------

# 1. Automatic calculation using R's built-in lm() function
model = lm(trainSet$y ~ ., data = trainSet)
summary(model)
estimate_coeff = coef(model)
# View(estimate_coeff)

# 2. Manual calculation using OLS Matrix Formula: β = (X^T X)^-1 X^T Y
# Construct the Design Matrix X (adding a column of 1s for the Intercept)
X = as.matrix(cbind(1, trainSet[, grep("^x_", names(trainSet))])) 
Y_vec = as.matrix(trainSet$y)

# Matrix operations
manual_estimate_coeff = solve(t(X) %*% X) %*% t(X) %*% Y_vec
# View(manual_estimate_coeff)

# 3. Verification: Compare built-in vs manual coefficients
compare_coeff = data.frame(
  Coeff_Names = names(estimate_coeff),
  lm_coef = as.numeric(estimate_coeff),
  manual_coef = as.numeric(manual_estimate_coeff)
)
# Calculate the difference (should be effectively zero)
compare_coeff$diff = compare_coeff$lm_coef - compare_coeff$manual_coef
View(compare_coeff)    

# 4. Calculate Training MSE (Mean Squared Error) for the Full Model
train_pred_auto = predict(model, newdata = trainSet)
mse_auto = mean((trainSet$y - train_pred_auto)^2)

# ---------------------------------------------------------
# C. BACKWARD STEPWISE SELECTION (Metric: Mallows' Cp)
# ---------------------------------------------------------
# We use backward selection to remove irrelevant features.
# Mallows' Cp estimates the bias-variance tradeoff; lower values are better.

# Perform backward selection (nvmax=12 allows checking all subset sizes)
regfit_bwd = regsubsets(y ~ ., data = trainSet, nvmax = 12, method = "backward")
reg_summary = summary(regfit_bwd)

# Plot Cp values to identify the optimal number of variables
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b", pch = 19, main = "Mallows' Cp Selection")

# Identify the index (number of variables) with the minimum Cp
best_model_idx = which.min(reg_summary$cp)
points(best_model_idx, reg_summary$cp[best_model_idx], col = "red", cex = 2, pch = 20)

print(paste("Optimal number of variables according to Cp:", best_model_idx))

# Extract the coefficients for the optimal subset
coef(regfit_bwd, best_model_idx)

# --- Refitting the Reduced Model ---
# 'regsubsets' does not have a standard 'predict' method. 
# To calculate MSE easily, we create a new 'lm' object using only the selected features.

# 1. Extract selected variable names (excluding intercept)
vars_selected = names(coef(regfit_bwd, best_model_idx))
vars_selected = vars_selected[vars_selected != "(Intercept)"] 

# 2. Construct the formula dynamically
formula_str = paste("y ~", paste(vars_selected, collapse = " + "))
print(paste("Formula for the Reduced Model:", formula_str))

# 3. Fit the new 'best' model using lm()
best_model_lm = lm(as.formula(formula_str), data = trainSet)
summary(best_model_lm)

# 4. Calculate Training MSE for the Reduced Model
train_pred_bwd = predict(best_model_lm, newdata = trainSet)
mse_train_bwd = mean((trainSet$y - train_pred_bwd)^2)

# COMPARE TRAINING ERRORS:
print(paste("MSE Training (Full Model):", mse_auto))
print(paste("MSE Training (Reduced Model):", mse_train_bwd))

# COMMENTARY ON SELECTION:
# Compare 'vars_selected' with the correlation matrix in step (A).
# The stepwise process likely kept features with high correlation and removed 
# "noisy" features with low correlation to Y.

# ---------------------------------------------------------
# D. TEST MSE & FINAL COMPARISON
# ---------------------------------------------------------
# We evaluate both models on the Test Set to check for overfitting.

# 1. Test MSE for Full Model (12 variables)
test_pred_full = predict(model, newdata = testSet)
mse_test_full = mean((testSet$y - test_pred_full)^2)

# 2. Test MSE for Reduced Model (Selected variables)
test_pred_red = predict(best_model_lm, newdata = testSet)
mse_test_red = mean((testSet$y - test_pred_red)^2)

# Create a summary table
results_compare = data.frame(
  Model = c("Full Model (12 vars)", "Reduced Model (Stepwise)"),
  Train_MSE = c(mse_auto, mse_train_bwd),
  Test_MSE = c(mse_test_full, mse_test_red)
)

print(results_compare)

# ---------------------------------------------------------
# FINAL ANALYSIS:
# 1. Train MSE: The Full Model usually has a slightly lower Train MSE because 
#    it uses more variables to "memorize" the training data (potential overfitting).
#
# 2. Test MSE: If the Reduced Model has a lower (or very similar) Test MSE 
#    compared to the Full Model, the selection was successful. It means we 
#    removed noise without losing predictive power, resulting in a simpler 
#    and more robust model.
# ---------------------------------------------------------