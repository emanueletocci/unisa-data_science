# SETUP 
# install.packages("corrplot")
library(corrplot)

setwd("/Users/emanueletocci/Documents/GitHub/unisa-data_science")
dataset = read.csv("exercise_30_10.csv", header = TRUE, sep = ",")

n = nrow(dataset)

## Dataset Splitting
set.seed(42)
sample_indices = sample(1:n, size = 0.80 * n)

trainSet <- dataset[sample_indices, ]      # Create 80% training set
testSet <- dataset[-sample_indices, ]      # Create 20% test set

## a. Corr matrix
cor_matrix <- cor(trainSet)
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7)


# x1, x_3, x_2, x_7 are the best predictors

## b. Calculate regression model coefficients
# Autamtically calculate coefficients
model = lm(trainSet$y ~ ., data = trainSet)     # built coeff calcutation function
summary(model)
estimate_coeff = coef(model)                    # estimate coefficients
View(estimate_coeff)

# Manual calculating coefficients: β^=(XTX)−1XTY
X = as.matrix(cbind(1, trainSet[, grep("^x_", names(trainSet))]))   # picking (only) all predictors
Y_vec = as.matrix(trainSet$y)                                       # picking y column

manual_estimate_coeff = solve(t(X) %*% X) %*% t(X) %*% Y_vec
View(manual_estimate_coeff)

# Comparing manual_estimate_coeff and estimate_coeff
compare_coeff = data.frame(
  Coeff_Names = names(estimate_coeff),
  lm_coef = as.numeric(estimate_coeff),
  manual_coef = as.numeric(manual_estimate_coeff)
)
compare_coeff$diff = compare_coeff$lm_coef - compare_coeff$manual_coef
View(compare_coeff)    

# Calculating training mse
train_pred_auto = predict(model, newdata = trainSet)
mse_auto = mean((trainSet$y - train_pred_auto)^2)

## c. 