# install.packages("ISLR")
# install.packages("leaps")

library(ISLR)                  # Load the library
View(Hitters)
dim(Hitters)                   # Dataset Size

CleanedHitters <- na.omit(Hitters)    # Removing all the rows that have missing values in any variables
CleanedHitters <- CleanedHitters[, sapply(CleanedHitters, is.numeric)] # Remove all non numeric columns

View(CleanedHitters)
dim(CleanedHitters)

# a. Manually splitting the dataset in 75%-25% subsets
set.seed(101)                 # Set Seed to randomically generates numbers

# Crea un vettore con indici casuali per il 75% delle righe
sample_indices <- sample.int(n = nrow(CleanedHitters), size = floor(0.75 * nrow(CleanedHitters)), replace = FALSE)

trainSet <- CleanedHitters[sample_indices, ]      # Create 75% training set
testSet <- CleanedHitters[-sample_indices, ]      # Create 25% test set

dim(trainSet)                  # check train dim
dim(testSet)                   # check test dim

# b. Computing regression model
 library(leaps)
 bsm = regsubsets(Salary ~ ., data = trainSet, nvmax = 16)
 bsm_summary = summary(bsm)

# c. 
par(mfrow=c(2,2))
par(mfrow=c(2,2))

# RSS - minimo
plot(bsm_summary$rss, xlab="Number of predictors", ylab="RSS", type="b", main="RSS")
min_rss <- which.min(bsm_summary$rss)
points(min_rss, bsm_summary$rss[min_rss], col="red", cex=2, pch=19)

# CP - minimo
plot(bsm_summary$cp, xlab="Number of predictors", ylab="CP", type="b", main="CP")
min_cp <- which.min(bsm_summary$cp)
points(min_cp, bsm_summary$cp[min_cp], col="red", cex=2, pch=19)

# BIC - minimo
plot(bsm_summary$bic, xlab="Number of predictors", ylab="BIC", type="b", main="BIC")
min_bic <- which.min(bsm_summary$bic)
points(min_bic, bsm_summary$bic[min_bic], col="red", cex=2, pch=19)

# Adjusted R2 - massimo
plot(bsm_summary$adjr2, xlab="Number of predictors", ylab="Adjusted R2", type="b", main="Adjusted R2")
max_adjr2 <- which.max(bsm_summary$adjr2)
points(max_adjr2, bsm_summary$adjr2[max_adjr2], col="red", cex=2, pch=19)

