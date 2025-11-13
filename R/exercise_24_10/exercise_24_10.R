# ---------------------------------------------------------
# Exercise on R basic commands: "Auto" Dataset Analysis
# ---------------------------------------------------------

# a. Load the Auto dataset using read.table or read.csv. 

getwd()   # Check current working directory 
dir()     # List files in the directory to ensure the file exists
setwd("/home/emanueletocci/Documents/GitHub/unisa-data_science/R/exercise_24_10") # Set working directory

# IMPORTING DATA
# CRITICAL: We use na.strings = "?" because this specific dataset uses "?" 
# to represent missing values. Without this, numerical columns might be read as text.
data_csv <- read.csv("Auto.csv", header = TRUE, na.strings = "?") 

# Alternative using read.table (useful for .data files or specific separators)
data_tab <- read.table("Auto.data", sep = "", header = TRUE, na.strings = "?") 

data <- data_csv # Renaming for convenience


# UTILITY FUNCTIONS FOR DATA INSPECTION
dim(data)        # Returns dimensions (Rows, Columns)
str(data)        # distincts structure (data types like int, num, factor)
summary(data)    # Provides summary statistics (Min, Max, Mean, Quartiles)
head(data)       # Previews the first 6 rows of the dataset

# ---------------------------------------------------------
# b. Verify missing data (NA) and check contents
# ---------------------------------------------------------

summary(data)    # Quick check: look for "NA's" in the output
anyNA(data)      # Returns TRUE if there is at least one NA in the dataset
sum(is.na(data)) # Counts the total number of missing values

# Data Cleaning: Remove rows containing missing values to avoid errors in plotting/modeling
data <- na.omit(data) 

# ---------------------------------------------------------
# c. Scatter plot: MPG vs Cylinders
# ---------------------------------------------------------

# Plotting relationship between discrete variable (cylinders) and continuous variable (mpg)
# Syntax: plot(x_axis, y_axis)
plot(data$cylinders, data$mpg) 

# ---------------------------------------------------------
# d. Histogram and Probability Density Function of MPG
# ---------------------------------------------------------

# Basic histogram to see the distribution of Miles Per Gallon
hist(data$mpg) 

# Histogram with increased granularity (15 bins)
hist(data$mpg, breaks = 15)        

# NORMALIZED HISTOGRAM + DENSITY CURVE
# We must use freq = FALSE to plot "density" on the Y-axis instead of "count".
# This is necessary to overlay the density line (which has an area of 1).
hist(data$mpg, breaks = 15, freq = FALSE, main = "MPG Distribution with Density Curve")

# Overlay the estimated probability density function (smooth curve)
lines(density(data$mpg), lwd = 2, col = "red")

# ---------------------------------------------------------
# e. Boxplot for MPG
# ---------------------------------------------------------

# Single variable boxplot (visualizes median, quartiles, and outliers)
boxplot(data$mpg, main = "Boxplot of MPG") 

# Grouped Boxplot: MPG distribution grouped by Number of Cylinders
# Syntax: variable_to_plot ~ grouping_variable
boxplot(mpg ~ cylinders, data = data, col = "lightblue", main = "MPG by Cylinders") 

# ---------------------------------------------------------
# f. Pairwise Scatter Plots
# ---------------------------------------------------------

# The pairs() function requires numeric data. 
# We dynamically filter the dataset to keep only numeric columns.
colonne_numeriche <- sapply(data, is.numeric)
pairs(data[colonne_numeriche], main = "Pairwise Scatter Plots (All Numeric)") 

# Alternative: Plotting specific features of interest manually using formula notation
pairs(~mpg + displacement + horsepower + weight + acceleration, data = data, main = "Selected Features Pairs")