# Exercise on R basic commands

# a. Load the Auto dataset using read.table or read.csv. 
# b. Verify that there are no missing data or NA data in the dataset (you can use the 
#    "na" methods). Check the contents of the dataset in terms of its features (use the "summary" function). 
# c. Produce a scatter plot showing the miles per gallon (mpg feature of the dataset)
#    as a function of the the number of cylinders (cylinders feature).
# d. Show the histogram of the miles per gallon and the estimated probability density 
#    function of the miles per gallon (use the functions "hist" and "density"). 
# e. Produce a boxplot for the miles per gallon feature.
# f. Use the "pairs" command to plot pairwise scatter plots among the dataset features.
#    Comment on the linear relation observable from the scatter plots.

# Exercise on R basic commands

# a. Load the Auto dataset using read.table or read.csv. 

getwd()   #return the current directory 
dir()  #show the files in the working directory
setwd("/home/emanueletocci/Documents/GitHub/unisa-data_science/R/new/exercise_24_10") # 1. imposta la working directory per importare il dataset

# CORREZIONE QUI: Aggiunto na.strings = "?" per gestire i dati mancanti correttamente e rendere le colonne numeriche
data_csv <- read.csv("Auto.csv", header = TRUE, na.strings = "?") # 2. importa il dataset usa il comando read.csv o .table se il formato del ds è .data
data_tab <- read.table("Auto.data", sep = "", header = TRUE, na.strings = "?") #header true se il ds ha un header. sep indica il separatore se il formato è .data

data <- data_csv # per comodità userò il csv e lo rinomino in data


#funzioni di utility
dim(data)        # Numero di righe e colonne
str(data)        # Struttura e tipi di variabili
summary(data)    # Statistiche base
head(data)       # possiamo controllare le prime righe

# b. Verify that there are no missing data or NA data in the dataset (you can use the 
#    "na" methods). Check the contents of the dataset in terms of its features (use the "summary" function). 

summary(data)
anyNA(data) #true se ci sono degli na
sum(is.na(data)) #li conta
data <- na.omit(data) #li elimina

# c. Produce a scatter plot showing the miles per gallon (mpg feature of the dataset)
#    as a function of the the number of cylinders (cylinders feature).

plot(data$cylinders, data$mpg) #notazione NOMEDATASET$NOMECOLONNA

# d. Show the histogram of the miles per gallon and the estimated probability density 
#    function of the miles per gallon (use the functions "hist" and "density"). 

hist(data$mpg) #crea un histogram di mpg
hist(data$mpg, breaks = 15)        # Istogramma con 15 "barre"


# Istogramma normalizzato (area = 1)
hist(data$mpg, breaks = 15, freq = FALSE) #per fare la sovrapposizione devo normalizzare con freq=false
# Sovrapporre la curva di densità
lines(density(data$mpg), lwd = 2, col = "red")

# e. Produce a boxplot for the miles per gallon feature.

boxplot(data$mpg, data=data) # solo una colonna
boxplot(mpg ~ cylinders, data = data) #più colonne

# f. Use the "pairs" command to plot pairwise scatter plots among the dataset features.
#    Comment on the linear relation observable from the scatter plots.

pairs(data[sapply(data, is.numeric)]) # uso solo colonne numeriche per evitare errore
pairs(~mpg+displacement+horsepower+weight+acceleration, data=data)