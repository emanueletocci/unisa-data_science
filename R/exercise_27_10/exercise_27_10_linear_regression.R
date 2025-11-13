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


# Exercise on linear regression with R

# In this exercise you will estimate the coefficients of a linear regression model by using 
# the "Boston Housing dataset". You can have access to this dataset after loading the library MASS
# (look for the "Boston" object).

# install.packages("corrplot")
# install.packages("scatterplot3d")

library(MASS) #Carica la libreria MASS
data("Boston") #Carica il dataset Boston, ora lo possiamo usare usando "Boston"
head(Boston) #Visualizza le prime righe
str(Boston) #Controlla la struttura del dataset
summary(Boston) #Riassunto rapido

# For this exercise, focus only on the first ten columns, plus
# the last one (that is called medv), which will represent our response variable "y".

# CORREZIONE: Ho spostato la creazione di 'ds' PRIMA dell'attach, altrimenti dava errore "object not found"
ds <- Boston[,c(1:10,14)] #sintassi NOMEDATASET[RIGHE,COLONNE] per selezionare. inoltre c(A,B) seleziona A e B oppure come nel nostro caso da 1 a 10 e 14
attach(ds) #usiamo i nomi delle colonne direttamente

head(ds) #check al volo
sum(is.na(ds)) #controlla quanti na ci sono. in questo caso non ce ne sono

# a. Compute the correlation coefficient among the dataset features, and comment on the degree of 
#    correlation between medv and the other features. Use the functions "cor" and "corrplot". 
#    Check also the pairwise scatter plots, computed over the columns of the dataset, and comment 
#    on their shape in terms of the corresponding correlation coefficients.

cor_matrix <- cor(ds) #matrice di correlazione
cor_matrix #check al volo

library(corrplot) #carichiamo corrplot se no non lo possiamo usare
corrplot(cor_matrix, method = "color", addCoef.col = "black", number.cex = 0.7) #visualizzazione grafica della matrice di correlazione

cor(medv, ds) #correlazione diretta tra le 10 colonne e medv (basta anche solo iniziare l'esercizio da qui)
pairs(ds[,1:11]) #vediamo le coppie di correlazione tra tutte le colonne (considera solo con medv)


# b. Find the coefficients of a simple linear regression using the "lm" function. Use medv as the
#    response variable and nox as the feature. Read and comment on the summary information relative 
#    to the the obtained model (use "summary"). Produce the scatter plot relating nox to medv, and 
#    the corresponding regression line obtained with lm. 

model <- lm(medv ~ nox, data = ds) #fitta il modello. sintassi LABEL ~ FEATURE.
summary(model)
plot(nox, medv) #fai il plot che chiede
abline(model) #aggiungi una linea al plot corrente, normalmente passi inclinazione e punto d'intersezione
#ma nel nostro caso gli passiamo il modello di regrerssione lineare stimato
#la linea indica il valore stimato da model

# c. Solve a multiple linear regression problem using three freatures: nox, crim, and rm.
#    Compute the regression coefficients using the formulas seen during the lectures, and compare 
#    the result with the one from lm. Comment on the obtained model.
#    NOTE: be careful with the shape of your dataset when using the lecture formulas!

model_multi <- lm(medv ~ nox + crim + rm, data = ds)
summary(model_multi)

Mx = as.matrix(cbind(1, ds$nox, ds$crim, ds$rm))  #matrice dei regressori, creo una matrice unendo più colonne insieme, aggiungo la colonna di 1 per l'intercetta
Vy = as.matrix(ds$medv)  #vettore della variabile dipendente

# formula matriciale dei minimi quadrati: (X'X)^(-1) X'Y
beta= solve(t(Mx) %*% Mx) %*% t(Mx) %*% Vy
beta #mostro i coefficienti calcolati

# CORREZIONE: qui avevi scritto coef(mod), ma l'oggetto si chiama 'model_multi'
confronto = cbind(beta_manuale = as.vector(beta), beta_lm = coef(model_multi))
confronto


# d. Remove one of the features from point c, solve again the regression problem and plot the 
#    regression plane computed with lm. Check the documentation on "scatterplot3d" for help.

# Nota: se il pacchetto è già installato, puoi commentare la riga qui sotto
# install.packages("scatterplot3d") 
library(scatterplot3d)
model_3d <- lm(medv ~ nox + rm, data = ds)
summary(model_3d)

plot3d <- scatterplot3d(ds$nox, ds$rm, ds$medv) #plot 3d con le tre variabili
plot3d$plane3d(model_3d) #aggiungi il piano derivante dal modello multi feature trovato