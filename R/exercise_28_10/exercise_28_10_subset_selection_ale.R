# Exercise on subset selection for linear regression

# In this exercise we use the Hitters dataset from the ISLR library. 

install.packages("ISLR") #installa il package
library(ISLR) #caricalo nel ambiente

# We wish to 
# predict a baseball player’s Salary on the basis of statistics associated 
# with the player's performance during the year.

# Remove from the dataset the features that are not numeric, as well as any 
# observation with NA values.

sum(is.na(Hitters)) #59 na
data_clean <- na.omit(Hitters) #leviamoli
sum(is.na(data_clean)) #controlliamo, ok tutto apposto
attach(data_clean)
head(data_clean)

# a. Split the dataset in a train part and a test part. Use 75% of the 
#    observations for training, and the remaining 25% for test. 

set.seed(1) #il seed serve per rendere il tutto riproducibile
n <- nrow(data_clean) #numero righe dataset
n_train <- floor(0.75 * n) # calcola il 75%, floor serve per avere un intero
train_index <- sample(seq_len(n), size = n_train) #Estrai casualmente gli indici delle righe per il training set
train <- data_clean[train_index, ]   # NAME[righe, colonne] colonne vuote quindi tutte. righe solo alcune date da un vettore
test  <- data_clean[-train_index, ]  # il - indica tutte le righe tranne quelle in train_index
#check rapido
dim(train)   # dovresti vedere ~0.75 * n righe
dim(test)    # dovresti vedere ~0.25 * n righe


# b. Compute the regression model, using the training set, relating the Salary 
#    variable to the available features. For this task apply best subset selection
#    se the "regsubsets" function, which is part of the leaps library. Check the
#    summary to understand how the features are selected.

library(leaps)
best_subset <- regsubsets(Salary ~ ., data = train) #ricerca i migliori subset e li salviamo 
summary(best_subset) #possiamo vedere i vari indicatori

# c. Choose the best subset of features using estimates of the test error based 
#    on C_p, AIC, BIC and adjusted R^2. Produce suitable plots showing the value
#    of the performance indicators vs the number of features. Additionally, use 
#    the builtin "plot.regsubsets" function and comment on the plot it produces. 

stats <- summary(best_subset) #il riassunto alla fine ha le statistiche dei vari subset
n_train_val <- n_train # Numero di osservazioni nel training set

# Calcoliamo l'AIC perchè non c'è una funzione prepronta
# p = numero di predittori (da 1 a 16) + 1 (per l'intercetta)
p_values <- 1:16 + 1 
aic_values <- n_train_val * log(stats$rss / n_train_val) + 2 * p_values


par(mfrow = c(2, 2)) #par() è la funzione che imposta i parametri grafici. mfrow è uno di questi parametri e sta per Multi-Frame. c(righe, colonne)


plot(stats$adjr2, main = "Selezione Modello: Adjusted R^2")
plot(stats$cp, main = "Selezione Modello: Mallows' C_p")
plot(stats$bic, main = "Selezione Modello: BIC")
plot(aic_values, main = "Selezione Modello: AIC")
par(mfrow = c(1, 1)) # Resettiamo la finestra grafica

# Numero di predittori ottimale per ciascun criterio
best_adjr2 <- which.max(stats$adjr2)
best_cp    <- which.min(stats$cp)
best_bic   <- which.min(stats$bic)
best_aic   <- which.min(aic_values)


# d. Compute the regression model using the forward stepwise selection. 
#    Compare the obtained model against the best subset selection one, verifying
#    if there are any differences in the two approaches for a given number of 
#    features (use the "coef" function to look at the coefficients of the model).
#    As in point c, choose the best subset using the test error estimates. 


forward_model <- regsubsets(Salary ~ ., data = train, method = "forward")
forward_stats <- summary(forward_model)

# Confronto coefficienti per 7 predittori 
coef(best_subset, 7) #coef(object, id)
coef(forward_model, 7) #	object: l’oggetto modello creato da regsubsets. id: il numero di predittori del modello di cui vuoi estrarre i coefficienti.
#coef() ti restituisce tutti i parametri del modello selezionato così puoi usarli per predizioni o confronti tra modelli.

# Calcolo AIC
p_values <- 1:length(forward_stats$rss) + 1
aic_forward <- n_train_val * log(forward_stats$rss / n_train_val) + 2 * p_values

par(mfrow = c(2, 2))
plot(forward_stats$adjr2, main = "Forward: Adjusted R^2")
plot(forward_stats$cp, main = "Forward: Cp")
plot(forward_stats$bic, main = "Forward: BIC")
plot(aic_forward, main = "Forward: AIC")
par(mfrow = c(1, 1))

# Numero di predittori ottimale per ciascun criterio
best_adjr2 <- which.max(forward_stats$adjr2)
best_cp    <- which.min(forward_stats$cp)
best_bic   <- which.min(forward_stats$bic)
best_aic   <- which.min(aic_forward)


# e. Compute the regression model using the step() function, which applies the 
#    BIC to choose the best subset (use "extractAIC" to compute the BIC of the 
#    selected model).

# e. Stepwise selection con BIC
full_model <- lm(Salary ~ ., data = train)  # modello completo
null_model <- lm(Salary ~ 1, data = train)  # modello con solo intercetta

# stepwise backward usando BIC
step_model <- step(full_model, direction = "both", k = log(n_train_val)) # k=log(n) -> BIC
coef(step_model) # Coefficienti del modello selezionato
extractAIC(step_model, k = log(n_train_val))[2] # BIC del modello selezionato

# f. For all the subsets computed using best subset selection and forward 
#    stepwise, compute the MSE on the test set. Check if the results are 
#    consistent with the ones from the analysis using the test MSE estimates.

#non ho la minima idea di cosa mi chiede, lo lascero come ultima cosa


# HOMEWORK: add to the comparison the backward stepwise method, and use also the function "stepAIC".
