getwd()
setwd('/Users/aicha/Desktop/2CS/2CS-TP/S2/TP SChrono/TP4')
library(readxl)
data<- read_excel('data1.xlsx')
args(read_excel)
head(data)
serie<-na.omit(data$X0) 
print(serie)

#1 lissage exp  simple
lissage_exponentiel_simple <- function(serie, alpha) {
  n <- length(serie)
  lissage <- numeric(n)
  lissage[1] <- serie[1] # Initialisation avec la première valeur de la série
  
  for (t in 2:n) {
    lissage[t] <- alpha * serie[t] + (1 - alpha) * lissage[t - 1]
  }
  
  return(lissage)
}
alpha <- 0.3
result <- lissage_exponentiel_simple(serie, alpha)
print(result)
#Comparaison
plot(1:length(serie), serie, type = "o", col = "blue", xlab = "Temps", ylab = "Valeur", main = "Lissage Exponentiel Simple")
lines(1:length(result), result, type = "o", col = "red")
legend("topright", legend = c("Original", "Lissé"), col = c("blue", "red"), lty = 1, pch = 1)


#2lissage exponentiel double 

lissage_exponentiel_double <- function(serie, alpha) {
  n <- length(serie)
  lissage_simple <- lissage_exponentiel_simple(serie, alpha)
  lissage_double <- lissage_exponentiel_simple(lissage_simple, alpha)
  x_prediction <- 2 * lissage_simple - lissage_double
  return(x_prediction)
}
alpha <- 0.3
result_2 <- lissage_exponentiel_double(serie, alpha)
print(result_2)
#Comparaison
plot(1:length(serie), serie, type = "o", col = "blue", xlab = "Temps", ylab = "Valeur", main = "Lissage Exponentiel double")
lines(1:length(result_2), result_2, type = "o", col = "red")
legend("topright", legend = c("Original", "Lissé"), col = c("blue", "red"), lty = 1, pch = 1)

#3 Données 
X <- c(23.1, 26.5, 27.2, 24.5, 25.1, 23.6, 24.9, 26.8, 26.2, 24.5, 25.4, 27.3, 26.5, 23.5, 25.1)
Y <- c(12.9, 13.5, 14.1, 14.3, 14.7, 14.8, 16.2, 15.1, 15.8, 17.2, 15.9, 16.3, 17.3, 17.9, 18.9, 18.2, 18.5)

#4 Représentation graphique des séries
plot(X, type = "o", col = "blue", pch = 16, ylim = range(c(X, Y)),
     xlab = "Temps", ylab = "Valeur", main = "Séries Chronologiques X et Y")
lines(Y, type = "o", col = "red", pch = 16)
# Ajouter une légende
legend("topright", legend = c("Série X", "Série Y"), col = c("blue", "red"), lty = 1, pch = 16)

#5 lissage simple pour x et double pour y
#serie x
alpha <- 0.3
lissage1 <- lissage_exponentiel_simple(X, alpha)
print(lissage1)
plot(1:length(X), X, type = "o", col = "blue", xlab = "Temps", ylab = "Valeur", main = "Lissage Exponentiel Simple pour la serie X")
lines(1:length(lissage1),lissage1, type = "o", col = "red")
legend("topright", legend = c("Original", "Lissé"), col = c("blue", "red"), lty = 1, pch = 1)
#serie Y
alpha <- 0.3
lissage2 <- lissage_exponentiel_double(Y, alpha)
print(lissage2)
plot(1:length(Y),Y, type = "o", col = "blue", xlab = "Temps", ylab = "Valeur", main = 'Lissage Exponentiel double pour la serie Y')
lines(1:length(lissage2),lissage2, type = "o", col = "red")
legend("topright", legend = c("Original", "Lissé"), col = c("blue", "red"), lty = 1, pch = 1)

#6 Estimation des valeurs (n+1) avec α = 0.3 et α = 0.8
predire_prochaine_valeur <- function(serie, alpha) {
  lissage <- lissage_exponentiel_simple(serie, alpha)
  return(lissage[length(lissage)]) # Dernière valeur lissée pour prédire n+1
}

alpha1 <- 0.3
alpha2 <- 0.8

X_pred_03 <- predire_prochaine_valeur(X, alpha1)
X_pred_08 <- predire_prochaine_valeur(X, alpha2)

Y_pred_03 <- predire_prochaine_valeur(Y, alpha1)
Y_pred_08 <- predire_prochaine_valeur(Y, alpha2)

cat("Prédictions pour X avec α = 0.3:", X_pred_03, "\n")
cat("Prédictions pour X avec α = 0.8:", X_pred_08, "\n")
cat("Prédictions pour Y avec α = 0.3:", Y_pred_03, "\n")
cat("Prédictions pour Y avec α = 0.8:", Y_pred_08, "\n")
