#Exercice1
getwd()
setwd('/Users/aicha/Desktop/2CS/2CS-TP/S2/TP SChrono/TP4')
library(readxl)
data<- read_excel('data1.xlsx')
args(read_excel)
head(data)
#Appliquer la moyenne mobile pour supprimer les effets saisonniers et lisser la tendance.
#Calculer les coefficients de saisonnalité pour chaque période :
#Dans le modèle additif, on soustrait la moyenne générale.
#Dans le modèle multiplicatif, on divise par la moyenne générale.
#Prendre le minimum des coefficients calculés (S0 et S1).
#Comparer ces valeurs pour voir si s0<= s1
saison_add <- function(x){
  #calculer les mm (1er etape)
  n <- length(x)
  mm <- rep(0,n) # moyen mobile (en peut calculer just n-2 valeur)
  p<- 4
  m<- 2
  d<- 1+m
  f<- n-m # indice de fin
  for (t in d:f){
    mm[t] = 1/p*(x[t-2]/2+ x[t-1]+x[t]+x[t+1]+x[t+2]/2) # claculer la moyen mobile, x est l'observation
  }
  #calculer les diff x - mm (etape 2)//ici on met / et pas -
  y = rep(0,n) # le contenue des 0
  for (t in d:f){ y[t] = x[t] - mm[t]} # y est la diff
  #donner la 1er estimat on des Sj (etape 3)
  s <- c(0,0,0,0)
  ss <- 0
  for (t in p){
    z <- 0 # z pour calculer les valeurs null , les valeur null sont les valeur null de la diff y
    for (i in 1:4){
      j<- t+4##############
      ss <- y[j]
      if (ss  == 0){z <- z+1}
      s[t] <- s[t]+ y[j]
    }
    l <- p - z
    s[t] <- s[t]/l
  }
  #claculer l s bar
  ms = mean(s)
  #ajuster les Sj
  s = s - ms #et on met / si multiplicatif
  return(s)
}
X0 <- data$X0
s_add_X0 <- saison_add(X0)


saison_multi <- function(x){
  #calculer les mm (1er etape)
  n <- length(x)
  mm <- rep(0,n) # moyen mobile (en peut calculer just n-2 valeur)
  p<- 4
  m<- 2
  d<- 1+m
  f<- n-m # indice de fin
  for (t in d:f){
    mm[t] = 1/p*(x[t-2]/2+ x[t-1]+x[t]+x[t+1]+x[t+2]/2) # claculer la moyen mobile, x est l'observation
  }
  #calculer les diff x - mm (etape 2)//ici on met / et pas -
  y = rep(0,n) # le contenue des 0
  for (t in d:f){ y[t] = x[t] / mm[t]} # y est la diff
  #donner la 1er estimat on des Sj (etape 3)
  s <- c(0,0,0,0)
  ss <- 0
  for (t in p){
    z <- 0 # z pour calculer les valeurs null , les valeur null sont les valeur null de la diff y
    for (i in 1:4){
      j<- t+4##############
      ss <- y[j]
      if (ss  == 0){z <- z+1}
      s[t] <- s[t]+ y[j]
    }
    l <- p - z
    s[t] <- s[t]/l
  }
  #claculer l s bar
  ms = mean(s)
  #ajuster les Sj
  s = s / ms #et on met / si multiplicatif
  return(s)
}
X0 <- data$X0
s_add_X0 <- saison_add(X0)
X1 <- data$X1
# Déterminer S0 et S1

s_mult_X1 <- saison_multi(X1)

# Déterminer S0 et S1
S0 <- min(s_add_X0)
S1 <- min(s_mult_X1)

# Afficher les résultats
cat("Coefficients de saisonnalité - Modèle additif (X0) :\n", s_add_X0, "\n")
cat("Coefficients de saisonnalité - Modèle multiplicatif (X1) :\n", s_mult_X1, "\n")
cat("S0 =", S0, "\n")
cat("S1 =", S1, "\n")
cat("Vérification : S0 <= S1 :", S0 <= S1, "\n")