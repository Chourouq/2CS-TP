getwd()
setwd('/Users/aicha/Desktop/2CS/2CS-TP/S2/TP SChrono/TP3')
library(readxl)
data<- read_excel('data.xlsx')
args(read_excel)
head(data)
serie1<- data$série1
serie1
serie2<-data$série2
serie2
serie3<-data$série3
serie3
plot.ts(serie1)#il ya des tendances 
plot.ts(serie2)#il ya tendance ,il ya unn phenomene qui repete alors il s'agit un periodique 
plot.ts(serie3)#il est stationnaire psq //la variance au debut est faible alors variance depend du temps 
#Exercice2
#representation en utilisant une fonction y(t)
data2<-read_excel('data.xlsx',sheet=2)
args(read_excel)
data2
y<-data2$série4
y
n<-length(y)
t <- 1:n
z <- log(0.55 * t + 2)
plot( y, col = "blue")
lines(t, z, col = "red", lwd = 2)
legend("topright", legend = c("y", "z"), 
       col = c("blue", "red"), lwd = 2)

#Exercice3
#1
n<-100
e=ts(rnorm(n,0,1))
plot(e)
#2
hist(e)
#La commande hist() permet d'afficher l'histogramme des valeurs contenues dans e1 (ici bruit_blanc). Elle est utile pour visualiser la distribution des données.
#3
n2 <- 200
bruit2 <- rnorm(n2, mean = 0, sd = sqrt(2.5)) 
plot(bruit2, type = "l", col = "red", main = "Bruit blanc de variance 2.5", ylab = "Valeurs", xlab = "Temps")

#Exercice4
#la simulation pour l'education des series ou tester les modeles.
n <- 150
t <- 1:n

Tt <- 0.75 * t + 1.5  # Tendance linéaire
St <- 10 * sin(2 * pi * t / 25)  # Composante saisonnière de periode 25
et <- rnorm(n, mean = 0, sd = sqrt(1))  # Bruit blanc 

Xt <- Tt + St + et

plot(t, Xt, type = "l", col = "purple", main = "Série chronologique avec tendance et s", ylab = "Valeurs", xlab = "Temps")
lines(t, Tt, col = "red", lwd = 2)  # Tracer la ten
lines(t, St, col = "blue", lwd = 2)  # Tracer la saisonnalité 
#Additif : Pas besoin d'ajuster, chaque composante garde sa forme.les composants sont independants

#Exercice5
n <- 150
t <- 1:n

Tt <- (t^2) / 20 + 1  # Tendance quadratique
St <- 50 * cos(2 * pi * t / 10)  # Saisonnière de period 10
et <- runif(n, min = 1, max = 5)  # Bruit blanc uniforme entre [1,5]

Xt <- Tt * St * et  

plot(t, Xt, type = "l", col = "darkgreen", main = "Série avec tendance quadratique et saisonnalité", ylab = "Valeurs", xlab = "Temps")
lines(t, Tt * mean(St) * mean(et), col = "red", lwd = 2)  # Tendance ajustée 
lines(t, mean(Tt) * St * mean(et), col = "blue", lwd = 2)  # Saisonnière ajustée


#probleme: t et s séparément sans considérer qu'ils sont multipliés dans la série.
#les deux composantes saisonni`ere et r ́esiduelle, sont d ́ependantes de la tendance.

