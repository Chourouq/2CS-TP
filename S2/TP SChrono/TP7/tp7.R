getwd()
setwd('/Users/aicha/Desktop/2CS/2CS-TP/S2/TP SChrono/TP7')
library(readxl)
data<- read_excel('data5.xlsx')
args(read_excel)
head(data)
X1<-na.omit(data$x1) 
print(X1)
#1-Identification
#Observer le graphique
plot.ts(X1, main = "Série x1-Indice Biologique", ylab = "x1", xlab = "Temps")
#Étudier la stationnarité
library(tseries)
t<-1 : length(X1)
tend <- lm(X1 ~ t)
summary(tend)
adf.test(X1, alternative = c("stationary"),k=0)
#Appliquer une différenciation si la série n’est pas stationnaire.
dx1 <- diff(X1)
t<-1 : length(dx1)
tend <- lm(dx1 ~ t)
summary(tend)
adf.test(dx1, alternative = c("stationary"),k=0)
plot.ts(dx1, main = "Différence première de x1")
#Utiliser les ACF (Autocorrelation Function) et PACF (Partial ACF) pour estimer les ordres p (AR), d (différence), q (MA).
par(mfrow = c(1, 2)) 
acf(dx1, main = "ACF de x1")
pacf(dx1, main = "PACF de x1")
par(mfrow = c(1, 1))  
acf(dx1, main = "ACF de x1")
pacf(dx1, main = "PACF de x1")


#2-Estimation
# AR(5)
mod1 <- arima(dx1, order = c(5,0,0))
# MA(4)
mod3 <- arima(dx1, order = c(0,0,4))
# ARMA(5, 4)
mod2 <- arima(dx1, order = c(5,0,4))
# Modèle AR(1)
model_ar <- arima(dx1, order = c(1, 0, 0))
# Modèle MA(1)
model_ma <- arima(dx1, order = c(0, 0, 1))
# Modèle ARMA(1,1)
model_arma <- arima(dx1, order = c(1, 0, 1))
# Modèle ARMA(2,1)
model_arma2 <- arima(dx1, order = c(2, 0, 1))

AIC(mod1) #958.35
AIC(mod2) #913.6977
AIC(mod3) #1064.354
AIC(model_ar)
AIC(model_ma)
AIC(model_arma)
AIC(model_arma2)

# Calculer les erreurs standards des paramètres.
tsdiag(model_arma)  # ou model_ar / model_ma / model_arma2

# 1. Récupération des résidus
residus <- residuals(model_arma)
t<-1 : length(residus)
tend <- lm(residus ~ t)
summary(tend)
adf.test(residus, alternative = c("stationary"),k=0)

# 1️⃣ Test de stationnarité (ADF test)
adf_result <- adf.test(residus, alternative = c("stationary"),k=0)
print(adf_result)

# 2️⃣ Test de normalité
shapiro_result <- shapiro.test(residus)
print(shapiro_result)
#0.9927>0.05
# Graphique de normalité
par(mfrow = c(1, 2))
hist(residus, main = "Histogramme des résidus", col = "lightblue")
qqnorm(residus)
qqline(residus, col = "red")

# 3️⃣ Test d'indépendance (autocorrélation)
Box_test <- Box.test(residus, lag = 10, type = "Ljung-Box")
print(Box_test)
#p-value > 0.05 → pas d'autocorrélation → indépendants ✅

# ACF des résidus
acf(residus, main = "ACF des résidus")
#acf: tous les points dans les bandes bleues → pas d'autocorrélation



#Exercice2:

X2<-na.omit(data$x2) 
head(X2)
print(X2)
plot.ts(X2, main = "Série x2-", ylab = "x1", xlab = "Temps")
#n'est pas stationnaire 
library(tseries)
t<-1 : length(X2)
tend <- lm(X2 ~ t)
summary(tend)
adf.test(X2, alternative = c("stationary"),k=0)
#diffirenciation 1
dx2<- diff(X2)
t<-1 : length(dx2)
tend <- lm(dx2 ~ t)
summary(tend)
adf.test(dx2, alternative = c("stationary"),k=0)
plot.ts(dx2, main = "Différence première de x2")
#Utiliser les ACF (Autocorrelation Function) et PACF (Partial ACF) pour estimer les ordres p (AR), d (différence), q (MA).
par(mfrow = c(1, 2)) 
acf(dx2, main = "ACF de x2")
pacf(dx2, main = "PACF de x2")
par(mfrow = c(1, 1))  
acf(dx2, main = "ACF de x2")
pacf(dx2, main = "PACF de x2")
#2-Estimation  de x2
# AR(1)
mod1 <- arima(dx2, order = c(1,0,0))
# ARMA(1,1)
mod3 <- arima(dx2, order = c(1,0,1))
# ARMA(2, 0)
mod2 <- arima(dx2, order = c(2,0,0))
# Modèle AR(2,1)
mod4 <- arima(dx2, order = c(2, 0, 1))
#AIC
AIC(mod1) 
AIC(mod2) 
AIC(mod3) 
AIC(mod4)
#les erreurs
residus1 <- residuals(mod3)
t<-1 : length(residus1)
tend <- lm(residus1 ~ t)
summary(tend)
adf.test(residus, alternative = c("stationary"),k=0)

# 1️⃣ Test de stationnarité (ADF test)
adf_result <- adf.test(residus1, alternative = c("stationary"),k=0)
print(adf_result)

# 2️⃣ Test de normalité
shapiro_result <- shapiro.test(residus1)
print(shapiro_result)

# Graphique de normalité
par(mfrow = c(1, 2))
hist(residus1, main = "Histogramme des résidus", col = "lightblue")
qqnorm(residus1)
qqline(residus, col = "red")

# 3️⃣ Test d'indépendance (autocorrélation)
Box_test <- Box.test(residus1, lag = 10, type = "Ljung-Box")
print(Box_test)
#p-value > 0.05 → pas d'autocorrélation → indépendants ✅

# ACF des résidus
acf(residus1, main = "ACF des résidus")
#acf: tous les points dans les bandes bleues → pas d'autocorrélation
# derniere question
forcest<- forecast(mod3,h=1)
print(forcest)
forecast_x2 <-forecast(mod3)
plot(forecast_x2) 

