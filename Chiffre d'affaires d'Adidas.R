#Partie I : la moyenne mobile
#On rentre les donn�es de 2010 � 2016
data <- c(2674,2917,3468,2931,3273,3064,3744,3241,3824,3517,4173,3369,3751,3383,3879,3391,3480,3400,4044,3610,4083,3907,4758,4167,4769,4199,5413,4687)
plot(data)
#Date de d�part et p�riode
data_ts <- ts(data, start=c(2010,1),frequency = 4)


#On utilise decompose pour une s�rie multiplicative
decompose_data <- decompose(data_ts, type ="multiplicative")
plot(decompose_data)
#On print les coefficients saisonniers
print(decompose_data$seasonal)

#On utilise decompose pour une s�rie additive
decompose2_data <- decompose(data_ts)
#On print les coefficients saisonniers
print(decompose2_data$seasonal)
#On privil�gie le mod�le multiplicatif, dont les coefficients saisonniers sont moins al�atoires.

#Partie II : le lissage exponentiel

#On importe les librairies
library(forecast)
library(MLmetrics)

data.des=decompose(data_ts)$trend
#Cr�ation d'une fen�tre graphique
X11()
plot(data_ts,main="2010-2016",ylab="",xlab="")
points(data.des,type="l",col="red") 


data.LES=window(data.des,start=c(2010,1),end=c(2016,4))
data.LES.des=decompose(data_ts)$trend

#On divise les donn�es en une partie training (environ 80%) et un partie validation (environ 20%)
data.LES.train = window(data_ts,start=c(2010,1),end=c(2014,4))
data.LES.val = window(data_ts, start=c(2015,1), end=c(2016,4))

#Premier lissage exponentiel
LES = ets(data.LES.train, model="MNN")
summary(LES)
LES_forecast= forecast(LES, h = 8)
MAPE(LES_forecast$mean, data.LES.val)*100
#On print les pr�visions les pr�visions
#Cr�ation d'une fen�tre graphique
X11()
plot(LES_forecast)
points(data.LES.val,type="l",col="red")

#Deuxi�me lissage exponentiel
data.LED.train=window(data_ts,start=c(2010,1), end=c(2014,4))
data.LED.val=window(data_ts,start=c(2015,1), end=c(2016,4))
LED = ets(data.LED.train, model="MMN")
summary(LED)
#On print les pr�visions
#Cr�ation d'une fen�tre graphique
X11()
LED_forecast=forecast(LED, h = 8)
MAPE(LED_forecast$mean, data.LED.val)*100
#On print les pr�visions
plot(LED_forecast)
points(data.LED.val,type="l",col="red")

#Holt-Winters

HW = ets(data.LED.train,model="ZZZ")
prev = forecast(HW, h = 8)
summary(HW)
X11()
plot(prev)
points(data.LED.val,type="l",col="red")

#On compare les diff�rents mod�les entre eux
HW1 = ets(data.LED.train,model="MMM")
prev1 = forecast(HW1, h = 8)
summary(HW1)

HW2 = ets(data.LED.train,model="AAA")
prev2 = forecast(HW2, h = 8)
summary(HW2)

HW3 = ets(data.LED.train,model="ZZZ")
prev3 = forecast(HW3, h = 8)
summary(HW3)
#On les print

#Cr�ation d'une fen�tre graphique
X11()
par(mfcol=c(3,1))
plot(prev1)
points(data.LED.val,type="l",col="red")
plot(prev2)
points(data.LED.val,type="l",col="red")
plot(prev3)
points(data.LED.val,type="l",col="red") 
