install.packages(c("quantmod","rugarch","rmgarch"))   # only needed in case you have not yet installed these packages
install.packages("dplyr")
install.packages("fGarch")
install.packages("ggplot2")
library(quantmod)
library(rugarch)
library(rmgarch)
library(dplyr)
library(fGarch)
library(readxl)
library(ggplot2)
#trebuie sa folosesc randamentele zilnice pe care le-am calculat in excell
#indicii locali trebuie grupati fiecare cu indicii globali; asta ar putea rezulta in 5 modele sau 25 de modele; de ce?pentru a vedea
#corelarile zilnice dintre indici
#informate noua: fiecare indice local trebuie grupat cu fiecare indice global , deci 25 de modele estimate in total!

#upload data incercare
setwd("C:/Users/despa/Desktop/Facultate/Licenta/DCC GARCH")
BET <- read_excel("CEE3.xlsx", sheet = "BET")
 View(BET)
DAX <- read_excel("CEE3.xlsx", sheet = "DAX30")
 View(DAX)

BUX <- read_excel("CEE3.xlsx", sheet = "BUX")

PX <- read_excel("CEE3.xlsx", sheet = "PX")

MOEX <- read_excel("CEE3.xlsx", sheet = "MOEX")

WIG <- read_excel("CEE3.xlsx", sheet = "WIG")

SP <- read_excel("CEE3.xlsx", sheet = "SP500")

N <- read_excel("CEE3.xlsx", sheet = "N225")

CAC <- read_excel("CEE3.xlsx", sheet = "CAC40")

FTSE <- read_excel("CEE3.xlsx", sheet = "FTSE100")
View(FTSE)

 #incercare de unire a dataseturilor ; aici trebuie schimbat in functie de ce vrei sa unesti
 cee<-left_join(WIG,SP)
# cee1<-cbind(BET,DAX) asta nu a mers din cauza ca nu au acelasi numar de randuri
 #View(cee)
 cee1<-na.omit(cee)

# View(cee1)
 
 # mi-a luat o ora,dar am reusit sa creez baza in variabila cee1
 
 #aici trebuie schimbat de fiecare data cu coloanele corespunzatoare pentru a functiona
 rX<-cee1[, c("rentab_WIG" ,"rentab_SP")]
 View(rX)
 
 #sursa a doua : functioneaza ! de cautat daca este valida si cu ce se deosebeste de prima sursa:
 # univariate normal GARCH(1,1) for each series
 garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                           variance.model = list(garchOrder = c(1,1), 
                                                 model = "sGARCH"), 
                           distribution.model = "norm")
 
 # dcc specification - GARCH(1,1) for conditional correlations
 dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                            dccOrder = c(1,1), 
                            distribution = "mvnorm")
 dcc.garch11.spec
 

 plot(rX)
 dcc.fit = dccfit(dcc.garch11.spec, data = rX)
 
 dcc.fit
 
 
 # plotting
 #startDate = as.Date("2004-01-02") #Specify period of time we are interested in
 #endDate = as.Date("2019-12-31")
 
 rp<-cee1[, ("Exchange Date")]
# View(rp)
 
 cov1 = rcov(dcc.fit)  # extracts the covariance matrix
cor1 = rcor(dcc.fit)  # extracts the correlation matrix

dim(cor1)
cor1[,,dim(cor1)[2]]
cor_BG <- cor1[2,1,]   # leaving the last dimension empty implies that we want all elements
#cor_BG <- as.xts(cor_BG)  # imposes the xts time series format - useful for plotting

plot(cor_BG)
plot(cor_BG, x=rp,type="l", lwd=2,main="(WIG ; S&P500)", xlab= "Orizontul de timp", ylab="corelatia zilnica dintre indici")
abline(h=c("0.0","0.05","0.1","0.15","0.2","0.25","0.3","0.35","0.4","0.45","0.5","0.55","0.6","0.65","0.7", "0.75"), col="grey")

#par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
#plot(as.xts(cor1[1,2,]),main="BET and FTSE")
#plot(as.xts(cor1[1,2,]),main="BET and FTSE")

#dupa daca faci forecasting dar nu prea cred 
# trebuie sa ajustezi liniile de cod la baza ta de date
#trebuie sa inveti sa iti introduci baza de date in R ; poti face pentru inceput un excel cu toate rentabilitatile pe o foaie ca sa fie mai usor