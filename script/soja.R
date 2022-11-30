require(forecast)
require(timeSeries) 
require(urca)
require(randtests)
require(tseries)
require(stats)
library(readxl)
library(tidyverse)

soja <- read_xls("C:\\Users\\Douglas\\Documents\\TRABALHO\\time-series-soja\\dataset\\soja.xls")

soja <- ts(soja$Preço, start = c(1997,10), frequency = 12)

class(soja)
as.ts(soja)

summary(soja) # lembrar de sempre fazer estatatisticas descritivas

# Como ajustar modelos? (Metodologia Box e Jenkins)

# Como uma s?rie temporal tem os dados coletados sequencialmente ao longo 
# do tempo, espera-se que ela apresente correla??o seriada no tempo.
# Autocorrela??o ? a correla??o entre os valores da s?rie em um determinado 
# per?odo de tempo e os valores da mesma s?rie em um outro momento. 
# Os modelos de Box e Jenkins s?o modelos que visam 
# captar o comportamento da correla??o entre os valores da s?rie temporal,
# e com base nesse comportamento realizar previs?es futuras.

########################################
### Identifica??o do modelo
########################################

plot(soja, ylab = 'Preço da Soja')
plot(decompose(soja))

acf(soja, main="Função de Autocorrelação")

ndiffs(soja) #quant de diferencia??es necess?rias para a s?rie ser estacion?ria
nsdiffs(soja) #quant de diferencia??ess necess?rias na parte sazonal da s?rie

APdif<- diff(soja)
plot(APdif, ylab = 'Preço da Soja')

APlog <- log(soja)
plot(APlog)

par(mfrow=c(1,1))

APlog.dif <- diff(APlog)
plot(APlog.dif, ylab = "Preço da Soja")

APlog.dif2 <- diff(APlog.dif, lag=12)
plot(APlog.dif2,ylab='Preço da Soja')

## Tamb?m podem ser utilizado os seguintes comandos:

# A s?rie precisa de 1 diferen?a simples e 1 sazonal (ap?s sua transforma??o)
z = diff(diff(APlog),lag=12)
plot(z, ylab='Passengers')

# Teste para verificar estacionariedade

adf.test(APlog.dif2)
# H0: A s?rie n?o ? estacion?ria
# H1: A s?rie ? estacion?ria
# Se p-valor < menor que alfa, rejeita-se H0. Caso contr?rio, n?o rejeita-se H0.
# A s?rie ? estacion?ria, pois p-valor ? Menor que alfa.

#teste de Philippe Perron 
pp.test(APlog.dif2)

# Teste Kwiatkowski, Philips, Schimidt e Shin
kpss.test(APlog.dif2,null="Level")
# H0: A s?rie ? estacionaria
# H1: A s?rie n?o ? estacion?ria
# Se p-valor ? menor que alfa, rejeita-se H0
# p-valor > 0,05 - Indica que a s?rie ? estacion?ria ###
ur.kpss(APlog.dif2)

# Teste para tend?ncia
cox.stuart.test(APlog.dif2)
# H0: N?o existe tend?ncia
# H1: Existe tend?ncia
# Se p-valor ? menor que alfa, rejeita-se H0.
# A s?rie n?o possui tend?ncia. 

# Tamb?m pode ser usado o teste de Mann Kendall

# Para verificar a Sazonalidade
#Friedman
#Kruskall Wallis

#ggtsdisplay(APlog.dif2)


# AR(p) - fun??o de autocorrela??o parcial
# MA(q) - fun??o de autocorrela??o
# ARMA(p,q)
# ARIMA(p,d,q)
# SARIMA(1,1,1)(1,1,1)

x11()
acf(APlog.dif2)

#m1 <- acf(APlog.dif2, plot = F)
#m1$lag <- m1$lag*12
#plot(m1, main = "ACF da s?rie com diferen?a Simples e Sazonal")

pacf(APlog.dif2)
# PACF ou FACP

#m2 <- pacf(APlog.dif2, plot = F)
#m2$lag <- m2$lag*12
#plot(m2, main = "FACP da s?rie com diferen?a Simples e Sazonal")

par(mfrow=c(1,1))

#SARIMA(0,1,1)(0,1,1)

auto.arima(APlog)

# realiza a verifica??o dos poss?veis modelos gerados a partir da s?rie temporal
# em quest?o, visando ao ajuste ideal.


########################################
### Estimando o modelo
########################################

# SARIMA(1,1,1)(1,1,1)

#Arima(AirPassengers, order = c(1,1,1), seasonal = c(1,1,1), lambda = 0) 
#fun??o 'Arima'(Arima com 'a' mai?sculo)do pacote forecast. O lambda = 0 permite que seja feita a transforma??o logar?tmica
#n?o ? preciso diferenciar a s?rie pois a pr?pria fun??o faz isso
#ou

modelo1 <- arima(APlog, order = c(1,1,1), seasonal = list(order=c(1,1,1)))
acf(modelo1$residuals)
pacf(modelo1$residuals)
confint(modelo1)
modelo1$coef

# os param?tros da parte AR n?o sazonal e sazonal s?o n?o significativos 
# (pois cont?m o zero no intervalo),
# logo, devem ser retirados e o modelo reestimado

# SARIMA(0,1,1)(0,1,1)

modelo2 <- arima(APlog, order = c(0,1,1), seasonal = list(order=c(0,1,1)))
acf(modelo2$residuals)
pacf(modelo2$residuals)
confint(modelo2)

modelo3 <- arima(APlog, order = c(0,1,1), seasonal = list(order=c(0,1,1)))
acf(modelo3$residuals)
pacf(modelo3$residuals)
confint(modelo3)
modelo3$coef
# par?metro significativo, pois n?o cont?m o zero no intervalo
# Os par?metros dos modelos s?o significantes, o que implica na necessidade da 
# an?lise de seus res?duos para verificar se os modelos s?o adequados.


########################################
### Verifica??o do modelo
########################################

x11()
tsdiag(modelo3) 
# retorna o gr?fico dos res?duos padronizados, 
# o correlograma e os
# p-valores do testes Ljung-Box, que devem estar acima de 0 para indicar
# que os res?duos s?o independentes.

# H0: Autocorrela??es at? lag k s?o iguais a 0 
#(n?o existe depend?ncia linear entre os res?duos)

Box.test(modelo3$residuals) 
Box.test(modelo2$residuals)
Box.test(modelo1$residuals)

# P-valores superiores ao n?vel de signific?ncia (alpha = 0,05)
# indicam que os res?duos se comportam como um ru?do branco. Portanto, 
# n?o rejeta H0
# Logo, ao n?vel de 5% de signific?ncia, 
# n?o rejeita-se a hip?tese de que as autocorrela??es n?o s?o significantes, 
# ou seja, os res?duos se comportam como ru?do branco 
# e o modelo est? ajustado adequadamente

Box.test(modelo3$residuals, type = c("Ljung-Box"))

#checkresiduals(modelo3)
hist(modelo3$residuals)
qqnorm(modelo3$residuals)
qqline(modelo3$residuals)
shapiro.test(modelo3$residuals)

#O teste Shapiro-Wilk tem como hip?tese nula a normalidade. 
# Se p-valor < 0,05, rejeita-se a normalidade. 
#Como p-valor > 0,05 n?o podemos rejeitar que os res?duos s?o
#normalmente distribu?dos.


# Crit?rios de compara??o dos modelos 

AIC(modelo1)
AIC(modelo2)
AIC(modelo3)

BIC(modelo1)
BIC(modelo2)
BIC(modelo3)

# o melhor modelo ser? aquele que apresentar menor AIC e BIC

############################
##Previs?o do modelo
############################

accuracy(modelo3)

prev <- forecast(modelo3, h=12)
plot(prev)

AirPassengers

# prev = C?lculo com o R das previs?es pontuais 
#e intervalos de 80 e 95% de confian?a 


# Previs?es para a s?rie transformada

p = as.data.frame(predict(modelo3, 12, interval="prediction")); p

### Predi??es para "12" per?odos seguintes e os respectivos erros padr?o ###


# Intervalo de confian?a 95%
LI = exp(p$pred - 1.96*p$se)
LI
LS=exp(p$pred + 1.96*p$se)
LS

data.frame(p$pred,LI,LS)

# Previs?es para a s?rie original - segue o que diz Morettin e Toloi (2004, p.233)

previsoes = exp(p$pred + modelo3$sigma2/2)

df = data.frame(previsoes,LI,LS)
df

#print(df,row.names=FALSE)

### Limite Inferior, Previs?es, Limite Superior ###

############################
## Extraindo as previs?es
############################
#Em formato .csv:
write.csv2(df,"C://Users//Lulu//Documents//UNIR_2021//Series Temporais//previsao.csv")

#Em formato .xlsx :
require(xlsx)
write.xlsx(prev,"C://Users//Lulu//Documents//UNIR_2021//Series Temporais//previsao.xlsx")


x = data.frame(AirPassengers)
linhas = c(133:144)
x.novo = x[linhas,]
data.frame(x.novo,previsoes)



#################
# Outras formas
# https://rpubs.com/Possato/ARIMA_ajuste_tutorial
# https://github.com/pedrocostaferreira/SeriesTemporais/blob/master/Aula7_ModelagemAirpassengers.R

d=seq(range(modelo3$residuals)[1]-3*sd(modelo3$residuals),range(modelo3$residuals)[2]+3*sd(modelo3$residuals),0.001)
lines(d,dnorm(d,0,sd(modelo3$residuals)))
#previsoes para 15 periodos seguintes e respectivos erros padroes
pmodelo3=predict(modelo3,n.ahead=12, se.fit=T)
pmodelo3
plot(ts(c(APlog,pmodelo3$pred)))
APlog
plot(ts(c(APlog,pmodelo3$pred),frequency=12,start=c(1949,1)))
abline(v=c(1961,1),lty=2)
lines(pmodelo3$pred+1.96*pmodelo3$se,col=2)
lines(pmodelo3$pred-1.96*pmodelo3$se,col=2)


