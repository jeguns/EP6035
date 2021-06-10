

library(readxl)
library(dplyr)
library(tseries)
library(aTSA)
library(forecast)
library(TSA)
library(MTS)
library(sweep)
library(ggplot2)
library(gridExtra)
library(tidyquant)
library(dplyr)
library(tseries)
library(aTSA)
library(forecast)
library(TSA)
library(MTS)

# ARIMA(1,0,1) ------------------------------------------------------------

phi    = 0.88
theta  = 0.91
sigma  = 2.5

set.seed(564654)
arima.sim(list(order = c(1,0,1), ar = phi, ma = theta), n = 150) -> Y1

Y1 %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y1 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y1 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y1 %>% aTSA::stationary.test(method = "adf")
Y1 %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y1 %>% aTSA::stationary.test(method = "pp",lag.short=T)
Y1 %>% BoxCox.lambda()
Y1 %>% archTest()
Y1 %>% McLeod.Li.test(y=.)

Y1 %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y1 %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")

# ARIMA(2,2,2) ------------------------------------------------------------

phi    = c(0.65,0.2)
theta  = c(0.4,-0.9)
sigma  = 10

arima.sim(list(order = c(2,2,2), ar = phi, ma = theta), 
          n = 150,
          sd = sigma) -> Y2

Y2 %>% 
  autoplot(type="l") 

Y2 %>% diff %>% 
  autoplot(type="l") 

Y2 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y2 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y2 %>% aTSA::stationary.test(method = "adf")
Y2 %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y2 %>% aTSA::stationary.test(method = "pp",lag.short=T)

Y2 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y2 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y2 %>% diff %>% aTSA::stationary.test(method = "adf")
Y2 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y2 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)

Y2 %>% diff %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y2 %>% diff %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y2 %>% diff %>% diff %>% aTSA::stationary.test(method = "adf")
Y2 %>% diff %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y2 %>% diff %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)

Y2 %>% diff %>% diff %>%
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y2 %>% diff %>% diff %>%
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")

t_stat(Arima(Y2, order=c(2,2,2)))

