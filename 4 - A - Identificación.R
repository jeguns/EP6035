
# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tseries)
library(aTSA)
library(forecast)
library(TSA)
library(MTS)

# Lectura de datos --------------------------------------------------------

# TMIN2021
# Temperatura mínima diaria registrada en la estación von Humboldt, en la
# UNALM, durante el mes de abril 2021

# SINOPHARM
# Número diario de dosis de SINOPHARM aplicadas a nivel nacional

read.delim("TMIN2021.txt") -> datos1
read_excel("SINOPHARM.xlsx") -> datos2

datos1$temp %>% plot(type="l",col=5, lwd=2, main = "Temp minima")
datos2$n %>% plot(type="l",col=4, lwd=2, main = "Sinopharm")

# Operadores --------------------------------------------------------------

datos1$temp %>% dplyr::lag(n=2)
cbind(datos1$temp,
      datos1$temp  %>% dplyr::lag(n=1),
      datos1$temp  %>% dplyr::lag(n=2))
cbind(datos1$temp,
      datos1$temp  %>% dplyr::lag(n=1),
      datos1$temp  %>% dplyr::lag(n=2)) %>% na.contiguous()

datos1$temp  %>% dplyr::lead(n=2)
cbind(datos1$temp,
      datos1$temp  %>% dplyr::lead(n=1),
      datos1$temp  %>% dplyr::lead(n=2))

datos1$temp %>% diff
datos2$n %>% diff 

x11();par(mfrow=c(2,2))
datos1$temp %>% plot(type="l",col=5, lwd=2, main = "Temp. mínima")
datos2$n %>% plot(type="l",col=4, lwd=2, main = "Sinopharm")
datos1$temp %>% diff %>% plot(type="l",col=5, lwd=2, main = "Temp. mínima (diferenciado)")
datos2$n %>% diff %>% plot(type="l",col=4, lwd=2,  main = "Sinopharm (diferenciado)")

# Autocorrelacion ---------------------------------------------------------

datos1$temp %>% ts() -> serie1
serie1 %>% TSA::acf(type = "correlation", lag = 28)
serie1 %>% TSA::acf(type = "partial", lag = 28)
serie1 %>% TSA::acf(type = "correlation", lag = 28, plot=F)
serie1 %>% TSA::acf(type = "partial", lag = 28, plot=F)

datos2$n %>% ts() -> serie2
serie2 %>% TSA::acf(type = "correlation", lag = 28)
serie2 %>% TSA::acf(type = "partial", lag = 28)
serie2 %>% TSA::acf(type = "correlation", lag = 28, plot=F)
serie2 %>% TSA::acf(type = "partial", lag = 28, plot=F)

# Estacionariedad ---------------------------------------------------------

serie1 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie1 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie1 %>% aTSA::stationary.test(method = "adf")
serie1 %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie1 %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie1 %>% BoxCox.lambda()
serie1 %>% archTest()
serie1 %>% McLeod.Li.test(y=.)

serie1 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie1 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie1 %>% diff %>% aTSA::stationary.test(method = "adf")
serie1 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie1 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie1 %>% diff %>% BoxCox.lambda()
serie1 %>% diff %>% archTest()
serie1 %>% diff %>% McLeod.Li.test(y=.)

serie2 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie2 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie2 %>% aTSA::stationary.test(method = "adf")
serie2 %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie2 %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie2 %>% BoxCox.lambda()
serie2 %>% archTest()
serie2 %>% McLeod.Li.test(y=.)

serie2 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie2 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie2 %>% diff %>% aTSA::stationary.test(method = "adf")
serie2 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie2 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie2 %>% diff %>% BoxCox.lambda()
serie2 %>% diff %>% archTest()
serie2 %>% diff %>% McLeod.Li.test(y=.)

# Autocorrelaciones de series diferenciadas -------------------------------

serie1 %>% diff %>% TSA::acf(type = "correlation", lag = 28)
serie1 %>% diff %>% TSA::acf(type = "partial", lag = 28)

serie2 %>% diff %>% TSA::acf(type = "correlation", lag = 28)
serie2 %>% diff %>% TSA::acf(type = "partial", lag = 28)

