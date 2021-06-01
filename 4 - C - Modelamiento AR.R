
# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(tseries)
library(aTSA)
library(forecast)
library(TSA)
library(MTS)
library(ggplot2)
library(sweep)
library(tidyquant)

# Lectura de datos --------------------------------------------------------

read.delim("TMIN2021.txt") -> datos1
datos1$temp %>% ts() -> serie1

# Identificación ----------------------------------------------------------

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

# serie1 %>% TSA::acf(type = "correlation", lag = 28) # no
# serie1 %>% TSA::acf(type = "partial", lag = 28) # no

x11();serie1 %>% diff %>% TSA::acf(type = "correlation", lag = 28)
x11();serie1 %>% diff %>% TSA::acf(type = "partial", lag = 28)

# Modelamiento ------------------------------------------------------------

ntotal = length(serie1)
ntrain = 20
h      = 4
medidas1 = medidas2 = medidas3 = medidas4 = NULL

for(i in 0:(ntotal-ntrain-h)){
  training <- serie1 %>%  window(start = 1, end = ntrain + i)
  testing  <- serie1 %>%  window(start = ntrain + i + 1, end= ntrain + i + 4)
  modelo1  <- training %>% Arima(order=c(0,1,0))
  modelo2  <- training %>% Arima(order=c(1,1,0))
  modelo3  <- training %>% Arima(order=c(0,0,0))
  modelo4  <- training %>% Arima(order=c(3,3,3))
  pred1    <- modelo1 %>% forecast::forecast(h=4)
  pred2    <- modelo2 %>% forecast::forecast(h=4)
  pred3    <- modelo3 %>% forecast::forecast(h=4)
  pred4    <- modelo4 %>% forecast::forecast(h=4)
  medidas1 <- rbind(medidas1, accuracy(pred1,testing)[2,])
  medidas2 <- rbind(medidas2, accuracy(pred2,testing)[2,])
  medidas3 <- rbind(medidas3, accuracy(pred3,testing)[2,])
  medidas4 <- rbind(medidas4, accuracy(pred4,testing)[2,])  
}

medidas1 %>% colMeans
medidas2 %>% colMeans
medidas3 %>% colMeans
medidas4 %>% colMeans

arima010 <- function(x, h){forecast(Arima(x, order=c(0,1,0)), h=h)}
arima110 <- function(x, h){forecast(Arima(x, order=c(1,1,0)), h=h)}
arima000 <- function(x, h){forecast(Arima(x, order=c(0,0,0)), h=h)}
arima333 <- function(x, h){forecast(Arima(x, order=c(3,3,3)), h=h)}

e010 <- tsCV(serie1, arima010, h=4, window = 20)
e110 <- tsCV(serie1, arima110, h=4, window = 20)
e000 <- tsCV(serie1, arima000, h=4, window = 20)
e333 <- tsCV(serie1, arima333, h=4, window = 20)

me = mae = mape = rmse = NULL
for(i in 1:nrow(e010)){
  me[i]   = e010[i,] %>% mean
  rmse[i] = e010[i,]^2 %>% mean %>% sqrt
  mae[i]  = e010[i,] %>% abs %>% mean
}
(data.frame(ME = me, RMSE = rmse, MAE = mae) %>% filter(!is.na(rmse)) -> medidas_1)
medidas1

medidas_1 %>% colMeans
medidas1 %>% colMeans

# Escriba el modelo final:

serie1 %>% Arima(order = c(1,1,0)) -> modelofinal

modelofinal %>% sw_tidy()

# Diagnóstico -------------------------------------------------------------

modelofinal %>% residuals -> residuales

residuales %>% autoplot + 
  geom_hline(yintercept=0,col="red") + 
  labs(title = "Residuales del modelo AR(1,1,0)", x = "")+
  theme_bw()

modelofinal %>% sw_augment() %>% select(.resid) -> residuales_tibble

modelofinal %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo ARIMA(1,1,0)", x = "") + 
  theme_minimal()

residuales %>% shapiro.test()
residuales %>% TSA::acf()
residuales %>% McLeod.Li.test(y=.)
residuales %>% BoxCox.lambda()
residuales %>% aTSA::stationary.test(method="kpss")
residuales %>% aTSA::stationary.test(method="adf")

modelofinal %>% sw_glance()

# Predicciones ------------------------------------------------------------

modelofinal %>% forecast(h=4)

modelofinal %>% forecast(h=4) %>% autoplot +
  labs(x = "Día",
       y = "Temperatura") + 
  theme_minimal()

modelofinal %>% forecast(h=4) %>% sw_sweep() %>% View

modelofinal %>% 
  forecast(h=4) %>% 
  sw_sweep() %>%
  ggplot(aes(x = index, y = value, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Predicción Temperatura Mínima", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()
