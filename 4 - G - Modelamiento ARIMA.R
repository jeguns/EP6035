

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

datos <- read_excel("Indice.xlsx")

ts(datos$Y) -> serie


serie %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie %>% aTSA::stationary.test(method = "adf")
serie %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie %>% BoxCox.lambda()
serie %>% archTest()
serie %>% McLeod.Li.test(y=.)


serie %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie %>% diff %>% aTSA::stationary.test(method = "adf")
serie %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie %>% diff %>% BoxCox.lambda()
serie %>% diff %>% archTest()
serie %>% diff %>% McLeod.Li.test(y=.)

serie %>% diff %>% TSA::acf(type = "correlation", lag = 28)
x11();serie %>% diff %>% TSA::acf(type = "partial", lag = 28)

serie %>% auto.arima

ntotal = length(serie)
ntrain = 290
h      = 7 
medidas1 = medidas2 = medidas3 = medidas4 = medidas5 = medidas6 = NULL

for(i in 0:(ntotal-ntrain-h)){
  training <- serie %>%  window(start = 1, end = ntrain + i)
  testing  <- serie %>%  window(start = ntrain + i + 1, end= ntrain + i + 4)
  modelo1  <- training %>% Arima(order=c(1,1,1))
  modelo2  <- training %>% Arima(order=c(2,1,1))
  modelo3  <- training %>% Arima(order=c(1,1,2))
  modelo4  <- training %>% Arima(order=c(0,1,0))
  modelo5  <- training %>% Arima(order=c(1,1,0))
  modelo6  <- training %>% Arima(order=c(0,1,1))
  pred1    <- modelo1 %>% forecast::forecast(h=4)
  pred2    <- modelo2 %>% forecast::forecast(h=4)
  pred3    <- modelo3 %>% forecast::forecast(h=4)
  pred4    <- modelo4 %>% forecast::forecast(h=4)
  pred5    <- modelo5 %>% forecast::forecast(h=4)
  pred6    <- modelo6 %>% forecast::forecast(h=4)
  medidas1 <- rbind(medidas1, accuracy(pred1,testing)[2,])
  medidas2 <- rbind(medidas2, accuracy(pred2,testing)[2,])
  medidas3 <- rbind(medidas3, accuracy(pred3,testing)[2,])
  medidas4 <- rbind(medidas4, accuracy(pred4,testing)[2,])  
  medidas5 <- rbind(medidas4, accuracy(pred5,testing)[2,])  
  medidas6 <- rbind(medidas4, accuracy(pred6,testing)[2,])  
  
}


medidas1 %>% colMeans
medidas2 %>% colMeans
medidas3 %>% colMeans
medidas4 %>% colMeans
medidas5 %>% colMeans
medidas6 %>% colMeans

serie %>% Arima(order = c(2,1,1)) -> modelo_2
serie %>% Arima(order = c(0,1,0)) -> modelo_4

modelo_2 %>% residuals -> residuales_2
modelo_4 %>% residuals -> residuales_4

modelo_2 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo ARIMA(0,1,1)", x = "") + 
  theme_minimal() -> graf_res_2

modelo_4 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo ARIMA(1,1,2)", x = "") + 
  theme_minimal()-> graf_res_4

grid.arrange(graf_res_6, graf_res_3, graf_res.6, graf_res.3)

modelo_2 %>% t_stat
modelo_4 %>% t_stat

modelo_2 %>% residuals %>% shapiro.test
modelo_4 %>% residuals %>% shapiro.test

residuales_2 %>% hist()
residuales_4 %>% hist()

residuales_2 %>% TSA::acf(lag=50)
residuales_4 %>% TSA::acf(lag=50)

residuales_2 %>% BoxCox.lambda()
residuales_4 %>% BoxCox.lambda()

residuales_2 %>% aTSA::stationary.test(method="kpss")
residuales_4 %>% aTSA::stationary.test(method="kpss")

residuales_2 %>% aTSA::stationary.test(method="adf")
residuales_4 %>% aTSA::stationary.test(method="adf")

residuales_2 %>% aTSA::stationary.test(method="pp")
residuales_4 %>% aTSA::stationary.test(method="pp")


# Prediccion --------------------------------------------------------------

modelo_2 %>% forecast::forecast(h=7)

modelo_2 %>% forecast::forecast(h=7) %>% autoplot +
  labs(x = "Día",
       y = "Y") + 
  theme_minimal()

modelo_2 %>% 
  forecast::forecast(h=7) %>% 
  sw_sweep() %>%
  ggplot(aes(x = index, y = value, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Predicción Recuperados", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()

