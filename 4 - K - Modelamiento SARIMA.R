
# Paquetes ----------------------------------------------------------------

library(readxl)
library(forecast)
library(ggplot2)
library(caschrono)
library(TSA)
library(astsa)
library(sweep)
library(gridExtra)
library(tidyquant)
library(nortest)


# Lectura -----------------------------------------------------------------

datos = read_excel('Pruebas.xlsx',col_names=F)
serie = datos %>% ts(frequency = 7)

autoplot(serie)
seasonplot(serie,season.labels=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
ggseasonplot(serie,season.labels=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
ggseasonplot(diff(serie),season.labels=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))
monthplot(serie)
ggmonthplot(serie,labels= c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"))

serie %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie %>% aTSA::stationary.test(method = "adf") #
serie %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie %>% aTSA::stationary.test(method = "pp",lag.short=T) #
serie %>% BoxCox.lambda()
serie %>% acf2(140)

serie %>% diff() %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie %>% diff() %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie %>% diff() %>% aTSA::stationary.test(method = "adf")
serie %>% diff() %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie %>% diff() %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie %>% diff() %>% BoxCox.lambda()
serie %>% diff() %>% acf2(280) # Propuesta: SARIMA(1,1,1)x(3,0,0)7

serie %>% diff() %>% diff(7) %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie %>% diff() %>% diff(7) %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie %>% diff() %>% diff(7) %>% aTSA::stationary.test(method = "adf")
serie %>% diff() %>% diff(7) %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie %>% diff() %>% diff(7) %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie %>% diff() %>% diff(7) %>% BoxCox.lambda()
serie %>% diff() %>% diff(7) %>% acf2(280) # SARIMA(1,1,1)x(0,1,1)7

serie %>% auto.arima # SARIMA(1,0,2)x(0,1,1)7

ntotal = length(serie)
ntrain = 350
h      = 14
medidas0 = NULL
medidas1 = medidas2 = medidas3 = medidas4 = medidas5 = NULL
medidas6 = medidas7 = medidas8 = medidas9 = medidas10 = NULL
medidas11 = medidas12 = medidas13 = medidas14 = medidas15 = NULL

for(i in 0:(ntotal-ntrain-h)){
  training <- serie %>%  window(start = 1, end = ntrain/7 + i/7)
  testing  <- serie %>%  window(start = ntrain/7  + i/7 + 1/7, end= ntrain/7 + i/7 + 14/7)
  modelo0  <- training %>% Arima(order=c(1,1,1),seasonal=c(3,0,0)) # propuesta 0
  modelo1  <- training %>% Arima(order=c(1,1,1),seasonal=c(0,1,1)) # propuesta
  modelo2  <- training %>% Arima(order=c(1,0,2),seasonal=c(0,1,1)) # auto.arima
  modelo3  <- training %>% Arima(order=c(1,1,1),seasonal=c(0,1,0)) # propuesta Q-1
  modelo3  <- training %>% Arima(order=c(1,1,1),seasonal=c(0,1,2)) # propuesta Q+1
  modelo4  <- training %>% Arima(order=c(1,1,1),seasonal=c(1,1,1)) # propuesta P+1
  modelo5  <- training %>% Arima(order=c(0,1,1),seasonal=c(0,1,1)) # propuesta p-1
  modelo6  <- training %>% Arima(order=c(2,1,1),seasonal=c(0,1,1)) # propuesta p+1
  modelo7  <- training %>% Arima(order=c(1,1,0),seasonal=c(0,1,1)) # propuesta q-1
  modelo8  <- training %>% Arima(order=c(1,1,2),seasonal=c(0,1,1)) # propuesta q+1
  modelo9  <- training %>% Arima(order=c(1,0,2),seasonal=c(1,1,1)) # auto.arima P+1
  modelo10  <- training %>% Arima(order=c(1,0,2),seasonal=c(0,1,2)) # auto.arima Q+1
  modelo11  <- training %>% Arima(order=c(1,0,2),seasonal=c(0,1,0)) # auto.arima Q-1
  modelo12  <- training %>% Arima(order=c(2,0,2),seasonal=c(0,1,1)) # auto.arima p+1
  modelo13  <- training %>% Arima(order=c(0,0,2),seasonal=c(0,1,1)) # auto.arima p-1
  modelo14  <- training %>% Arima(order=c(1,0,3),seasonal=c(0,1,1)) # auto.arima q+1
  modelo15  <- training %>% Arima(order=c(1,0,1),seasonal=c(0,1,1)) # auto.arima q-1
  pred0    <- modelo0 %>% forecast::forecast(h=8)
  pred1    <- modelo1 %>% forecast::forecast(h=8)
  pred2    <- modelo2 %>% forecast::forecast(h=8)
  pred3    <- modelo3 %>% forecast::forecast(h=8)
  pred4    <- modelo4 %>% forecast::forecast(h=8)
  pred5    <- modelo5 %>% forecast::forecast(h=8)
  pred6    <- modelo6 %>% forecast::forecast(h=8)
  pred7    <- modelo7 %>% forecast::forecast(h=8)
  pred8    <- modelo8 %>% forecast::forecast(h=8)
  pred9    <- modelo9 %>% forecast::forecast(h=8)
  pred10   <- modelo10 %>% forecast::forecast(h=8)
  pred11   <- modelo11 %>% forecast::forecast(h=8)
  pred12   <- modelo12 %>% forecast::forecast(h=8)
  pred13   <- modelo13 %>% forecast::forecast(h=8)
  pred14   <- modelo14 %>% forecast::forecast(h=8)
  pred15   <- modelo15 %>% forecast::forecast(h=8)
  medidas0 <- rbind(medidas0, accuracy(pred0,testing)[2,])
  medidas1 <- rbind(medidas1, accuracy(pred1,testing)[2,])
  medidas2 <- rbind(medidas2, accuracy(pred2,testing)[2,])
  medidas3 <- rbind(medidas3, accuracy(pred3,testing)[2,])
  medidas4 <- rbind(medidas4, accuracy(pred4,testing)[2,])
  medidas5 <- rbind(medidas5, accuracy(pred5,testing)[2,])
  medidas6 <- rbind(medidas6, accuracy(pred6,testing)[2,])
  medidas7 <- rbind(medidas7, accuracy(pred7,testing)[2,])
  medidas8 <- rbind(medidas8, accuracy(pred8,testing)[2,])
  medidas9 <- rbind(medidas9, accuracy(pred9,testing)[2,])
  medidas10 <- rbind(medidas10, accuracy(pred10,testing)[2,])
  medidas11 <- rbind(medidas11, accuracy(pred11,testing)[2,])
  medidas12 <- rbind(medidas12, accuracy(pred12,testing)[2,])
  medidas13 <- rbind(medidas13, accuracy(pred13,testing)[2,])
  medidas14 <- rbind(medidas14, accuracy(pred14,testing)[2,])
  medidas15 <- rbind(medidas15, accuracy(pred15,testing)[2,])
}

options(scipen = 999)
medidas0 %>% colMeans %>% abs %>% rbind(
  medidas1 %>% colMeans %>% abs,
  medidas2 %>% colMeans %>% abs,
  medidas3 %>% colMeans %>% abs,
  medidas4 %>% colMeans %>% abs,
  medidas5 %>% colMeans %>% abs,
  medidas6 %>% colMeans %>% abs,
  medidas7 %>% colMeans %>% abs,
  medidas8 %>% colMeans %>% abs,
  medidas9 %>% colMeans %>% abs,
  medidas10 %>% colMeans %>% abs,
  medidas11 %>% colMeans %>% abs,
  medidas12 %>% colMeans %>% abs,
  medidas13 %>% colMeans %>% abs,
  medidas14 %>% colMeans %>% abs,
  medidas15 %>% colMeans) %>% 
  as.data.frame() %>% 
  mutate(modelo=seq(0,15)) %>% 
  as.matrix() -> medidas

library(Rfast)
colMins(medidas) # modelo 8,modelo 15 y modelo 13 

serie %>% Arima(order=c(1,1,2),seasonal=c(0,1,1)) -> modelo_8
serie %>% Arima(order=c(0,0,2),seasonal=c(0,1,1)) -> modelo_13
serie %>% Arima(order=c(1,0,1),seasonal=c(0,1,1)) -> modelo_15

modelo_8 %>% residuals -> residuales_8
modelo_13 %>% residuals -> residuales_13
modelo_15 %>% residuals -> residuales_15

modelo_8 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo SARIMA(1,1,2)x(0,1,1)7", x = "") + 
  theme_minimal() -> grafico_8

modelo_13 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo SARIMA(0,0,2)x(0,1,1)7", x = "") + 
  theme_minimal() -> grafico_13

modelo_15 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo SARIMA(1,0,1)x(0,1,1)4", x = "") + 
  theme_minimal() -> grafico_15

grid.arrange(grafico_8,grafico_13,grafico_15,ncol=3)

modelo_8 %>% t_stat
modelo_13 %>% t_stat
modelo_15 %>% t_stat

modelo_8 %>% residuals %>% t.test
modelo_13 %>% residuals %>% t.test
modelo_15 %>% residuals %>% t.test

modelo_8 %>% residuals %>% shapiro.test
modelo_13 %>% residuals %>% shapiro.test
modelo_15 %>% residuals %>% shapiro.test

modelo_8 %>% residuals %>% ad.test
modelo_13 %>% residuals %>% ad.test
modelo_15 %>% residuals %>% ad.test

modelo_8 %>% residuals %>% ks.test("pnorm")
modelo_13 %>% residuals %>% ks.test("pnorm")
modelo_15 %>% residuals %>% ks.test("pnorm")

residuales_8 %>% hist()
residuales_13 %>% hist()
residuales_15 %>% hist()

residuales_8 %>% qqnorm();residuales_2 %>% qqline()
residuales_13 %>% qqnorm();residuales_15 %>% qqline()
residuales_15 %>% qqnorm();residuales_15 %>% qqline()

library(moments)
residuales_8 %>% kurtosis
residuales_13 %>% kurtosis
residuales_15 %>% kurtosis

residuales_8 %>% TSA::acf(lag=140) 
residuales_13 %>% TSA::acf(lag=140) 
residuales_15 %>% TSA::acf(lag=140) 

residuales_8 %>% BoxCox.lambda() 
residuales_13 %>% BoxCox.lambda() 
residuales_15 %>% BoxCox.lambda() 

residuales_8 %>% aTSA::stationary.test(method="kpss")
residuales_13 %>% aTSA::stationary.test(method="kpss")
residuales_15 %>% aTSA::stationary.test(method="kpss")

residuales_8 %>% aTSA::stationary.test(method="adf")
residuales_13 %>% aTSA::stationary.test(method="adf")
residuales_15 %>% aTSA::stationary.test(method="adf")

residuales_8 %>% aTSA::stationary.test(method="pp")
residuales_13 %>% aTSA::stationary.test(method="pp")
residuales_15 %>% aTSA::stationary.test(method="pp")


# Predicción --------------------------------------------------------------

modelo_8 %>% forecast::forecast(h=14)

modelo_8 %>% 
  forecast::forecast(h=14) %>% 
  sw_sweep() %>%
  dplyr::rename(Prediccion=3) %>% 
  ggplot(aes(x = index, y = Prediccion, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point()+
  geom_line(size = 1) +
  labs(title = "Predicción Y", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()

# Valores reales reportados
# 50557
# 52305
# 53288
# 50226
# 41342
# 17889
# 44949

