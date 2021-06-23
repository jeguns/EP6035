

# Paquetes ----------------------------------------------------------------

library(sarima)
library(forecast)
library(ggplot2)
library(caschrono)
library(TSA)
library(astsa)
library(sweep)
library(gridExtra)
library(tidyquant)

# Modelo SARIMA (AR estacional y estacionario) ----------------------------

set.seed(444)
sim_sarima(model = list(sar=c(0.9,-0.32), nseasons=4),
           n     = 120) -> serie6
serie6 %>% autoplot()
serie6 %>% ts(frequency = 4) %>% autoplot()
serie6 %>% auto.arima
serie6 %>% ts(frequency = 4) %>% auto.arima
serie6 %>% ts(frequency = 4) -> serie6

serie6 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie6 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie6 %>% aTSA::stationary.test(method = "adf")
serie6 %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie6 %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie6 %>% BoxCox.lambda()
serie6 %>% acf2(72)

# Modelo SARIMA (AR estacional no estacionario) ---------------------------

set.seed(6565)
sim_sarima(model = list(sar=c(0.75,-0.6), nseasons=4, iorder=1),
           n     = 200) -> serie7
serie7 %>% ts(frequency = 4) -> serie7
serie7 %>% autoplot()
serie7 %>% auto.arima
serie7 %>% auto.arima %>% t_stat

serie7 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie7 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie7 %>% aTSA::stationary.test(method = "adf")
serie7 %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie7 %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie7 %>% BoxCox.lambda()
serie7 %>% acf2(72)

serie7 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie7 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie7 %>% diff %>% aTSA::stationary.test(method = "adf")
serie7 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie7 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie7 %>% diff %>% BoxCox.lambda()
serie7 %>% diff %>% acf2(72)

ntotal = length(serie7)
ntrain = 100
h      = 8
medidas1 = medidas2 = medidas3 = medidas4 = medidas5 = medidas6 = NULL
medidas7 = medidas8 = medidas9 = medidas10 = medidas11 = medidas12 = NULL

for(i in 0:(ntotal-ntrain-h)){
  training <- serie7 %>%  window(start = 1, end = ntrain/4 + i/4)
  testing  <- serie7 %>%  window(start = ntrain/4  + i/4 + 1/4, end= ntrain/4 + i/4 + 8/4)
  modelo1  <- training %>% Arima(order=c(0,1,0),seasonal=c(2,1,0)) # PACF se trunca en 2
  modelo2  <- training %>% Arima(order=c(1,1,1),seasonal=c(2,1,0)) # auto.arima
  modelo3  <- training %>% Arima(order=c(0,1,0),seasonal=c(3,1,0)) # propuesto P+1 
  modelo4  <- training %>% Arima(order=c(0,1,0),seasonal=c(1,1,0)) # propuesto P-1
  modelo5  <- training %>% Arima(order=c(0,1,0),seasonal=c(2,1,1)) # propuesto Q+1 
  modelo6  <- training %>% Arima(order=c(1,1,0),seasonal=c(2,1,0)) # propuesto p+1
  modelo7  <- training %>% Arima(order=c(0,1,1),seasonal=c(2,1,0)) # propuesto q+1
  modelo8  <- training %>% Arima(order=c(1,1,1),seasonal=c(3,1,0)) # auto.arima P+1
  modelo9  <- training %>% Arima(order=c(1,1,1),seasonal=c(1,1,1)) # auto.arima P-1
  modelo10  <- training %>% Arima(order=c(1,1,1),seasonal=c(2,1,1)) # auto.arima Q+1
  modelo11  <- training %>% Arima(order=c(2,1,1),seasonal=c(2,1,0)) # auto.arima p+1
  modelo12  <- training %>% Arima(order=c(1,1,2),seasonal=c(2,1,0)) # auto.arima q+1
  pred1    <- modelo1 %>% forecast::forecast(h=8)
  pred2    <- modelo2 %>% forecast::forecast(h=8)
  pred3    <- modelo3 %>% forecast::forecast(h=8)
  pred4    <- modelo4 %>% forecast::forecast(h=8)
  pred5    <- modelo5 %>% forecast::forecast(h=8)
  pred6    <- modelo6 %>% forecast::forecast(h=8)
  pred7    <- modelo7 %>% forecast::forecast(h=8)
  pred8    <- modelo8 %>% forecast::forecast(h=8)
  pred9    <- modelo9 %>% forecast::forecast(h=8)
  pred10    <- modelo10 %>% forecast::forecast(h=8)
  pred11    <- modelo11 %>% forecast::forecast(h=8)
  pred12    <- modelo12 %>% forecast::forecast(h=8)
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
}

medidas1 %>% colMeans
medidas2 %>% colMeans
medidas3 %>% colMeans
medidas4 %>% colMeans
medidas5 %>% colMeans
medidas6 %>% colMeans
medidas7 %>% colMeans
medidas8 %>% colMeans
medidas9 %>% colMeans
medidas10 %>% colMeans
medidas11 %>% colMeans
medidas12 %>% colMeans

serie7 %>% Arima(order=c(0,1,0),seasonal=c(2,1,1)) -> modelo_5
serie7 %>% Arima(order=c(1,1,1),seasonal=c(2,1,1)) -> modelo_10

modelo_5 %>% residuals -> residuales_5
modelo_10 %>% residuals -> residuales_10

modelo_5 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo SARIMA(0,1,0)x(2,1,1)4", x = "") + 
  theme_minimal() -> graf_res_5

modelo_10 %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo SARIMA(1,1,1)x(2,1,1)4", x = "") + 
  theme_minimal()-> graf_res_10

grid.arrange(graf_res_5, graf_res_10,ncol=2)

modelo_5 %>% t_stat
modelo_10 %>% t_stat

modelo_5 %>% residuals %>% t.test
modelo_10 %>% residuals %>% t.test

modelo_5 %>% residuals %>% shapiro.test
modelo_10 %>% residuals %>% shapiro.test

library(nortest)

modelo_5 %>% residuals %>% ad.test
modelo_10 %>% residuals %>% ad.test

modelo_5 %>% residuals %>% ks.test("pnorm")
modelo_10 %>% residuals %>% ks.test("pnorm")

residuales_5 %>% hist()
residuales_10 %>% hist()

residuales_5 %>% qqnorm();residuales_1 %>% qqline()
residuales_10 %>% qqnorm();residuales_2 %>% qqline()

library(moments)
residuales_5%>% kurtosis
residuales_10 %>% kurtosis

residuales_5 %>% TSA::acf(lag=72) 
residuales_10 %>% TSA::acf(lag=72) 

residuales_5 %>% BoxCox.lambda() #
residuales_10 %>% BoxCox.lambda()

residuales_5 %>% aTSA::stationary.test(method="kpss")
residuales_10 %>% aTSA::stationary.test(method="kpss")

residuales_5 %>% aTSA::stationary.test(method="adf")
residuales_10 %>% aTSA::stationary.test(method="adf")

residuales_5 %>% aTSA::stationary.test(method="pp")
residuales_10 %>% aTSA::stationary.test(method="pp")


modelo_5 %>% forecast::forecast(h=8)

modelo_5 %>% 
  forecast::forecast(h=8) %>% 
  sw_sweep() %>%
  ggplot(aes(x = index, y = value, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point()+
  geom_line(size = 1) +
  labs(title = "Predicción Y", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()

# Modelo SARIMA (completo) ------------------------------------------------

set.seed(123) #5435
sim_sarima(model = list(ar  = 0.85, ma  = -0.52,
                        sar = -0.63, sma = 0.48, 
                        nseasons = 4, 
                        iorder   = 1, siorder=1),
           n     = 150) -> serie8
serie8 %>% ts(frequency = 4) -> serie8
serie8 %>% autoplot()
serie8 %>% auto.arima # SARIMA(2,1,2)x(1,1,0)4
serie8 %>% auto.arima %>% t_stat 

serie8 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie8 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie8 %>% aTSA::stationary.test(method = "adf")
serie8 %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie8 %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie8 %>% BoxCox.lambda()
serie8 %>% acf2(72)

serie8 %>% diff() %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie8 %>% diff() %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie8 %>% diff() %>% aTSA::stationary.test(method = "adf")
serie8 %>% diff() %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie8 %>% diff() %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie8 %>% diff() %>% BoxCox.lambda()
serie8 %>% diff() %>% acf2(72) # SARIMA(1,1,1)x(2,0,0)4

serie8 %>% diff() %>% diff(12) %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie8 %>% diff() %>% diff(12) %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie8 %>% diff() %>% diff(12) %>% aTSA::stationary.test(method = "adf")
serie8 %>% diff() %>% diff(12) %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie8 %>% diff() %>% diff(12) %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie8 %>% diff() %>% diff(12) %>% BoxCox.lambda()
serie8 %>% diff() %>% diff(12) %>% acf2(72) # SARIMA(1,1,1)x(3,1,0)4

ntotal = length(serie8)
ntrain = 120
h      = 8
medidas1 = medidas2 = medidas3 = NULL

for(i in 0:(ntotal-ntrain-h)){
  training <- serie8 %>%  window(start = 1, end = ntrain/4 + i/4)
  testing  <- serie8 %>%  window(start = ntrain/4  + i/4 + 1/4, end= ntrain/4 + i/4 + 8/4)
  modelo1  <- training %>% Arima(order=c(2,1,2),seasonal=c(1,1,0)) # auto.arima
  modelo2  <- training %>% Arima(order=c(1,1,1),seasonal=c(2,0,0)) # propuesta 1
  modelo3  <- training %>% Arima(order=c(1,1,1),seasonal=c(3,1,0)) # propuesta 2
  pred1    <- modelo1 %>% forecast::forecast(h=8)
  pred2    <- modelo2 %>% forecast::forecast(h=8)
  pred3    <- modelo3 %>% forecast::forecast(h=8)
  medidas1 <- rbind(medidas1, accuracy(pred1,testing)[2,])
  medidas2 <- rbind(medidas2, accuracy(pred2,testing)[2,])
  medidas3 <- rbind(medidas3, accuracy(pred3,testing)[2,])
}

medidas1 %>% colMeans
medidas2 %>% colMeans
medidas3 %>% colMeans

serie8 %>% Arima(order=c(1,1,1),seasonal=c(2,0,0)) -> modelo_final

modelo_final %>% residuals -> residuales_final

modelo_final %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo SARIMA(0,0,0)x(3,1,0)4", x = "") + 
  theme_minimal() 

modelo_final %>% t_stat

modelo_final %>% residuals %>% t.test

modelo_final %>% residuals %>% shapiro.test

modelo_final %>% residuals %>% ad.test

modelo_final %>% residuals %>% ks.test("pnorm")

residuales_final %>% hist()

residuales_final %>% qqnorm();residuales_final %>% qqline()

library(moments)
residuales_final %>% kurtosis

residuales_final %>% TSA::acf(lag=80) 

residuales_final %>% BoxCox.lambda() 

residuales_final %>% aTSA::stationary.test(method="kpss")
residuales_final %>% aTSA::stationary.test(method="adf")
residuales_final %>% aTSA::stationary.test(method="pp")

residuales_final %>% forecast::forecast(h=8)

modelo_final %>% 
  forecast::forecast(h=8) %>% 
  sw_sweep() %>%
  ggplot(aes(x = index, y = value, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point()+
  geom_line(size = 1) +
  labs(title = "Predicción Y", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()






