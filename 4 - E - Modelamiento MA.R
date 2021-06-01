
library(readxl)

datos = read_excel('RecuperadosAbril.xlsx')
ts(datos$Recuperados) -> serie2

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


ntotal = length(serie2)
ntrain = 15
h      = 4
medidas1 = medidas2 = medidas3 = medidas4 = NULL

for(i in 0:(ntotal-ntrain-h)){
  training <- serie2 %>%  window(start = 1, end = ntrain + i)
  testing  <- serie2 %>%  window(start = ntrain + i + 1, end= ntrain + i + 4)
  modelo1  <- training %>% Arima(order=c(0,1,0))
  modelo2  <- training %>% Arima(order=c(0,1,1))
  modelo3  <- training %>% Arima(order=c(0,1,2))
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

# Escriba el modelo final:

serie2 %>% Arima(order = c(0,1,1)) -> modelofinal

modelofinal %>% sw_tidy()


# Diagnóstico -------------------------------------------------------------

modelofinal %>% residuals -> residuales

residuales %>% autoplot + 
  geom_hline(yintercept=0,col="red") + 
  labs(title = "Residuales del modelo ARIMA(0,1,1)", x = "")+
  theme_bw()

modelofinal %>% sw_augment() %>% select(.resid) -> residuales_tibble

modelofinal %>%
  sw_augment() %>% 
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Residuales del modelo ARIMA(0,1,1)", x = "") + 
  theme_minimal()

residuales %>% shapiro.test()
residuales %>% TSA::acf()
residuales %>% McLeod.Li.test(y=.)
residuales %>% BoxCox.lambda()
residuales %>% aTSA::stationary.test(method="kpss")
residuales %>% aTSA::stationary.test(method="adf")
residuales %>% aTSA::stationary.test(method="pp")

# Predicciones ------------------------------------------------------------

modelofinal %>% forecast(h=4)

modelofinal %>% forecast(h=4) %>% autoplot +
  labs(x = "Día",
       y = "Recuperados") + 
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
  labs(title = "Predicción Recuperados", x = "", y = "°C") +
  scale_color_tq() +
  theme_tq()

