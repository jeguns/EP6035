
# Paquetes ----------------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(funtimes) # función notrend_test
library(trend) # función cs.test
library(randtests) # función cox.stuart.test
library(zoo)
library(smooth) # Usaremos sma en vez de pracma
library(forecast)
library(nortest)
library(DescTools) # funciones RMSE,MAPE,MAE

# Lectura datos 1 ---------------------------------------------------------

datos3 = read_csv('daily-min-temperatures.txt')

# Análisis exploratorio  --------------------------------------------------

datos3 %>% 
  ggplot(aes(x=Date,y=Temp))+
  geom_line()+
  labs(x="Tiempo",
       y="°C",
       title = "Temperatura mínima diaria (Melbourne Australia)",
       caption = "Fuente: Australian Bureau of Meteorology")+
  theme_minimal()

datos3 %>% 
  mutate(Mes = as.yearmon(Date)) %>% 
  group_by(Mes) %>% 
  summarise(TM = mean(Temp)) -> datos3a

datos3a %>% 
  ggplot(aes(x=Mes,y=TM))+
  geom_line()+
  labs(x="Tiempo",
       y="°C",
       title = "Temperatura mínima media mensual (Melbourne Australia)",
       caption = "Fuente: Australian Bureau of Meteorology")+
  theme_minimal()

ts(datos3a$TM, frequency = 12, start = c(1981,1)) -> serie3

autoplot(TSA::acf(serie3, type = "correlation", lag = 60, plot = FALSE))
serie3 %>% cor.test(1:length(serie3), method = "spearman")
notrend_test(serie3)$p.value
notrend_test(serie3,test = "MK", B = 1e4)$p.value
serie3 %>% cs.test
serie3 %>% cox.stuart.test()
serie3 %>% cox.stuart.test(alternative = c("left.sided"))
serie3 %>% cox.stuart.test(alternative = c("right.sided"))
notrend_test(serie3, test = "WAVK")$p.value

serie3 %>% seasonplot
serie3 %>% monthplot 
boxplot(serie3 ~ cycle(serie3), col = "gold")
kruskal.test(serie3 ~ cycle(serie3))


# Suavización exponencial de Holt Winters ---------------------------------

# Cómo funciona
# ~~~~~~~~~~~~~

serie3 %>% 
  HoltWinters(alpha = NULL, beta = NULL, gamma = NULL, seasonal = "additive")

serie3 %>% 
  HoltWinters(alpha = NULL, beta = NULL, gamma = NULL, seasonal = "multiplicative")

# Data split
# ~~~~~~~~~~

ntotal = length(serie3)  # longitud de la serie
ntrain = 96 # longitud data de entrenamiento
nvalid = 12 # longitud data de validación
ntest  = 12 # longitud data de prueba

train  = serie3 %>% window(start = 1981, end = 1981+(ntrain-1)/12)
valid  = serie3 %>% window(start = 1981+(ntrain)/12, end = 1981+(ntrain+nvalid-1)/12)
test   = serie3 %>% window(start = 1981+(ntrain+nvalid)/12)


# Tuning: alfa, beta y gamma - aditivo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rmse = mape = mae = theil = NULL
alfa = seq(0.01,0.99, by = 0.05)
beta = seq(0.01,0.99, by = 0.05)
gama = seq(0.01,0.99, by = 0.05)
abg  = expand.grid(alfa,beta,gama)
for(i in 1:nrow(abg)){
  fit     = HoltWinters(train, alpha = abg[i,1], beta = abg[i,2], gamma = abg[i,3], seasonal = "additive")
  pred    = predict(fit, n.ahead = 15)
  rmse[i] = DescTools::RMSE(pred,valid, na.rm=T) # accuracy(pred,valid)[2]
  mae[i]  = DescTools::MAE(pred,valid, na.rm=T)  # accuracy(pred,valid)[3]
  mape[i] = DescTools::MAPE(pred,valid, na.rm=T) # accuracy(pred,valid)[5]
  theil[i] = accuracy(pred,valid)[7]
}


coef.fit  = tibble(abg, rmse,mae,mape,theil)
(coef.min = filter(coef.fit, rmse == min(rmse)))
(alfa.opt <- coef.min[1] %>% as.numeric) # decidimos que alfa = 0.41
(beta.opt <- coef.min[2] %>% as.numeric) # decidimos que beta = 0.91
(gama.opt <- coef.min[3] %>% as.numeric) # decidimos que beta = 0.16

mod.val = HoltWinters(ts(c(train,valid),frequency=12), 
                      alpha = alfa.opt, beta = beta.opt, gamma = gama.opt, seasonal = "additive") 
y.obs = ts(c(train,valid))
y.est = ts(c(rep(NA,12),mod.val$fitted[,1]))
data.frame(y.obs,y.est)

x11();autoplot(ts.union(y.obs,y.est),
               facets = FALSE) +
  geom_point()+
  labs(x       = "Tiempo", 
       y       = "°C", 
       title   = "Evolución de la temperatura en Melbourne (Australia)",
       caption = "") +
  scale_colour_manual(labels = c("Serie observada","Predicción"), 
                      values = c("darkblue", "red")) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text  = element_text(color = "deepskyblue4"))

resid = (y.obs - y.est)[-seq(1,12,1)]

resid %>% plot(type="b", pch=18)

resid %>% mean(na.rm=T)
resid %>% t.test # PH de la media: H0: mu_error = 0

resid %>% archTest(lag=10)
McLeod.Li.test(y = resid)

autoplot(TSA::acf(resid, lag = 36, plot = F))

resid %>% 
  as.data.frame() %>% 
  ggplot(aes(sample=resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

resid %>% qplot(geom = "histogram", 
                bins = round(1 + 3.3*log10(length(resid))),
                fill = I("darkblue")) +
  theme_minimal()

resid %>% shapiro.test
resid %>% ad.test


# Tuning: alfa, beta y gamma - multiplicativo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rmse = mape = mae = theil = NULL
alfa = seq(0.01,0.99, by = 0.05)
beta = seq(0.01,0.99, by = 0.05)
gama = seq(0.01,0.99, by = 0.05)
abg  = expand.grid(alfa,beta,gama)
for(i in 1:nrow(abg)){
  fit     = HoltWinters(train, alpha = abg[i,1], beta = abg[i,2], gamma = abg[i,3], seasonal = "multiplicative")
  pred    = predict(fit, n.ahead = 15)
  rmse[i] = DescTools::RMSE(pred,valid, na.rm=T) # accuracy(pred,valid)[2]
  mae[i]  = DescTools::MAE(pred,valid, na.rm=T)  # accuracy(pred,valid)[3]
  mape[i] = DescTools::MAPE(pred,valid, na.rm=T) # accuracy(pred,valid)[5]
  theil[i] = accuracy(pred,valid)[7]
}

coef.fit  = tibble(abg, rmse,mae,mape,theil)
(coef.min = filter(coef.fit, rmse == min(rmse)))
(alfa.opt <- coef.min[1] %>% as.numeric) # decidimos que alfa = 0.46
(beta.opt <- coef.min[2] %>% as.numeric) # decidimos que beta = 0.91
(gama.opt <- coef.min[3] %>% as.numeric) # decidimos que beta = 0.16

mod.val = HoltWinters(ts(c(train,valid),frequency=12), 
                      alpha = alfa.opt, beta = beta.opt, gamma = gama.opt, seasonal = "multiplicative") 
y.obs = ts(c(train,valid))
y.est = ts(c(rep(NA,12),mod.val$fitted[,1]))
data.frame(y.obs,y.est)

x11();autoplot(ts.union(y.obs,y.est),
               facets = FALSE) +
  geom_point()+
  labs(x       = "Día", 
       y       = "S/", 
       title   = "Evolución de valores cuota",
       caption = "") +
  scale_colour_manual(labels = c("Serie observada","Predicción"), 
                      values = c("darkblue", "red")) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text  = element_text(color = "deepskyblue4"))

resid = (y.obs - y.est)[-seq(1,12,1)]

resid %>% plot(type="b", pch=18)

resid %>% mean(na.rm=T)
resid %>% t.test # PH de la media: H0: mu_error = 0

resid %>% archTest(lag=36)
McLeod.Li.test(y = resid, gof.lag=36)

autoplot(TSA::acf(resid, lag = 36, plot = F))

resid %>% 
  as.data.frame() %>% 
  ggplot(aes(sample=resid))+
  stat_qq() +
  stat_qq_line(distribution = "qnorm")+
  labs(x = "cuantil teórico",
       y = "residuales")+
  theme_minimal()

resid %>% qplot(geom = "histogram", 
                bins = round(1 + 3.3*log10(length(resid))),
                fill = I("darkblue")) +
  theme_minimal()

resid %>% shapiro.test
resid %>% ad.test

# Predicción : ¿cuál elegimos?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mod.comp = HoltWinters(serie3, alpha = alfa.opt, beta = beta.opt, gamma = gama.opt, seasonal = "multiplicative") 
predict(mod.comp, n.ahead=12, prediction.interval = T)

mod.comp = HoltWinters(serie3, alpha = alfa.opt, beta = beta.opt, gamma = gama.opt, seasonal = "additive") 
predict(mod.comp, n.ahead=12, prediction.interval = T)
