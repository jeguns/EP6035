
# Paquetes ----------------------------------------------------------------

library(readxl)
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

datos1 = read_excel('MolecularesAbril.xlsx')

# Análisis exploratorio  --------------------------------------------------

datos1 %>% 
  mutate(Fecha = ymd(Fecha)) %>% 
  ggplot(aes(x=Fecha,y=PCR))+
  geom_line()+
  scale_y_continuous(limits=c(0,40000))+
  labs(x="Día",
       y="Cantidad",
       title = "Número de pruebas PCR COVID-19 a nivel nacional",
       caption = "Fuente: MINSA")+
  theme_minimal()

ts(datos1$PCR, frequency = 7) -> serie1

autoplot(TSA::acf(serie1, type = "correlation", lag = 24, plot = FALSE))
serie1 %>% cor.test(1:length(serie1), method = "spearman")
notrend_test(serie1)$p.value
notrend_test(serie1,test = "MK", B = 1e4)$p.value
serie1 %>% cs.test
serie1 %>% cox.stuart.test()
serie1 %>% cox.stuart.test(alternative = c("left.sided"))
serie1 %>% cox.stuart.test(alternative = c("right.sided"))
notrend_test(serie1, test = "WAVK")$p.value

serie1 %>% seasonplot
serie1 %>% monthplot 
boxplot(serie1 ~ cycle(serie1), col = "gold")
kruskal.test(serie1 ~ cycle(serie1))

# Usando función sma ------------------------------------------------------

1:10 %>% sma(order = 3, intervals="parametric",h=3) -> prueba
data.frame(variable = 1:10, prueba$fitted)

# Data split
# ~~~~~~~~~~

#https://medium.com/@soumyachess1496/cross-validation-in-time-series-566ae4981ce4
ntotal = length(serie1)  # longitud de la serie
ntrain = 24 # longitud data de entrenamiento
nvalid = 3  # longitud data de validación
ntest  = 3  # longitud data de prueba

train  = serie1 %>% window(start = 1, end = 1+(ntrain-1)/7)
valid  = serie1 %>% window(start = 1+(ntrain)/7, end = 1+(ntrain+nvalid-1)/7)
test   = serie1 %>% window(start = 1+(ntrain+nvalid)/7)

# Tuning: r
# ~~~~~~~~~~ 

rmse = mape = mae = NULL
for(r in 2:12){
  fit       = sma(train, order = r, h = nvalid)
  pred      = fit$forecast
  rmse[r-1] = DescTools::RMSE(pred,valid, na.rm=T)
  mape[r-1] = DescTools::MAPE(pred,valid, na.rm=T)
  mae[r-1]  = DescTools::MAE(pred,valid, na.rm=T)
}

data.frame(r = 2:12, rmse, mape, mae) # decidimos que r=9

# Trabajamos con los datos de train + valid
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

y.obs = ts(c(train,valid))
y.est = sma(c(train,valid), order = 9)$fitted
data.frame(y.obs,y.est)

x11();autoplot(ts.union(y.obs,y.est),
               facets = FALSE) +
  geom_point()+
  labs(x       = "Día", 
       y       = "Número de pruebas", 
       title   = "Pruebas moleculares de detección COVID-19 a nivel nacional",
       caption = "Fuente: MINSA") +
  scale_colour_manual(labels = c("Serie observada","Predicción"), 
                      values = c("darkblue", "red")) + 
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text  = element_text(color = "deepskyblue4"))

resid = y.obs - y.est

# cov(y.obs,y.est)
# var(y.obs)
# var(y.est)

# matrix(c(14620721,9777794,9777794,45220813),nrow=2)->matriz
# matriz %>% det
# cor(y.obs,resid)

resid %>% mean(na.rm=T)
resid %>% t.test # PH de la media: H0: mu_error = 0

library(MTS)
library(TSA)
resid %>% archTest(lag=10)
McLeod.Li.test(y = resid)

autoplot(TSA::acf(resid, lag = 21, plot = F))

resid %>% ggplot(aes(sample=resid))+
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
#resid %>% ks.test("pnorm")

# Comparando con el data test
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

test
pred.test  = ts(sma(c(train,valid), order = 9, h = ntest)$forecast,
                start=c(4,7), frequency = 7)
pred.resid = test - pred.test
data.frame(test,pred.test,pred.resid)
pred.resid %>% autoplot

DescTools::MAE(test,pred.test)
mean(abs(pred.resid), na.rm=T)

DescTools::MAPE(test,pred.test)
mean(abs(pred.resid)/pred.test, na.rm=T)

DescTools::RMSE(test,pred.test)
sqrt(mean(pred.resid^2, na.rm=T))

# Predicción
# ~~~~~~~~~~

prediccion = sma(serie1, order = 9, h = 3, interval = "p", level = .95)
data.frame(prediccion$lower, prediccion$forecast, prediccion$upper)


