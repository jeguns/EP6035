
# Paquetes ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(funtimes) # función notrend_test
library(trend) # función cs.test
library(randtests) # función cox.stuart.test
library(zoo)
library(pracma)
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

# Media móvil  ------------------------------------------------------------

# Cómo funciona
# ~~~~~~~~~~~~~

serie1 %>% movavg(n = 3, type="s") # Recomiendo
serie1 %>% rollmean(k = 3)
serie1 %>% ma(order = 3)
serie1 %>% ma(order = 3, centre=FALSE)

serie1 %>% movavg(n = 4, type="s")
serie1 %>% rollmean(k = 4)
serie1 %>% ma(order = 4)
serie1 %>% ma(order = 4, centre=FALSE)

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
  fit       = movavg(train, n = r, type="s")
  pred      = rep(fit[length(fit)],nvalid)
  rmse[r-1] = RMSE(pred,valid, na.rm=T)
  mape[r-1] = MAPE(pred,valid, na.rm=T)
  mae[r-1]  = MAE(pred,valid, na.rm=T)
}

data.frame(r = 2:12, rmse, mape, mae) # decidimos que r=9

y.obs = ts(c(train,valid,NA))
y.sua = movavg(ts(c(train,valid)), n=9, type="s")
y.est = ts(c(NA,y.sua))

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

resid %>% mean(na.rm=T)
resid %>% t.test # PH de la media: H0: mu_error = 0

resid[-c(1,28)] %>% archTest(lag=10)
McLeod.Li.test(y = resid[-c(1,28)])

autoplot(TSA::acf(resid[-c(1,28)], lag = 21, plot = F))

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
resid %>% ks.test("pnorm")

# Comparando con el data test
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

test
pred.test  = rep(y.est[length(y.est)], length(test))
pred.resid = test - pred.test
pred.resid %>% autoplot

MAE(test,pred.test, na.rm=T)
mean(abs(pred.resid), na.rm=T)

MAPE(test,pred.test, na.rm=T)
mean(abs(pred.resid)/pred.test, na.rm=T)

RMSE(test,pred.test, na.rm=T)
sqrt(mean(pred.resid^2, na.rm=T))

# Predicción
# ~~~~~~~~~~

y.suavizado = movavg(serie1, n=9, type="s")


y.obs = ts(c(serie1,NA))
y.est = ts(c(NA,y.suavizado))
data.frame(y.obs,y.est)
residual = y.obs - y.est

y.h1    = y.suavizado[length(y.suavizado)]
valorz  = qnorm(0.975) # requiere que se haya verificado previamente normalidad
sigma2e = var(resid, na.rm = T)
LIPR    = y.h1 - valorz*sqrt(sigma2e/r)
LSPR    = y.h1 + valorz*sqrt(sigma2e/r)
IPR     = data.frame("Lim Inferior"   = LIPR, 
                     "Estim Puntual"  = y.h1,
                     "Lim Superior"   = LSPR)
IPR
