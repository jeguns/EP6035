
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
library(TSA)
library(MTS) # función archTest

# Lectura datos 1 ---------------------------------------------------------

datos1 = read_excel('MolecularesAbril.xlsx')

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

# Suavización exponencial simple ------------------------------------------

# Cómo funciona
# ~~~~~~~~~~~~~

ts(c(3,2,4,3,2,3,3,2,3,3,4,3,3,2,4)) %>% 
  HoltWinters(alpha = NULL, beta = FALSE, gamma = FALSE)

serie1 %>% 
  HoltWinters(alpha = NULL, beta = FALSE, gamma = FALSE) -> mod.com

(serie1[-1] - mod.com$fitted[,1] -> resi.com)

(RMSE(serie1,mod.com$fitted[,1]) -> rmse.com) # 6235.781

forecast(mod.com, h = 3)

# Data split
# ~~~~~~~~~~

ntotal = length(serie1)  # longitud de la serie
ntrain = 24 # longitud data de entrenamiento
nvalid = 3  # longitud data de validación
ntest  = 3  # longitud data de prueba

train  = serie1 %>% window(start = 1, end = 1+(ntrain-1)/7)
valid  = serie1 %>% window(start = 1+(ntrain)/7, end = 1+(ntrain+nvalid-1)/7)
test   = serie1 %>% window(start = 1+(ntrain+nvalid)/7)

# Tuning: alfa
# ~~~~~~~~~~~~

rmse = mape = mae = theil = NULL
alfa = seq(0.001,0.999, by = 0.001)
for(i in seq_along(alfa)){
  fit     = HoltWinters(train, alpha = alfa[i], beta = F, gamma = F)
  pred    = predict(fit, n.ahead = 3) 
  rmse[i] = DescTools::RMSE(pred,valid, na.rm=T) # accuracy(pred,valid)[2]
  mae[i]  = DescTools::MAE(pred,valid, na.rm=T)  # accuracy(pred,valid)[3]
  mape[i] = DescTools::MAPE(pred,valid, na.rm=T) # accuracy(pred,valid)[5]
  theil[i] = accuracy(pred,valid)[7]
}

alfa.fit  = tibble(alfa, rmse,mae,mape,theil)
(alfa.min = filter(alfa.fit, rmse == min(rmse)))
alfa.fit %>% 
  ggplot(aes(alfa, rmse)) +
  geom_line() +
  geom_point(data = alfa.min, aes(alfa, rmse), size = 2, color = "blue")+
  theme_minimal()
(alfa.opt <- alfa.min[1] %>% as.numeric) # decidimos que alfa = 0.179

mod.val = HoltWinters(ts(c(train,valid),frequency=7), 
                      alpha = alfa.opt, beta = F, gamma = F) 
y.obs = ts(c(train,valid))
y.est = ts(c(NA,mod.val$fitted[,2]))
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

resid = (y.obs - y.est)[-1]

resid %>% plot

resid %>% mean(na.rm=T)
resid %>% t.test # PH de la media: H0: mu_error = 0

resid %>% archTest(lag=10)
McLeod.Li.test(y = resid)

autoplot(TSA::acf(resid, lag = 21, plot = F))

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
#resid %>% ks.test("pnorm")

# Comparando con el data test
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

test
(pred.test   = predict(mod.val, n.ahead = ntest))
data.frame(test,pred.test,resid=test-pred.test)
DescTools::RMSE(test,pred.test, na.rm=T)
accuracy(test,pred.test)

# Predicción
# ~~~~~~~~~~

mod.comp = HoltWinters(serie1, alpha = alfa.opt, beta = F, gamma = F) 
predict(mod.comp, n.ahead=3, prediction.interval = T)


