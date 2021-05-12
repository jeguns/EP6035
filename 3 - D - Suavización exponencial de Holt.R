
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

# Lectura datos 2 ---------------------------------------------------------

datos2 = read_excel('ValoresCuota.xlsx')

# Análisis exploratorio  --------------------------------------------------

datos2 %>% 
  ggplot(aes(x=Dia,y=VC))+
  geom_line()+
  labs(x="Día",
       y="S/",
       title = "Evolución de valores cuota de un fondo mutuo",
       caption = "")+
  theme_minimal()

ts(datos2$VC) -> serie2

autoplot(TSA::acf(serie2, type = "correlation", lag = 24, plot = FALSE))
serie2 %>% cor.test(1:length(serie2), method = "spearman")
notrend_test(serie2)$p.value
notrend_test(serie2,test = "MK", B = 1e4)$p.value
serie2 %>% cs.test
serie2 %>% cox.stuart.test()
serie2 %>% cox.stuart.test(alternative = c("left.sided"))
serie2 %>% cox.stuart.test(alternative = c("right.sided"))
notrend_test(serie2, test = "WAVK")$p.value


# Suavización exponencial de Holt -----------------------------------------

# Cómo funciona
# ~~~~~~~~~~~~~

serie2 %>% 
  HoltWinters(alpha = NULL, beta = NULL, gamma = FALSE)

# Data split
# ~~~~~~~~~~

ntotal = length(serie2)  # longitud de la serie
ntrain = 60 # longitud data de entrenamiento
nvalid = 12  # longitud data de validación
ntest  = 12  # longitud data de prueba

train  = serie2 %>% window(start = 1, end = ntrain)
valid  = serie2 %>% window(start = ntrain+1, end = ntrain+nvalid)
test   = serie2 %>% window(start = ntrain+nvalid+1)

# Tuning: alfa y beta
# ~~~~~~~~~~~~~~~~~~~~

rmse = mape = mae = theil = NULL
alfa = seq(0.01,0.99, by = 0.01)
beta = seq(0.01,0.99, by = 0.01)
albe = expand.grid(alfa,beta)
for(i in 1:nrow(albe)){
  fit     = HoltWinters(train, alpha = albe[i,1], beta = albe[i,2], gamma = F)
  pred    = predict(fit, n.ahead = 15)
  rmse[i] = DescTools::RMSE(pred,valid, na.rm=T) # accuracy(pred,valid)[2]
  mae[i]  = DescTools::MAE(pred,valid, na.rm=T)  # accuracy(pred,valid)[3]
  mape[i] = DescTools::MAPE(pred,valid, na.rm=T) # accuracy(pred,valid)[5]
  theil[i] = accuracy(pred,valid)[7]
}

coef.fit  = tibble(albe, rmse,mae,mape,theil)
(coef.min = filter(coef.fit, rmse == min(rmse)))
(alfa.opt <- coef.min[1] %>% as.numeric) # decidimos que alfa = 0.12
(beta.opt <- coef.min[2] %>% as.numeric) # decidimos que beta = 0.53

mod.val = HoltWinters(ts(c(train,valid)), 
                      alpha = alfa.opt, beta = beta.opt, gamma = F) 
y.obs = ts(c(train,valid))
y.est = ts(c(NA,NA,mod.val$fitted[,1]))
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

resid = (y.obs - y.est)[-c(1,2)]

resid %>% plot(type="b", pch=18)

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

# Comparando con el data test
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

test
(pred.test   = predict(mod.val, n.ahead = ntest))
data.frame(test,pred.test,pred.resid=test-pred.test)
DescTools::RMSE(test,pred.test, na.rm=T)
accuracy(test,pred.test)


# Predicción
# ~~~~~~~~~~

mod.comp = HoltWinters(serie2, alpha = alfa.opt, beta = beta.opt, gamma = F) 
predict(mod.comp, n.ahead=7, prediction.interval = T)

