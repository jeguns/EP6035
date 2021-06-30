

# Paquetes ----------------------------------------------------------------

library(dplyr)
library(astsa)
library(forecast)
library(readxl)
library(sweep)
library(timetk)
library(ggplot2)

# Ejemplos introductorios -------------------------------------------------

arima.sim(n= 250, model = list(ar=c(0.5,-0.3))) -> serie_ar2
serie_ar2 %>% acf2()
serie_ar2 %>% autoplot()
serie_ar2 %>% nnetar() 
serie_ar2 %>% nnetar(p=2) 
serie_ar2 %>% nnetar(p=2,size=1)

arima.sim(n= 250, model = list(ar=c(0.8,-0.1,-0.6,0.4,-0.5,0.3))) -> serie_ar6
serie_ar6 %>% acf2()
serie_ar6 %>% autoplot()
serie_ar6 %>% nnetar() 

arima.sim(n= 200, model = list(ma=c(0.9))) -> serie_ma
serie_ma %>% acf2()
serie_ma %>% autoplot()
serie_ma %>% nnetar() 

# Ejemplo con serie real --------------------------------------------------

read_excel('DowJones.xlsx', skip=2,
           col_names=c('Dia','Indice'),
           col_types = c("text","numeric")) -> datos

Indice <- ts(datos$Indice[5657:6387], frequency=5)
Indice %>% is.na() %>% sum()
Indice %>% na.locf() -> Indice
Indice %>% autoplot()
Indice %>% is.na() %>% sum()
Indice %>% ggseasonplot()
Indice %>% monthplot
Indice %>% acf2(100)

Indice <- ts(datos$Indice[5657:6387], frequency=1)
Indice %>% na.locf() -> Indice

Indice %>% nnetar() -> modelo1
modelo1
modelo1 %>% sw_tidy()
modelo1 %>% sw_glance()
modelo1 %>% sw_augment() 
modelo1 %>% sw_augment() %>% select(.resid) %>% filter(!is.na(.resid)) %>% ts() -> residuales_1
residuales_1 %>% checkresiduals()
library(moments)
residuales_1 %>% skewness()
residuales_1 %>% kurtosis()

Indice %>% BoxCox.lambda()
Indice %>% nnetar(lambda = Indice %>% BoxCox.lambda()) -> modelo2
modelo2
modelo2 %>% sw_tidy()
modelo2 %>% sw_glance()
modelo2 %>% sw_augment() 
modelo2 %>% sw_augment() %>% select(.resid) %>% filter(!is.na(.resid)) %>% ts() -> residuales_2
residuales_2 %>% checkresiduals()
library(moments)
residuales_2 %>% skewness()
residuales_2 %>% kurtosis()

modelo2 %>% forecast::forecast(h=12,level = c(90, 95)) -> predicciones
predicciones
predicciones %>% tk_tbl()
predicciones %>% autoplot

sim <- ts(matrix(0, nrow=12, ncol=40), start=end(Indice)[1]+1)
for(i in 1:40){
  sim[,i] <- modelo2 %>% simulate(nsim=12)}

Indice %>% 
  autoplot() + 
  forecast::autolayer(sim)

modelo2 %>% forecast::forecast(h=12,level = c(90, 95),PI=TRUE,npaths=40) -> predicciones_int
predicciones_int
predicciones_int %>% tk_tbl()
predicciones_int %>% autoplot



Indice_Train = window(Indice,start = 1, end = 650)
Indice_Test  = window(Indice,start = 651)

Indice_Train %>% nnetar() -> modelo_train1
modelo_train1
modelo_train1 %>% forecast::forecast(h=81, PI=TRUE) -> predicciones_train1
accuracy(predicciones_train1, Indice_Test)
autoplot(Indice_Test) +
  autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="gray60") + 
  theme_minimal() +
  ylab("Índice")

Indice_Train %>% BoxCox.lambda()
Indice_Train %>% nnetar(lambda = 0.6605) -> modelo_train2
modelo_train2
modelo_train2 %>% forecast::forecast(h=81, PI=TRUE) -> predicciones_train2
accuracy(predicciones_train2, Indice_Test)
autoplot(Indice_Test) +
  autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
            alpha = 0.3, size= 1.25, color="gray60") +
  theme_minimal() +
  ylab("Índice")
