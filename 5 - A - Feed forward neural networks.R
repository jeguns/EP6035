

# Paquetes ----------------------------------------------------------------

library(dplyr)
library(astsa)
library(forecast)
library(readxl)
library(sweep)
library(timetk)
library(ggplot2)
library(zoo)
library(gridExtra)

# Ejemplos introductorios -------------------------------------------------

arima.sim(n= 250, model = list(ar=c(0.5,-0.3))) -> serie_ar2
serie_ar2 %>% acf2()
serie_ar2 %>% autoplot()
serie_ar2 %>% nnetar() 
serie_ar2 %>% nnetar(p=3) 
serie_ar2 %>% nnetar(p=5,size=3)

arima.sim(n= 250, model = list(ar=c(0.8,-0.1,-0.6,0.4,-0.5,0.3))) -> serie_ar6
serie_ar6 %>% acf2()
serie_ar6 %>% autoplot()
serie_ar6 %>% nnetar() 

arima.sim(n= 200, model = list(ma=c(0.9))) -> serie_ma
serie_ma %>% acf2()
serie_ma %>% autoplot()
serie_ma %>% nnetar() 

# Ejemplo con serie real --------------------------------------------------

read_excel('DowJones.xlsx', skip=4,
           col_names=c('Dia','Indice'),
           col_types = c("text","numeric")) -> datos

datos %>% nrow
ts(datos$Indice[5386:6385], frequency=5) -> Indice
Indice %>% is.na() %>% sum()
Indice %>% na.locf() -> Indice
Indice %>% autoplot()
Indice %>% is.na() %>% sum()
Indice %>% ggseasonplot()
Indice %>% monthplot
Indice %>% acf2(100)

ts(datos$Indice[5386:6385], frequency=1) -> Indice
Indice %>% na.locf() -> Indice
Indice %>% is.na() %>% sum()
Indice %>% acf2(100)

set.seed(159753)
Indice %>% nnetar() -> modelo1
modelo1
modelo1 %>% sw_tidy()
modelo1 %>% sw_glance()
modelo1 %>% sw_augment() 
modelo1 %>% sw_augment() %>% View
modelo1 %>% sw_augment() %>% select(.resid) %>% filter(!is.na(.resid)) %>% ts() -> residuales_1
residuales_1 %>% checkresiduals()
library(moments)
residuales_1 %>% skewness()
residuales_1 %>% kurtosis()

set.seed(159753)
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

Indice %>% acf2(100)
set.seed(159753)
Indice %>% nnetar(p=9) -> modelo3
modelo3
modelo3 %>% sw_tidy()
modelo3 %>% sw_glance()
modelo3 %>% sw_augment() 
modelo3 %>% sw_augment() %>% select(.resid) %>% filter(!is.na(.resid)) %>% ts() -> residuales_3
residuales_3 %>% checkresiduals()
library(moments)
residuales_3 %>% skewness()
residuales_3 %>% kurtosis()

modelo1 %>% forecast::forecast(h=12) -> predicciones
predicciones
predicciones %>% autoplot
predicciones %>% tk_tbl()

ts(matrix(0, nrow=12, ncol=40), start=end(Indice)[1]+1) -> sim
for(i in 1:40){
  modelo1 %>% simulate(nsim=12) -> sim[,i]}

sim[,20]

Indice %>% 
  autoplot() + 
  forecast::autolayer(sim)

modelo1 %>% forecast::forecast(h=12,level = c(90, 95),PI=TRUE,npaths=40) -> predicciones_int
predicciones_int
predicciones_int %>% tk_tbl()
predicciones_int %>% autoplot

# Training y Testing

Indice_Train = window(Indice,start = 1, end = 900)
Indice_Test  = window(Indice,start = 901)

set.seed(159753)
Indice_Train %>% nnetar() -> modelo_train1
modelo_train1
modelo_train1 %>% forecast::forecast(h=100, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train1
accuracy(predicciones_train1, Indice_Test)
autoplot(Indice_Test) +
  autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo1
forecast::autoplot(Indice_Test) +
  capa_modelo1 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-3,3))+
  theme_minimal() -> grafica_modelo1

Indice_Train %>% BoxCox.lambda()
Indice_Train %>% nnetar(lambda = Indice_Train %>% BoxCox.lambda()) -> modelo_train2
modelo_train2
modelo_train2 %>% forecast::forecast(h=100, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train2
accuracy(predicciones_train2, Indice_Test)
autoplot(Indice_Test) +
  autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1.25, color="blue") +
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1.25, color="blue") -> capa_modelo2
forecast::autoplot(Indice_Test) +
  capa_modelo2 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-3,3))+
  theme_minimal() -> grafica_modelo2

Indice_Train %>% acf2
Indice_Train %>% nnetar(p=9) -> modelo_train3
modelo_train3
modelo_train3 %>% forecast::forecast(h=100, PI=TRUE) -> predicciones_train3
accuracy(predicciones_train3, Indice_Test)
autoplot(Indice_Test) +
  autolayer(predicciones_train3, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train3, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo3
forecast::autoplot(Indice_Test) +
  capa_modelo3 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-3,3))+
  theme_minimal() -> grafica_modelo3

grid.arrange(grafica_modelo1,grafica_modelo2,grafica_modelo3,ncol=3)

