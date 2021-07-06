

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

read_excel('PBI.xlsx', skip=3,
           col_names=c('año','pbi'),
           col_types = c("numeric","numeric")) -> datos

datos$pbi %>% ts -> PBI
PBI %>% ts %>% autoplot

PBI_Train = window(PBI,start = 1, end = 60)
PBI_Test  = window(PBI,start = 61)

set.seed(159753)
PBI_Train %>% nnetar() -> modelo_train1
modelo_train1
modelo_train1 %>% forecast::forecast(h=10, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train1
accuracy(predicciones_train1, PBI_Test)
autoplot(PBI_Test) +
  autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo1
forecast::autoplot(PBI_Test) +
  capa_modelo1 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(300000,550000))+
  theme_minimal() -> grafica_modelo1


PBI_Train %>% BoxCox.lambda()
PBI_Train %>% nnetar(lambda = PBI_Train %>% BoxCox.lambda()) -> modelo_train2
modelo_train2
modelo_train2 %>% forecast::forecast(h=10, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train2
accuracy(predicciones_train2, PBI_Test)
autoplot(PBI_Test) +
  autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1.25, color="blue") +
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1.25, color="blue") -> capa_modelo2
forecast::autoplot(PBI_Test) +
  capa_modelo2 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(300000,550000))+
  theme_minimal() -> grafica_modelo2

PBI_Train %>% acf2

set.seed(159753)
PBI_Train %>% nnetar(p=2) -> modelo_train3
modelo_train3
modelo_train3 %>% forecast::forecast(h=10, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train3
accuracy(predicciones_train3, PBI_Test)
autoplot(PBI_Test) +
  autolayer(predicciones_train3, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train3, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo3
forecast::autoplot(PBI_Test) +
  capa_modelo3 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(300000,550000))+
  theme_minimal() -> grafica_modelo3

grid.arrange(grafica_modelo1,grafica_modelo2,grafica_modelo3,ncol=3)

