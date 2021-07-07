read_excel('PBI PESCA.xlsx', skip=2,
           col_names=c('mesaño','pbi'),
           col_types = c("text","numeric")) -> datos
datos$pbi %>% ts(frequency = 12,start=c(2003,1)) -> pbi

pbi %>% autoplot

pbi %>% acf2(100) 

stl(pbi, s.window = 12) -> descomp
descomp$time.series %>% 
  data.frame %>% 
  select(-seasonal) %>% 
  mutate(serie_se = trend+remainder) %>% 
  select(serie_se) %>% 
  ts(frequency=12) %>%
  acf2(100) 

maiz %>% nnetar() -> modelo1
modelo1
modelo1 %>% sw_tidy()
modelo1 %>% sw_glance()
modelo1 %>% sw_augment()
modelo1 %>% sw_augment() %>% View

pbi_Train = window(pbi,start = 2003, end = 2019)
pbi_Test  = window(pbi,start = 2019+1/12)
length(pbi_Test)

set.seed(65465)
maiz_Train %>% nnetar() -> modelo_train1
modelo_train1
modelo_train1 %>% forecast::forecast(h=27, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train1
accuracy(predicciones_train1, pbi_Test)
autoplot(pbi_Test) +
  autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("")
autolayer(predicciones_train1, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo1
forecast::autoplot(pbi_Test) +
  capa_modelo1 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-50,400))+
  theme_minimal() -> grafica_modelo1

pbi_Train %>% BoxCox.lambda()
set.seed(65465)
pbi_Train %>% nnetar(lambda = pbi_Train %>% BoxCox.lambda()) -> modelo_train2
modelo_train2
modelo_train2 %>% forecast::forecast(h=27, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train2
accuracy(predicciones_train2, pbi_Test)
autoplot(pbi_Test) +
  autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("")
autolayer(predicciones_train2, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo2
forecast::autoplot(pbi_Test) +
  capa_modelo2 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-50,400))+
  theme_minimal() -> grafica_modelo2



pbi_Train %>% acf2(100) # P = 3

stl(pbi_Train, s.window = 12) -> descomp
descomp$time.series %>% 
  data.frame %>% 
  select(-seasonal) %>% 
  mutate(serie_se = trend+remainder) %>% 
  select(serie_se) %>% 
  ts %>%
  acf2(100) # p = 1

set.seed(159753)
pbi_Train %>% nnetar(p=1,P=3) -> modelo_train3
modelo_train3
modelo_train3 %>% forecast::forecast(h=27, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train3
accuracy(predicciones_train3, pbi_Test)
autoplot(pbi_Test) +
  autolayer(predicciones_train3, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train3, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo3
forecast::autoplot(pbi_Test) +
  capa_modelo3 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-50,400))+
  theme_minimal() -> grafica_modelo3

set.seed(159753)
pbi_Train %>% nnetar(p=1,P=2) -> modelo_train4
modelo_train4
modelo_train4 %>% forecast::forecast(h=27, PI=TRUE, level = c(0.90,0.95)) -> predicciones_train4
accuracy(predicciones_train4, pbi_Test)
autoplot(pbi_Test) +
  autolayer(predicciones_train4, series="Prediccion", linetype = "dashed", 
            alpha = 0.5, size= 1, color="blue") + 
  theme_minimal() +
  ylab("Índice")
autolayer(predicciones_train4, series="Prediccion", linetype = "dashed", 
          alpha = 0.5, size= 1, color="blue") -> capa_modelo4
forecast::autoplot(pbi_Test) +
  capa_modelo4 +
  geom_point(size = 1) +
  scale_y_continuous(limits=c(-50,400))+
  theme_minimal() -> grafica_modelo4

grid.arrange(grafica_modelo1,grafica_modelo2,grafica_modelo3,grafica_modelo4,ncol=4)

