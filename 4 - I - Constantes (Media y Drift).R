# Paquetes ----------------------------------------------------------------

library(forecast)
library(ggplot2)
library(caschrono)
library(TSA)
library(astsa)
library(sweep)
library(gridExtra)
library(tidyquant)

# Modelo AR ---------------------------------------------------------------

set.seed(444)
arima.sim(model = list(order=c(2,0,0), ar = c(0.8,-0.6)), 
          n     = 500) + 5 -> serie0
serie0 %>% autoplot() + theme_minimal()
serie0 %>% acf2
serie0 %>% auto.arima
Arima(serie0, order = c(2,0,0))
Arima(serie0, order = c(2,0,0)) %>% t_stat
Arima(serie0, order = c(2,0,0),include.mean=TRUE)
Arima(serie0, order = c(2,0,0),include.mean=FALSE)
Arima(serie0, order = c(2,0,0),include.drift=TRUE)
Arima(serie0, order = c(2,0,0),include.drift=TRUE) %>% t_stat
Arima(serie0, order = c(2,0,0),include.drift=FALSE)
Arima(serie0, order = c(2,0,0),include.constant=TRUE)
Arima(serie0, order = c(2,0,0),include.constant=FALSE)

# Modelo MA ---------------------------------------------------------------

set.seed(444)
arima.sim(model = list(order=c(0,0,1), ma = c(0.8)), 
          n     = 500) + 5 -> serie1
serie1 %>% autoplot() + theme_minimal()
serie1 %>% acf2
serie1 %>% auto.arima
Arima(serie1, order = c(0,0,1))
Arima(serie1, order = c(0,0,1)) %>% t_stat
Arima(serie1, order = c(0,0,1),include.mean=TRUE)
Arima(serie1, order = c(0,0,1),include.mean=FALSE)
Arima(serie1, order = c(0,0,1),include.drift=TRUE)
Arima(serie1, order = c(0,0,1),include.drift=TRUE) %>% t_stat
Arima(serie1, order = c(0,0,1),include.drift=FALSE)
Arima(serie1, order = c(0,0,1),include.constant=TRUE)
Arima(serie1, order = c(0,0,1),include.constant=FALSE)


# Modelo ARI --------------------------------------------------------------

set.seed(444)
arima.sim(model = list(order=c(1,1,0), ar = c(0.8)), 
          n     = 500) + 5 -> serie2
serie2 %>% autoplot() + theme_minimal()
serie2 %>% diff %>% autoplot() + theme_minimal()
serie2 %>% diff %>% acf2
serie2 %>% auto.arima
Arima(serie2, order = c(1,1,0))
Arima(serie2, order = c(1,1,0)) %>% t_stat
Arima(serie2, order = c(1,1,0),include.mean=TRUE)
Arima(serie2, order = c(1,1,0),include.mean=FALSE)
Arima(serie2, order = c(1,1,0),include.drift=TRUE)
Arima(serie2, order = c(1,1,0),include.drift=TRUE) %>% t_stat
Arima(serie2, order = c(1,1,0),include.drift=FALSE)
Arima(serie2, order = c(1,1,0),include.constant=TRUE)
Arima(serie2, order = c(1,1,0),include.constant=FALSE)


# Modelo IMA --------------------------------------------------------------

set.seed(444)
arima.sim(model = list(order=c(0,1,1), ma = c(0.8)), 
          n     = 500) + 5 -> serie3
serie3 %>% autoplot() + theme_minimal()
serie3 %>% diff %>% autoplot() + theme_minimal()
serie3 %>% diff %>% acf2
serie3 %>% auto.arima
Arima(serie3, order = c(0,1,1))
Arima(serie3, order = c(0,1,1)) %>% t_stat
Arima(serie3, order = c(0,1,1),include.mean=TRUE)
Arima(serie3, order = c(0,1,1),include.mean=FALSE)
Arima(serie3, order = c(0,1,1),include.drift=TRUE)%>% t_stat
Arima(serie3, order = c(0,1,1),include.drift=FALSE)
Arima(serie3, order = c(0,1,1),include.constant=TRUE)
Arima(serie3, order = c(0,1,1),include.constant=FALSE)

# Modelo ARMA -------------------------------------------------------------

set.seed(444)
arima.sim(model = list(order=c(1,0,1), ar = c(0.9), ma = c(-0.7)), 
          n     = 500) + 5 -> serie4
serie4 %>% autoplot() + theme_minimal()
serie4 %>% acf2
serie4 %>% auto.arima
Arima(serie4, order = c(1,0,1))
Arima(serie4, order = c(1,0,1)) %>% t_stat
Arima(serie4, order = c(1,0,1),include.mean=TRUE)
Arima(serie4, order = c(1,0,1),include.mean=FALSE)
Arima(serie4, order = c(1,0,1),include.drift=TRUE)
Arima(serie4, order = c(1,0,1),include.drift=FALSE)
Arima(serie4, order = c(1,0,1),include.constant=TRUE)
Arima(serie4, order = c(1,0,1),include.constant=FALSE)


# Modelo ARIMA ------------------------------------------------------------

set.seed(444)
arima.sim(model = list(order=c(1,1,1), ar = c(0.9), ma = c(-0.7)), 
          n     = 500) + 5 -> serie5
serie5 %>% autoplot() + theme_minimal()
serie5 %>% diff %>% autoplot() + theme_minimal()
serie5 %>% diff %>% acf2
serie5 %>% auto.arima
Arima(serie5, order = c(1,1,1))
Arima(serie5, order = c(1,1,1)) %>% t_stat
Arima(serie5, order = c(1,1,1),include.mean=TRUE)
Arima(serie5, order = c(1,1,1),include.mean=FALSE)
Arima(serie5, order = c(1,1,1),include.drift=TRUE)
Arima(serie5, order = c(1,1,1),include.drift=FALSE)
Arima(serie5, order = c(1,1,1),include.constant=TRUE)
Arima(serie5, order = c(1,1,1),include.constant=FALSE)

