

# Paquetes ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(zoo)
library(readr)
library(readxl)
library(forecast)
library(TSA)
library(funtimes)
library(Kendall)
library(trend)
library(randtests)

# Creación y lectura de datos ---------------------------------------------

set.seed(223)
data.frame(y = rnorm(150)) -> datos_simu
datos_simu$y %>% 
  zoo -> y

y %>% 
  autoplot()+ 
  geom_line(color="dodgerblue3",lwd=2)+
  labs(title = "Interés de búsqueda del término vacuna",
       caption = "Fuente: Google Trends")+
  theme_minimal()


read_csv('vacunas.csv',skip=3,col_names=c("Fecha","Interes")) -> datos_vacunas
datos_vacunas$Interes %>% 
  zoo(order.by = datos_vacunas$Fecha) -> Interes

Interes %>% 
  autoplot()+ 
  geom_line(color="dodgerblue3",lwd=2)+
  labs(title = "Interés de búsqueda del término vacuna",
       caption = "Fuente: Google Trends")+
  theme_minimal()

read_xlsx('Recuperados.xlsx') -> datos_recuperados
datos_recuperados$Recuperados %>% 
  zoo(order.by = datos_recuperados$Fecha) -> Recuperados

Recuperados %>% 
  autoplot()+ 
  geom_line(color="dodgerblue3",lwd=1)+
  labs(title = "Número de personas recuperadas del COVID-19 en Perú",
       caption = "Fuente: MINSA")+
  theme_minimal()


# Regresión lineal --------------------------------------------------------

datos_simu %>% 
  mutate(t = 1:nrow(datos_simu)) %>% 
  ggplot(aes(x = t, y = y)) +
  geom_line() +
  geom_smooth(method = "lm")+
  theme_minimal()

datos_vacunas %>% 
  mutate(t = 1:nrow(datos_vacunas)) %>% 
  ggplot(aes(x = t, y = Interes)) +
  geom_line() +
  geom_smooth(method = "lm")+
  theme_minimal()

datos_recuperados %>% 
  mutate(t = 1:nrow(datos_recuperados)) %>% 
  ggplot(aes(x = t, y = Recuperados)) +
  geom_line() +
  geom_smooth(method = "lm")+
  theme_minimal()



# Media móvil -------------------------------------------------------------

(ma(datos_simu$y, order = 10, centre = F) %>% 
  zoo() -> movavg0)
autoplot(cbind(y,movavg0) , facets = FALSE) +
  labs(title = "Serie de tiempo",
       y     = "y")+
  theme(legend.position="bottom") +
  scale_colour_manual(labels = c("Real", "Media móvil orden 6"), 
                      values = c("steelblue2", "red")) +
  theme_minimal()

(ma(datos_vacunas$Interes, order = 3, centre = F) %>% 
    zoo(order.by = datos_vacunas$Fecha) -> movavg1)
autoplot(cbind(Interes,movavg1) , facets = FALSE) +
  labs(title = "Búsquedas del término vacuna",
       y     = "Interés")+
  theme(legend.position="bottom") +
  scale_colour_manual(labels = c("Real", "Media móvil orden 3"), 
                      values = c("steelblue2", "red")) +
  theme_minimal()

(ma(datos_recuperados$Recuperados, order = 30, centre = F) %>% 
  zoo(order.by = datos_recuperados$Fecha) -> movavg2)
autoplot(cbind(Recuperados,movavg2) , facets = FALSE) +
  labs(title = "Personas recuperadas del COVID-19 en Perú",
       y     = "Cantidad")+
  theme(legend.position="bottom") +
  scale_colour_manual(labels = c("Real", "Media móvil orden 10"), 
                      values = c("steelblue2", "red")) +
  theme_minimal()


# Correlograma ------------------------------------------------------------

y %>% 
  ts() %>% 
  TSA::acf(type = "correlation", lag = 20, plot = FALSE) %>% 
  autoplot() +
  theme_minimal()

Interes %>% 
  ts() %>% 
  TSA::acf(type = "correlation", lag = 26, plot = FALSE) %>% 
  autoplot() +
  theme_minimal()

Recuperados %>% 
  ts() %>% 
  TSA::acf(type = "correlation", lag = 90, plot = FALSE) %>% 
  autoplot() +
  theme_minimal()

# Correlación de Spearman -------------------------------------------------

y %>% 
  ts %>% 
  cor.test(1:length(y), method = "spearman")

Interes %>% 
  ts %>% 
  cor.test(1:length(Interes), method = "spearman")

Recuperados %>% 
  ts %>% 
  cor.test(1:length(Recuperados), method = "spearman")


# Prueba t ----------------------------------------------------------------

notrend_test(y)$p.value
notrend_test(Interes)$p.value
notrend_test(Recuperados)$p.value


# Prueba Mann Kendall -----------------------------------------------------

MannKendall(y)$sl
notrend_test(y,test = "MK", B = 1e4)$p.value

MannKendall(Interes)$sl
notrend_test(Interes,test = "MK", B = 1e4)$p.value

MannKendall(Recuperados)$sl
notrend_test(Recuperados,test = "MK",B=1e4)$p.value


# Prueba de Cox Stuart ----------------------------------------------------

y %>% ts %>% cs.test
y %>% ts %>% cox.stuart.test

Interes %>% ts %>% cs.test
Interes %>% ts %>% cox.stuart.test
Interes %>% ts %>% cox.stuart.test(alternative = "left.sided")
Interes %>% ts %>% cox.stuart.test(alternative = "right.sided")

Recuperados %>% ts %>% cs.test
Recuperados %>% ts %>% cox.stuart.test
Recuperados %>% ts %>% cox.stuart.test(alternative = "left.sided")
Recuperados %>% ts %>% cox.stuart.test(alternative = "right.sided")


# Prueba de WAK -----------------------------------------------------------

notrend_test(y, test = "WAVK")$p.value
notrend_test(Interes, test = "WAVK")$p.value
notrend_test(Recuperados, test = "WAVK")$p.value
