
# Paquetes ----------------------------------------------------------------

library(readr) # función parse_number
library(lubridate) # funciones month, year
library(zoo) # función yearmon
library(dplyr)
library(ggplot2)
library(seastests)

# Creación y lectura de datos ---------------------------------------------

set.seed(223)
data.frame(y = rnorm(150)) -> datos_simu
datos_simu$y %>% 
  zoo -> y

y %>% 
  autoplot()+ 
  geom_line(color="dodgerblue3",lwd=2)+
  labs(title = "Ruido blanco",
       caption = "")+
  theme_minimal()


read_csv('Fiestas.csv',skip=3,col_names=c("Fecha","Interes")) %>% 
  mutate(Interes = parse_number(Interes)) %>% 
  mutate(Mes     = month(Fecha)) %>% 
  mutate(Año     = year(Fecha))  %>% 
  mutate(AñoMes  = as.yearmon(Fecha)) -> datos_fiestas

datos_fiestas %>% 
  group_by(AñoMes) %>% 
  summarise(IM = mean(Interes)) -> InteresFP

InteresFP %>% 
  ggplot(aes(x=AñoMes,y=IM))+
  geom_bar(stat="identity")+
  labs(x = "Fecha",
       y = "Interés",
       title = "Interés medio mensual de búsqueda del término Fiestas patrias",
       caption = "Fuente: Google Trends")+
  theme_minimal()


# Seasonplot --------------------------------------------------------------

datos_simu$y %>% 
  ts(frequency = 12, start = c(2016,4)) %>% 
  seasonplot()

InteresFP$IM %>% 
  ts(frequency = 12, start = c(2016,4)) %>% 
  seasonplot()

# Monthplot ---------------------------------------------------------------

datos_simu$y %>% 
  ts(frequency = 12, start = c(2016,4)) %>% 
  monthplot()

InteresFP$IM %>% 
  ts(frequency = 12, start = c(2016,4)) %>% 
  monthplot()


# Boxplot -----------------------------------------------------------------

datos_simu$y %>% 
  ts(frequency = 12, start = c(2016,4)) -> y0
boxplot(y0 ~ cycle(y0), col = "gold")

InteresFP$IM %>% 
  ts(frequency = 12, start = c(2016,4)) -> IM0
boxplot(IM0 ~ cycle(IM0), col = "gold")


# Correlograma ------------------------------------------------------------

datos_simu$y %>% 
  ts() %>% 
  TSA::acf(type = "correlation", lag = 60, plot = FALSE) %>% 
  autoplot() +
  theme_minimal()

InteresFP$IM %>% 
  ts() %>% 
  TSA::acf(type = "correlation", lag = 50, plot = FALSE) %>% 
  autoplot() +
  theme_minimal()


# Kruskal Wallis ----------------------------------------------------------

datos_simu$y %>% 
  ts(frequency = 12, start = c(2016,4)) -> y0
kruskal.test(y0 ~ cycle(y0))

InteresFP$IM %>% 
  ts(frequency = 12, start = c(2016,4)) -> IM0
kruskal.test(IM0 ~ cycle(IM0))

# Webel y Ollech ----------------------------------------------------------

datos_simu$y %>% 
  ts(frequency = 12, start = c(2016,4))  %>% 
  wo %>% 
  summary

InteresFP$IM %>% 
  ts(frequency = 12, start = c(2016,4)) %>% 
  wo %>% 
  summary

