library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

Vacunas_Peru = read_csv('vacunas_covid_2021-04-12.csv')

Vacunas_Peru %>% 
  mutate(FECHA_VACUNACION=ymd(FECHA_VACUNACION)) %>%
  mutate(AÑO = year(FECHA_VACUNACION)) %>% 
  mutate(TRI = quarter(FECHA_VACUNACION)) %>% 
  mutate(MES = month(FECHA_VACUNACION)) %>% 
  mutate(SEM = week(FECHA_VACUNACION)) %>% 
  mutate(SEE = epiweek(FECHA_VACUNACION)) %>% 
  mutate(DIA = day(FECHA_VACUNACION)) %>% 
  mutate(dia = yday(FECHA_VACUNACION)) %>% 
  mutate(DOSIS = as.factor(DOSIS)) %>% 
  mutate(DOSIS = recode_factor(DOSIS,'1'='Primera','2'='Segunda')) -> Vacunas_Peru

Vacunas_Peru %>% 
  count(SEE) 

Vacunas_Peru %>% 
  count(SEE) %>% 
  ggplot(aes(x=as.factor(SEE),y=n))+
  geom_bar(stat="identity")

Vacunas_Peru %>% 
  filter(GRUPO_RIESGO=="ADULTO MAYOR" & DEPARTAMENTO=="LIMA") %>% 
  count(SEE) %>% 
  ggplot(aes(x=as.factor(SEE),y=n))+
  geom_bar(stat="identity")

Vacunas_Peru %>% 
  filter(GRUPO_RIESGO=="ADULTO MAYOR") %>% 
  count(dia) %>% 
  select(n) %>% 
  ts() %>% 
  TSA::acf(main = "Función de autocorrelación")
