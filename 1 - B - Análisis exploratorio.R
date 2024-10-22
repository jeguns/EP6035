

# Carga de paquetes -------------------------------------------------------

library(readxl) # lectura datos en excel
library(zoo) # series de tiempo con �ndices 
library(dplyr)
library(ggplot2) # gr�ficas
library(lubridate) # fechas 
library(scales)
library(tidyr) # para funci�n replace_na
library(xts) # para funci�n xts y las agregaciones apply
library(ggfortify) # para que funcione autoplot
library(TSA) # para acf

# Lectura de datos --------------------------------------------------------

datos = read_xlsx("Cifras.xlsx")

# Preprocesamiento de datos -----------------------------------------------

datos %>% 
  mutate(Fecha = ymd(Fecha)) %>% 
  replace_na(list(Recuperados=0,Hospitalizados=0)) %>% 
  mutate(Dia = day(Fecha), # yday # wday
         Sem = week(Fecha), # epiweek # isoweek
         Mes = month(Fecha),
         Tri = quarter(Fecha),
         A�o = year(Fecha)) -> datos

#https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf

attach(datos)

Recuperados %>% ts() 
Recuperados %>% ts(start = c(2020, yday(min(datos$Fecha))),
                   frequency = 365) -> STRecuperados

Recuperados %>% str()
STRecuperados %>% str()

ts(Hospitalizados) -> STHospitalizados

xts(Recuperados,Fecha) -> Recuperados2
Recuperados %>% xts(Fecha) -> Recuperados2
Recuperados2 %>% str()

Hospitalizados %>% xts(Fecha) -> Hospitalizados2

# Gr�ficas de ST - Funci�n plot -------------------------------------------

Recuperados %>% plot() # graficando el vector tradicional
STRecuperados %>% plot() # graficando el vector de ST
STRecuperados %>% plot(type = "b",
                       pch  = 18,
                       ylab = "N� de pacientes", 
                       xlab = "Tiempo",
                       col  = "blue",
                       lwd  = 2,
                       lty  = 3,
                       main = "Pacientes recuperados de COVID-19 (nacional)") 

# Gr�ficas de ST - Funci�n autoplot ---------------------------------------

STRecuperados %>% ggplot2::autoplot()

STRecuperados %>% 
  ggplot2::autoplot(ts.geom = "bar", fill = "darkgreen")

STRecuperados %>% 
  ggplot2::autoplot(ts.geom = "point", colour = "darkgreen", shape = 8)

# Gr�ficas de ST - Funci�n qplot ------------------------------------------

qplot(x    = Fecha, 
      y    = Recuperados, 
      data = datos)

qplot(x    = Fecha, 
      y    = Recuperados,
      data = datos,
      xlab = "Fecha",
      ylab = "N�mero de pacientes",
      main = "Pacientes recuperados de COVID-19",
      geom = "path")

# Gr�ficas de ST - Funci�n ggplot2 ----------------------------------------

datos %>% 
  ggplot(aes(x=Fecha,y=Recuperados))+
  geom_line(color = "darkblue", size=0.5) + 
  scale_x_date(limits = c(min(datos$Fecha), max(datos$Fecha)),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "15 days"),
               expand = c(0,0),
               labels = date_format("%d-%b-%y"))+
  scale_y_continuous(breaks = seq(0,10000,1500))+
  labs(x = "Fecha",
       y = "Pacientes",
       title = "Evoluci�n de pacientes recuperados de COVID-19",
       subtitle = "Datos a nivel nacional",
       caption = "Fuente:MINSA")+
  theme_minimal()+
  theme(axis.text   = element_text(size=8),
        axis.text.x = element_text(angle=45)) -> grafico1

ggsave('grafico1.png',grafico1,width=30,height=20,units="cm")

datos %>% 
  ggplot(aes(x=Fecha,y=Recuperados))+
  geom_area(fill = "firebrick", alpha=0.95) + 
  scale_x_date(limits = c(ymd("2020-03-16"), max(datos$Fecha)),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "14 days"),
               expand = c(0,0),
               labels = date_format("%d/%m"))+
  labs(x = "Fecha",
       y = "Pacientes",
       title = "Evoluci�n de pacientes recuperados de COVID-19",
       subtitle = "Datos a nivel nacional",
       caption = "Fuente:MINSA")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45)) -> grafico2

ggsave('grafico2.png',grafico2, width = 25, height = 15, units ="cm")

# Autocovariancias y autocorrelaciones ------------------------------------

Recuperados %>% 
  stats::acf(type = "covariance",
             main = "Funci�n de autocovarianza")

Recuperados %>% 
  stats::acf(type = "covariance",
             plot = FALSE)

Recuperados %>% 
  stats::acf(main="Funci�n de autocorrelaci�n")

Recuperados %>% 
  stats::acf(main ="Funci�n de autocorrelaci�n",
             plot = F,
             lag  = 100)

Recuperados %>% 
  TSA::acf(main="Funci�n de autocorrelaci�n")


# Gr�fica de 2 ST - Funci�n ts.plot ---------------------------------------

ts.plot(datos[,2:3]) 

ts.plot(datos[,2:3],
        lty  = c(1,2),
        col  = c(2,4),
        xlab = "Tiempo",
        ylab = "N�mero de pacientes",
        main = "Pacientes recuperados de COVID-19")


# Gr�fica de 2 ST - Funci�n autoplot --------------------------------------

ST = cbind(STRecuperados,STHospitalizados)
autoplot(ST)
autoplot(ST, facets = FALSE)


# Gr�fica de 2 ST - Funci�n ggplot2 ---------------------------------------

datos %>% 
  pivot_longer(cols = c("Recuperados","Hospitalizados"),
               names_to = "Status",
               values_to = "Cantidad") %>% 
  ggplot(aes(x=Fecha,y=Cantidad,color=Status)) + 
  geom_line(size=1.25) + 
  scale_x_date(limits = c(min(datos$Fecha), max(datos$Fecha)),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month"),
               expand = c(0,0),
               labels = date_format("%d-%m-%Y"))+
  scale_color_manual(values = c("darkorange2","forestgreen"))+
  labs(x = "Fecha",
       y = "Pacientes",
       title = "Evoluci�n de pacientes de COVID-19",
       subtitle = "Datos a nivel nacional",
       caption = "Fuente:MINSA")+
  theme_minimal() -> grafico3

ggsave('grafico3.png',grafico3, width = 30, height = 15, units ="cm")

datos %>% 
  pivot_longer(cols = c("Recuperados","Hospitalizados"),
               names_to = "Status",
               values_to = "Cantidad") %>% 
  ggplot(aes(x=Fecha,y=Cantidad,fill=Status)) + 
  geom_area(position = "identity", alpha = 0.5) + 
  scale_x_date(limits = c(min(datos$Fecha), max(datos$Fecha)),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month"),
               expand = c(0,0),
               labels = date_format("%d-%m-%Y"))+
  scale_fill_manual(values = c("darkorange2","forestgreen"))+
  labs(x = "Fecha",
       y = "Pacientes",
       title = "Evoluci�n de pacientes de COVID-19",
       subtitle = "Datos a nivel nacional",
       caption = "Fuente:MINSA")+
  theme_minimal() -> grafico4

ggsave('grafico4.png',grafico4, width = 30, height = 15, units ="cm")

datos %>% 
  pivot_longer(cols = c("Recuperados","Hospitalizados"),
               names_to = "Status",
               values_to = "Cantidad") %>% 
  ggplot(aes(x=Fecha,y=Cantidad,fill=Status)) + 
  geom_area(position = "identity", alpha = 0.5) + 
  scale_x_date(limits = c(min(datos$Fecha), max(datos$Fecha)),
               breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month"),
               expand = c(0,0),
               labels = date_format("%d-%m-%Y"))+
  facet_grid(Status~.)+
  scale_fill_manual(values = c("darkorange2","forestgreen"))+
  labs(x = "Fecha",
       y = "Pacientes",
       title = "Evoluci�n de pacientes de COVID-19",
       subtitle = "Datos a nivel nacional",
       caption = "Fuente:MINSA")+
  theme_minimal() -> grafico5

ggsave('grafico5.png',grafico5, width = 30, height = 30, units ="cm")


# Agregaci�n con xts ------------------------------------------------------

Recuperados2 %>% apply.weekly(min)
Recuperados2 %>% apply.monthly(sum)
Recuperados2 %>% apply.quarterly(sum)
Recuperados2 %>% apply.yearly(sum)

Recuperados2 %>% apply.quarterly(min)
Recuperados2 %>% apply.quarterly(max)

Recuperados2 %>% apply.weekly(var)

Recuperados2 %>% apply.monthly(quantile,probs=c(0.25,0.6))

Recuperados2 %>% apply.quarterly(function(x) (min(x)+max(x))/2)

Recuperados2 %>% 
  apply.weekly(mean) %>% 
  barplot(col="forestgreen")

Recuperados2 %>% 
  apply.weekly(mean) %>% 
  autoplot()

# Agregaci�n con dplyr ----------------------------------------------------

datos %>% 
  group_by(A�o) %>%
  summarise(SR = sum(Recuperados),
            MH = mean(Hospitalizados)) %>% 
  ggplot(aes(x=as.factor(A�o),y=MH,label=round(MH,0)))+
  geom_bar(stat="identity",fill="darkorange")+
  geom_text(position = position_stack(vjust=0.5))+
  labs(x="A�o",
       y="Cantidad",
       title="N�mero promedio diario de pacientes hospitalizados",
       subtitle="A nivel nacional")

datos %>% 
  mutate(A�oMes=as.yearmon(Fecha)) %>% 
  group_by(A�oMes) %>%
  summarise(SR = sum(Recuperados),
            MH = mean(Hospitalizados)) %>% 
  ggplot(aes(x=as.factor(A�oMes),y=SR,label=round(SR,0)))+
  geom_bar(stat="identity",fill="forestgreen")+
  geom_text(position = position_stack(vjust=0.5),colour="white")+
  labs(x="Fecha",
       y="Cantidad",
       title="N�mero total de pacientes recuperados de COVID-19",
       subtitle="A nivel nacional")+
  theme_minimal()

datos %>% 
  group_by(Dia,Mes,A�o) %>%
  summarise(SR = sum(Recuperados),
            MH = mean(Hospitalizados)) %>% 
  ggplot(aes(x=as.factor(Dia),y=SR,label=round(SR,0)))+
  geom_bar(stat="identity",fill="gold")+
  geom_text(position = position_stack(vjust=0.5),colour="darkblue")+
  facet_grid(A�o+Mes~.)+
  labs(x="Fecha",
       y="Cantidad",
       title="N�mero total de pacientes recuperados de COVID-19",
       subtitle="A nivel nacional")+
  theme_minimal()


# Otros paquetes: Investigar / explorar: bsts, highfrequency