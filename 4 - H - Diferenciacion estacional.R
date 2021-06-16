
library(readxl)
library(dplyr)
library(ggfortify)
library(forecast)
library(astsa)

read_excel('ExportacionesFrutas.xlsx', skip = 98, col_names = c("Periodo","Exportaciones")) -> datos
datos$Exportaciones %>% ts(frequency = 12, start = c(1993,1)) -> serie1

serie1 %>% 
  autoplot(ts.geom = "ribbon", ts.fill="dodgerblue2", alpha = 0.5) + theme_void()

serie1 %>% 
  window(start = c(2010,7)) %>% 
  autoplot(ts.geom = "ribbon", ts.fill="dodgerblue2", alpha = 0.5) + theme_void()

serie1 %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie1 %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie1 %>% aTSA::stationary.test(method = "adf")
serie1 %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie1 %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie1 %>% BoxCox.lambda()

serie1 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie1 %>% diff %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie1 %>% diff %>% aTSA::stationary.test(method = "adf")
serie1 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie1 %>% diff %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie1 %>% diff %>% BoxCox.lambda()
serie1 %>% diff %>% acf2(240) # (1,1,1)x(1,0,0)
boxplot(diff(serie1) ~ cycle(diff(serie1)))
serie1 %>% diff %>% monthplot()
serie1 %>% diff %>% kruskal.test(cycle(diff(serie1)))

serie1 %>% diff %>% diff(12) %>% aTSA::stationary.test(method = "kpss",lag.short=T)
serie1 %>% diff %>% diff(12) %>% aTSA::stationary.test(method = "kpss",lag.short=F)
serie1 %>% diff %>% diff(12) %>% aTSA::stationary.test(method = "adf")
serie1 %>% diff %>% diff(12) %>% aTSA::stationary.test(method = "pp",lag.short=F)
serie1 %>% diff %>% diff(12) %>% aTSA::stationary.test(method = "pp",lag.short=T)
serie1 %>% diff %>% diff(12) %>% BoxCox.lambda()
boxplot(diff(diff(serie1),12) ~ cycle(diff(diff(serie1),12)))
serie1 %>% diff %>% diff(12) %>% monthplot()
serie1 %>% diff %>% diff(12) %>% kruskal.test(cycle(diff(diff(serie1),12)))
serie1 %>% diff %>% diff(12) %>% acf2(240) # (1,1,1)x(0,1,1)

auto.arima(serie1)
