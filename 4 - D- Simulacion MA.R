
library(TSA)
library(ggfortify)

# MA(1) con theta = 0.72 --------------------------------------------------

# ARIMA(0,0,1) con theta = 0.72

# Y_t = e_t - 0.72e_{t-1}
# Y_t = e_t + (-0.72)e_{t-1}

theta  = -0.72
sigma  = 3.4

set.seed(315)
arima.sim(model=list(ar=c(theta)), n=150, sd = sigma) -> Y.MA1A 
arima.sim(list(order = c(0,0,1), ma = theta), n = 150) -> Y.MA1A # AR(1) con phi = 0.85

Y.MA1A %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.MA1A %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y.MA1A %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y.MA1A %>% aTSA::stationary.test(method = "adf")
Y.MA1A %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y.MA1A %>% aTSA::stationary.test(method = "pp",lag.short=T)
Y.MA1A %>% BoxCox.lambda()
Y.MA1A %>% archTest()
Y.MA1A %>% McLeod.Li.test(y=.)

Y.MA1A %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.MA1A %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")

# ARIMA(0,1,1) con theta = 0.72 -------------------------------------------

theta  = -0.72
sigma  = 3.4

set.seed(165)
arima.sim(list(order = c(0,1,1), ma = theta), n = 150) -> Y.IMA1A # AR(1) con phi = 0.85

Y.IMA1A %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.IMA1A %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y.IMA1A %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y.IMA1A %>% aTSA::stationary.test(method = "adf")
Y.IMA1A %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y.IMA1A %>% aTSA::stationary.test(method = "pp",lag.short=T)
Y.IMA1A %>% BoxCox.lambda()
Y.IMA1A %>% archTest()
Y.IMA1A %>% McLeod.Li.test(y=.)

Y.IMA1A %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.IMA1A %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")

Y.IMA1A %>% diff %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.IMA1A %>% diff %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")



library(writexl)
writexl::write_xlsx(x = data.frame(Y = Y.IMA1A), path = "Serie_simulada.xlsx")



