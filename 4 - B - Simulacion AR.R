
library(TSA)
library(ggfortify)

# AR(1) con phi = 0.85 ----------------------------------------------------

# ARIMA(1,0,0) con phi = 0.85

phi    = 0.85
sigma  = 2

set.seed(78)
arima.sim(model=list(ar=c(phi)), n=150, sd = sigma) -> Y.AR1A  # AR(1) con phi = 0.85
#arima.sim(list(order = c(1,0,0), ar= phi), n = 150) -> Y.AR1A # AR(1) con phi = 0.85
Y.AR1A %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.AR1A %>% aTSA::stationary.test(method = "kpss",lag.short=T)
Y.AR1A %>% aTSA::stationary.test(method = "kpss",lag.short=F)
Y.AR1A %>% aTSA::stationary.test(method = "adf")
Y.AR1A %>% aTSA::stationary.test(method = "pp",lag.short=F)
Y.AR1A %>% aTSA::stationary.test(method = "pp",lag.short=T)
Y.AR1A %>% BoxCox.lambda()
Y.AR1A %>% archTest()
Y.AR1A %>% McLeod.Li.test(y=.)

Y.AR1A %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.AR1A %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")

Y.AR1A %>% 
  TSA::acf(type="partial", lag = 50, plot = FALSE, main = "Función de autocorrelación parcial")

lm(Y.AR1A~zlag(Y.AR1A))


# AR(1) con phi = 0.55 ----------------------------------------------------

phi    = 0.55
sigma  = sqrt(6)

set.seed(6545)
arima.sim(model=list(ar=c(phi)), n=150, sd = sigma) -> Y.AR1B  # AR(1) con phi = 0.48
Y.AR1B %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.AR1B %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.AR1B %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")


# AR(1) con phi = -0.1 ----------------------------------------------------

phi    = -0.1
sigma  = sqrt(2)

set.seed(7488)
arima.sim(model=list(ar=c(phi)), n=150, sd = sigma) -> Y.AR1C  # AR(1) con phi = 0.48
Y.AR1C %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.AR1C %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.AR1C %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")


# AR(1) con phi = -0.89 ---------------------------------------------------

phi    = -0.89
sigma  = sqrt(8)

set.seed(7488)
arima.sim(model=list(ar=c(phi)), n=150, sd = sigma) -> Y.AR1C  # AR(1) con phi = 0.48
Y.AR1C %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.AR1C %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.AR1C %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")


# AR(2) con phi1 = 0.75 y phi2 = 0.8 --------------------------------------

phi    = c(0.65,0.25)
sigma  = sqrt(2)

set.seed(15345)
arima.sim(model=list(ar=phi), n=150, sd = sigma) -> Y.AR2A  
Y.AR2A %>% 
  autoplot(type="l") + 
  geom_hline(yintercept = 0)

Y.AR2A %>% 
  TSA::acf(type="correlation", lag = 50, plot = TRUE, main = "Función de autocorrelación")

Y.AR2A %>% 
  TSA::acf(type="partial", lag = 50, plot = TRUE, main = "Función de autocorrelación parcial")



ts.plot(ts.sim)
ts.sim %>% Arima(order=c(1,1,0)) %>% forecast(h=3)
