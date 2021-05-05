
# Paquetes ----------------------------------------------------------------

library(tseries)
library(aTSA)
library(forecast) # función BoxCox.lambda, BoxCox
library(TSA)
library(MTS)

# Generación de series simuladas ------------------------------------------

set.seed(2875)

y0 = rnorm(50)

y1 = NULL
for(i in 1:200){y1[i] = rnorm(1,0,i)}

y2 = NULL
for(i in 1:200){y2[i] = rnorm(1,i,2)}

y3 = NULL
for(i in 1:200){y3[i] = rnorm(1,i,i/5)}

y4 = NULL
for(i in 1:200){y4[i] = sin(i) + rnorm(1,0,3)}

y5 = NULL
for(i in 1:200){y5[i] = sin(i) + rnorm(1,i,10)}


# Gráfica de las series ---------------------------------------------------

x11();par(mfrow=c(3,2))
plot(y0,type="l")
plot(y1,type="l")
plot(y2,type="l")
plot(y3,type="l")
plot(y4,type="l")
plot(y5,type="l")

# Prueba KPSS -------------------------------------------------------------

# H0: La serie SÍ es estacionaria
# H1: La serie NO es estacionaria

n = 1:300
lag.tseries  = NULL
lag.atsa.min = NULL
lag.atsa.max = NULL
for(i in 1:300){
  lag.tseries[i]  = trunc(4*(n[i]/100)^(1/4))
  lag.atsa.min[i] = max(1,floor(3*sqrt(n[i])/13))
  lag.atsa.max[i] = max(1,floor(10*sqrt(n[i])/13))
}

data.frame(n,lag.tseries,lag.atsa.min,lag.atsa.max)

tseries::kpss.test(y0,null="Level")
tseries::kpss.test(y0,null="Trend")
aTSA::stationary.test(y0,method = "kpss",lag.short=T)
aTSA::stationary.test(y0,method = "kpss",lag.short=F)

tseries::kpss.test(y1,null="Level")
tseries::kpss.test(y1,null="Trend")
aTSA::stationary.test(y1,method = "kpss",lag.short=T)
aTSA::stationary.test(y1,method = "kpss",lag.short=F)

tseries::kpss.test(y2,null="Level") # caso 1
tseries::kpss.test(y2,null="Trend") # caso 3
aTSA::stationary.test(y2,method = "kpss",lag.short=T)
aTSA::stationary.test(y2,method = "kpss",lag.short=F)

tseries::kpss.test(y3,null="Level")
tseries::kpss.test(y3,null="Trend")
aTSA::stationary.test(y3,method = "kpss",lag.short=T)
aTSA::stationary.test(y3,method = "kpss",lag.short=F)

tseries::kpss.test(y4,null="Level")
tseries::kpss.test(y4,null="Trend")
aTSA::stationary.test(y4,method = "kpss",lag.short=T)
aTSA::stationary.test(y4,method = "kpss",lag.short=F)

tseries::kpss.test(y5,null="Level")
tseries::kpss.test(y5,null="Trend")
aTSA::stationary.test(y5,method = "kpss",lag.short=T)
aTSA::stationary.test(y5,method = "kpss",lag.short=F)

# Prueba ADF --------------------------------------------------------------

# H0: La serie NO es estacionaria
# H1: La serie SÍ es estacionaria

tseries::adf.test(y0)
aTSA::stationary.test(y0,method = "adf")

tseries::adf.test(y1)
stationary.test(y1,method = "adf")

tseries::adf.test(y2, alternative = "explosive") #Type1
tseries::adf.test(y2, alternative = "stationary") #Type3
tseries::adf.test(y2) #Type3
stationary.test(y2,method = "adf")

tseries::adf.test(y3)
stationary.test(y3,method = "adf")

tseries::adf.test(y4)
stationary.test(y4,method = "adf")

tseries::adf.test(y5)
stationary.test(y5,method = "adf")

# Prueba Box Cox ----------------------------------------------------------

(BoxCox.lambda(y0) -> ly0)
(BoxCox.lambda(y1) -> ly1)
(BoxCox.lambda(y2) -> ly2)
(BoxCox.lambda(y3) -> ly3)
(BoxCox.lambda(y4) -> ly4)
(BoxCox.lambda(y5) -> ly5)

x11();par(mfrow=c(1,2))
plot(y1,type="l")
plot(BoxCox(y1,ly1),type="l")

x11();par(mfrow=c(1,2))
plot(y3,type="l")
plot(BoxCox(y3,ly3),type="l")

# Prueba McLeod Li --------------------------------------------------------

# Ho: autocorrel_lag = 0
# H1: autocorrel_lag != 0

x11();par(mfrow=c(3,2))
McLeod.Li.test(y=y0)
McLeod.Li.test(y=y1)
McLeod.Li.test(y=y2)
McLeod.Li.test(y=y3)
McLeod.Li.test(y=y4)
McLeod.Li.test(y=y5)

# Prueba ARCH -------------------------------------------------------------

archTest(y0, lag=10)
archTest(y1, lag=10)
archTest(y2, lag=10)
archTest(y3, lag=10)
archTest(y4, lag=10)
archTest(y5, lag=10)
