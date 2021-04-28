
# Paquetes ----------------------------------------------------------------

library(tseries)
library(aTSA)

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
for(i in 1:200){y4[i] = sin(i) + rnorm(1,0,0.5)}

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

n = 1:300
lag.tseries  = NULL
lag.atsa.min = NULL
lag.atsa.max = NULL
for(i in 1:300){
  lag.tseries[i] = trunc(4*(n[i]/100)^(1/4))
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

tseries::kpss.test(y2,null="Level")
tseries::kpss.test(y2,null="Trend")
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

tseries::adf.test(y0)
stationary.test(y0,method = "adf")

tseries::adf.test(y1)
stationary.test(y1,method = "adf")

tseries::adf.test(y2)
stationary.test(y2,method = "adf")

tseries::adf.test(y3)
stationary.test(y3,method = "adf")

tseries::adf.test(y4)
stationary.test(y4,method = "adf")

tseries::adf.test(y5)
stationary.test(y5,method = "adf")


