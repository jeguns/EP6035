

# Conceptos básicos -------------------------------------------------------

# Serie de tiempo muestral vs poblacional

x1 = c(-5.23, -6.96, -40.98, -3.55, -13.88, 35.21, 11.8, -5.54, -0.14, -14.19, -1.11)
x2 = c(-6.45, -8.32, -45.45, -5.01, -15.87, 37.93, 11.5, -8.04, 0.21, -13.33, -1.19)
x3 = c(-7.28, -8.04, -44.41, -4.39, -12.03, 34.59, 12.29, -4.7, 0.84, -17.34, -1.8)

plot(x1, type="l", lwd = 3, ylim = c(min(c(x1,x2,x3)), 
                                     max(c(x1,x2,x3))))
lines(x2, col="red", lwd = 3)
lines(x3, col="gold", lwd = 3)

# Varianza

x1

(gamma0 = sum((x1-mean(x1))^2)/10) # varianza convencional

(gamma0 = sum((x1-mean(x1))^2)/11) # varianza en S.T.
acf(x1,type="covariance",plot=F)

# Autocovarianza

library(quantmod)
Lag(x1)

x1n  = x1[-1]
x1ln = Lag(x1)[-1]

sum((x1n-mean(x1n))*(x1ln-mean(x1ln)))/9 # autocovarianza convencional
cov(D, use = "pairwise.complete.obs") # autocovarianza convencional

sum((x1n-mean(x1))*(x1ln-mean(x1)))/11 # autocovarianza en S.T.
acf(x1,type="covariance",plot=F) # autocovarianza en S.T.
acf(x1,type="covariance",plot=T)

# Autocorrelación

sum((x1n-mean(x1n))*(x1ln-mean(x1ln)))/(sqrt(sum((x1n-mean(x1n))^2))*sqrt(sum((x1ln-mean(x1ln))^2))) # autocorrelación convencional

sum((x1n-mean(x1))*(x1ln-mean(x1)))/sum((x1-mean(x1))^2) # autocorrelación en S.T.
  
(gamma1 = sum((x1n-mean(x1))*(x1ln-mean(x1)))/11)
gamma1/gamma0
acf(x1,plot=F)

(gamma2 = sum((x1[-c(1:2)]-mean(x1))*(Lag(x1,2)[-c(1:2)]-mean(x1)))/11)
gamma2/gamma0
acf(x1,plot=F)

(gamma3 = sum((x1[-c(1:3)]-mean(x1))*(Lag(x1,3)[-c(1:3)]-mean(x1)))/11)
gamma3/gamma0
acf(x1,plot=F)

(gamma7 = sum((x1[-c(1:7)]-mean(x1))*(Lag(x1,7)[-c(1:7)]-mean(x1)))/11)
gamma7/gamma0
acf(x1,plot=F)

# Ruido blanco iid

set.seed(120)
y = rnorm(100)
plot(y, type="b", pch=18)
abline(h=0)