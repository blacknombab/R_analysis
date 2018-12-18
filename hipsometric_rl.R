install.packages('lmtest')
install.packages('car')
require(lmtest)
require(car)
dwtest(mod2)

rm(list = ls())

dados  = read.table('dados.txt', header = T, dec = ',', quote = "")
dados
attach(dados)
summary(dados)

plot(dap2 ~ dap1 )

mod1 = lm(dap2 ~ dap1 )
summary(mod1)
v1 = log(dap2/dap1)/2

plot(ht ~ dap1)

# Modelos utilizados

## Lineares
### Linear simples
mlinear = lm(ht ~ dap1 )
summary(mlinear)
### Stoffels 
stoffels = lm(log(ht) ~ I(log(dap1)))
summary(stoffels)
### Henriksen
henriksen = lm(ht ~ I(log(dap1)))
summary(henriksen)
### Curtis
curtis = lm(log(ht) ~ I(1/dap1))
summary(curtis)
## Nao lineares
### Logistico
logistico = nls(ht ~ a/(1+ exp((b-dap1)/c)), 
                start = list(a = 24.1233,b = 10.2181 , c = 6.7431))           
summary(logistico)
### Monomolecular incompleto 
molecular = nls(ht ~ a*(1-exp(-b*dap1)), start = list(a = 27, b= 0.06))
summary(molecular)
### ChapRich
chaprich = nls(ht ~ a*(1-exp(-b*dap1))^c, 
               start = list(a = 25.85411, b = 0.07569, c = 1.18787))
summary(chaprich)
### Gompertz
gompertz = nls(ht ~ a*(exp(-b*exp(-c*dap1))), start = list(a = 24.80654,b = 2.13821, c = 0.10806))
summary(gompertz)


### AIC
aic = c(AIC(mlinear),
  AIC(stoffels),
  AIC(henriksen),
  AIC(curtis),
  AIC(logistico),
  AIC(molecular),
  AIC(chaprich),
  AIC(gompertz))
### BIC
bic = c(AIC(mlinear, k = log(nobs(mlinear))),
  AIC(stoffels, k = log(nobs(stoffels))),
  AIC(henriksen, k = log(nobs(henriksen))),
  AIC(curtis, k = log(nobs(curtis))),
  AIC(logistico, k = log(nobs(logistico))),
  AIC(molecular, k = log(nobs(molecular))),
  AIC(chaprich, k = log(nobs(chaprich))),
  AIC(gompertz, k = log(nobs(gompertz))))

modelo = c('Linear simples', 'Stoffels','Henriksen', 'Curtis','Logístico',
           'Monomolecular', 'Chapman & Richards', 'Gompertz')

aic_bic = as.data.frame(cbind(aic,bic))
names(aic_bic) = c('AIC', 'BIC')
aic_bic
cbind(modelo, aic_bic)
### Erro padrão
standardRes <- residuals(molecular)/summary(molecular)$sigma
hist(residuals(curtis))
### S_xy
S =  c(summary(mlinear)$sigma,
       exp(summary(stoffels)$sigma),
       summary(henriksen)$sigma,
       exp(summary(curtis)$sigma),
       summary(logistico)$sigma,
       summary(molecular)$sigma,
       summary(chaprich)$sigma,
       summary(gompertz)$sigma)
cbind(modelo, aic_bic,S)

### qqnorm
qqnorm((residuals(curtis)/summary(curtis)$sigma))
abline(a = 0, b = 1)



plot(ht ~ dap1, ylab = 'Altura total (m)', xlab = 'dap (cm)')
x = 1:100

lines(x,24.1233/(1+exp((10.2181-x)/6.7431)), lty = 1, lwd = 2)
lines(x,27.068488* (1 - exp(-0.060725 *x )), lty = 2, col = 2, lwd = 2)
lines(x,38.250*x/(20.738 + x), lty = 3, col = 12, lwd = 2)
lines(x,24.80647*(exp(-2.13824 * exp(-0.10806*x))), lty = 4, col = 21, lwd =2)

curve(24.1233/(1+exp((10.2181-x)/6.7431)),from = 1, to = 50, n=100)
predict(mod4)


par(mfrow = c(2,2))
plot(fitted(mod4),residuals(mod4))
abline(h = 0, col= 'red')
plot(fitted(molecular),standardRes)
abline(h = 0, col= 'red')
plot(dap1,residuals(mod6))
abline(h = 0, col= 'red')
plot(dap1,residuals(mod7))
abline(h = 0, col= 'red')
