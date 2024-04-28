library(quantmod)
library("readxl")
library(memisc)
library(dplyr)
library(lmtest)
library(sjPlot)
library(sgof)
library(ggplot2)
library(hexbin)
library(lmtest)
library(GGally)
library(rugarch)
library(forecast)
library(fGarch)
library(fUnitRoots)
library(tseries)
library(car)

vale <- read_excel("vale2.xlsx")
head(vale)

# Modifying column names for simplicity
colnames(vale) <- c("y", "x1", "x2", "x3","x4","x5","x6", "x7", "x8",
                    "x9", "x10", "x11", "x12", "x13", "x14","x15","x16","x17",
                    "x18","x19","x20", "x21","x22", "x23", "x24", "x25","x26","x27")
model_1 <- lm(y~ .-y, data = vale)

summary(model_1)

lm(formula = y ~., data =vale)
lm_AIC <- step(model_1, direction = 'backward', k =2)
lm_BIC <- step(model_1, direction = 'backward', k = log(nrow(vale)))

summary(lm_AIC)
summary(lm_BIC)

model_2 <- lm(y ~ x2 + x4 + x5 +x9 + x11 + x12 + x13 + x17 + x18 + x19 + x21 +  x22, data =vale)
summary(model_2)

#model_2 <- lm(y ~ x2 +  x5 +x9 + x11  + x13 + x18 + x19 + x21 + x22, data =vale)

lm_BIC_2 <- step(model_2, direction = 'backward', k = log(nrow(vale)))
summary(lm_BIC_2)
# Test for Gauss-Markov assumption

# Auto-correlation
durbinWatsonTest(model_2) #   No auto-correlation

#Conditional heteroskedasticity
bptest(model_2) # no conditional heteroskedasticity

# Normality of residuals
par(mfcol = c(2,1))
plot(model_2,2)
hist(model_2$residuals)

#Multicollinearity
vif(model_2) # For stock prediction higher the better

# Testing model fitness
fitted(model_2)

# Residuals analysis 
k = residuals(model_2)

plot(model_2$resid,type='l')   
Box.test(model_2$resid,lag=10,type='Ljung') # No autocorrelation in residuals
# No need for ARIMA
Box.test(k^2,lag=10,type='Ljung') 
# 6.64% volatility clustering insiginificant for 5% 
#but we will test ARCH/GARCH for 10% just for purpose of studies
# Residual analysis for ARIMA model setup
par(mfcol = c(2,1))
Acf(model_2$residuals,lag = 36)
Pacf(model_2$residuals,lag = 36)

# Residual analysis for volatility. clustering setup
par(mfcol = c(3,1))
plot(k^2,type='l') 
Acf(k^2,lag = 36)
Pacf(k^2,lag = 36)



 
# Test for stationarity
adf.test(model_2$residuals)

# Check best model
auto.arima(model_2$residuals, d = 0)
# arima(0,0,0) so further confirmation of our analysis

#Check volaitility using GARCH
# After running various models ; no need for GARCH also ; Tried to implement for case study purpose 
model_3 = garchFit(formula = ~ garch(1,1), data = model_2$residuals, cond.dist = "std", Trace=F)
summary(model_3)
predict(model_3,5)


model_3 = garchFit(formula = ~ garch(5,5), data = model_2$residuals, cond.dist = "std", Trace=F)
summary(model_3)
predict(model_3,5)

# norm better than std other model tried but best fit is arch(1,0)

# Final model
# y ~ x2 + x4 + x5 +x9 + x11 + x12 + x13 + x17 + x18 + x19 + x21 +  x22
# arch (1,0) for price prediction interval
#y = share rpice
#x2 = Rio tinto price = 75
#x4 = VIX =  21.67
#x5 = Iron ore price = 120.53
#x9 = total revenue = 10174400000.00
#x11 = Tax provision = 110730000
#x12 = depreciation = 742740000.00. ; 804583333.33(average)
#x13 = change in working capital = 11284000.00
#x17 = shares outstanding = 4243392869.00
#x18 = capital expenditure = -1526160000.00
#x19 = total assets = 100668700000.00
#x21 = inventory= 5426685000 ; 4660350000.00 (average)
#x22 = financial assets = 205291666.67

# Values from 2024 financial model
new <- data.frame(x2 =c(76),x4 = c(21.67), x5 = c(120.53), x9= c(10871400000), x11= c(1178317500), x12= c(742740000) , 
                  x13 = c(-123140000), x17 =c(4303392869) ,x18 = c(-1625160000) ,x19 = c(102639700000) , 
                  x21 = c(4660350000.00) ,  x22= c(205291666) )
# stock price :  (16.57961)

# Price prediction code 
x <-predict(model_2, newdata=new)
x


