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

target_old <- read_excel("target_2012_18.xlsx")
head(target_old)

# Modifying column names for simplicity
colnames(target_old) <- c("y", "x1", "x2", "x3","x4","x5","x6", "x7", "x8",
                    "x9", "x10", "x11", "x12", "x13", "x14","x15","x16","x17",
                    "x18","x19","x20", "x21","x22", "x23")
model_1 <- lm(y~ .-y, data = target_old)

summary(model_1)

lm(formula = y ~., data =target_old)
lm_AIC <- step(model_1, direction = 'backward', k =2)
lm_BIC <- step(model_1, direction = 'backward', k = log(nrow(target_old)))

summary(lm_AIC)
summary(lm_BIC)

model_new <- lm(y ~ x4 + x5 + x7+ x14  + x16 + x17 + x18 + x19 + x20+x21+ x22, data =target_old)
summary(model_new)

#model_n <- lm(y ~  x5 + x7+ x14  + x17 + x18 + x19 + x20+x21+ x22, data = target_old)
summary(model_new)


lm_BIC_2 <- step(model_new, direction = 'backward', k = log(nrow(target_old)))
summary(lm_BIC_2)
# Test for Gass-Markov assumption

# Auto-correlation
durbinWatsonTest(model_new)
# d-w stat between 1.5 -2.5 sp no auto-correlation
#Conditional heteroskedasticity
bptest(model_new)
# p-value is lower than 5% significance

# Normality of residuals
par(mfcol = c(2,1))
plot(model_2,2)
hist(model_2$residuals)

#Multicolliearity
vif(model_new) # Fir stock prediction higher the better

# Testing model fitness
fitted(model_new)

# Residuals analysis 
k = residuals(model_new)
plot(model_new$resid,type='l')   
Box.test(model_new$resid,lag=10,type='Ljung') 
# low value; ARIMA model will be needed as signs of auto-correlation in time seires

Box.test(k^2,lag=10,type='Ljung') 
# Above 5% significance. (79%) No volatility clustering; ARCH/GARCH model not necessary to be buil
par(mfcol = c(3,1))

# Test for stationarity
adf.test(model_new$residuals)
# low p-value we reject noll hypothesis( no presence of stationarity)

# Residual analysis for ARIMA model setup
par(mfcol = c(2,1))
Acf(model_new$residuals,lag = 36)
Pacf(model_new$residuals,lag = 36)


par(mfcol = c(3,1))
plot(k^2,type='l') 
Acf(k^2,lag = 36)
Pacf(k^2,lag = 36)
# low value; ARIMA model will be needed as signs of auto-correlation in time seire
par(mfcol = c(2,1))
Acf(model_n$residuals,lag = 36)
Pacf(model_n$residuals,lag = 36)


# Check best model
auto.arima(model_new$residuals, d = 0)


m1 <- arima(k, order=c(2,0,2))
m1

c1 = c(0,NA,0,NA,NA)
m2 <- arima(model_new$residuals, order=c(2,0,2), fixed = c1)
m2
predict(m2,2) # -0.491


#Check volaitility using GARCH # No need for Garch as Box test value is high
#model_3 = garchFit(formula = ~ arma(2,2)+garch(1,1), data = k, cond.dist = "std")
#summary(model_3)
#predict(model_3,5)


model_3 = garchFit(formula = ~arma(0,2) + garch(1,1), data = model_2$residuals, cond.dist = "std")
summary(model_3)
predict(model_3,5)


# Values from 2019 financial statements
#X4: 39.086666 ( wmt stock)
#X5: 3.568 (EV/REVENUE)
# X7: 18665000000 (TOTAL REVENUE)
# X14:43741000000 (TOTAL ASSETS)
# X16:11396000000 (inventory)
# X17:10513000000 ( long term)
# X18:-2803000000 (Working capital)
# X19: 506677740 (shares outstanding)
# X20: 1347000000 (op. cash flow)
# X21:-138000000 ( change in workingcapita)
# X22: -1009000000 (capital expenditure)
# Values from 2019 financial statement to create dataframes 
new <- data.frame(x4 =c(39.086),x5 = c(3.568) ,x7 = c(18665000000), x14 = c(43741000000)
                  ,x16 = c(11396000000) ,x17 = c(10513000000) ,  x18= c(-2803000000), x19 =c(506677740) 
                  ,x20= c(1347000000), x21 =c(-138000000),x22= c(-1009000000))

x <-predict(model_new, newdata=new)
x # 83.2222

price <- x - 0.491
price
# As per the model stock price predicted was  $ 82.73 which is a buy call 
# because at end of 2018 stock price was  $ 66 ( which suggested a 25%) upmove
# as per the 2019 stock updates targets of 86 were reached in June-july of 2019 and which further moved ahead.
# So our moved showed insights as per the move seen in 2019