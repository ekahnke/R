library(fpp)
retail = read.csv('US Annual Retail Trade Index.csv')
retail = ts(retail[,-1], start = 1955)

#Q1
##number of differences required to make it stationary
ndiffs(retail)
retail.diff = diff(log(retail),1)
par(mfrow=c(1,2))
plot(retail, main = "Not Stationary",ylab = "Time Series Retail Trade Index")
plot(retail.diff, main = "Stationary",ylab = "Time Series Retail Trade Index")

##to test whether stationary, perform ADF test
adf.test(retail.diff, alternative ="stationary")
##The Null hypothesis is that the time series is not stationary. After conducting the ADF test on it, the p-value is 0.01 which means that the NUll hypothesis can be rejected. Thus, this data is stationary.

##Based on the ACF and PACF of the stationary series justify the selection of an ARIMA model with the property that pq=0  that it either p=0 or q=0.  Plot and discuss the diagnostics of the residuals of your selected model.
retail.diff = diff(retail)

par(mfrow = c(1,2))
Acf(retail.diff)
Pacf(retail.diff)
tsdisplay(retail.diff)

Arima = Arima(retail.diff, order = c(2,1,0))
summary(Arima)
tsdiag(Arima)
Box.test(residuals(Arima), type=c("Ljung-Box"), lag=12, fitdf=2)


##Obtain the value of the best lambda for the Box-Cox transformation of the time series y.
L<-BoxCox.lambda(retail)
L
#The best value of Lambda is 0.5624
retail.bc = BoxCox(retail, L)

Arima011 = Arima(retail.bc, order=c(0,1,1))
summary(Arima011)

Arima210 = Arima(retail.bc, order=c(2,1,0))
summary(Arima210)

Arima110 = Arima(retail.bc, order=c(1,1,0))
summary(Arima110)

##Use the auto.arima(.) function with the value of the lambda selected above to obtain a non-seasonal ARIMA model.
AArima <- auto.arima(retail, lambda = L)
summary(AArima)


par(mfrow = c(1,1))
plot(forecast(AArima), h = 1, xlab= "Time", ylab = "Sales")

##use the fitted(.) function to obtain the fitted values obtained by the model, then use your explicit model to forecast the mean value of the US Retail Index for 2015 through 2017. 
fitted = fitted(AArima)
fitted.bc = BoxCox(fitted, L)
error = tail(retail.bc, n =1 ) -tail(fitted.bc, n =1 )

yhat.2015 = 0.2016 + 0.4043* error + (tail(retail.bc, n =1 ))
yhat.2016 = 0.2016 + 0 + 0.4043* 0 + yhat.2015
yhat.2017 = 0.2016 + 0 + 0.4043* 0 + yhat.2016

InvBoxCox(yhat.2015,L)
InvBoxCox(yhat.2016,L)
InvBoxCox(yhat.2017,L)
forecast(AArima, h = 3)

plot(forecast(AArima, h = 3))

##view the dynamic behavior of the data
View (hsales2)
tsdisplay(hsales2)

adf.test(hsales2,alternative = "stationary")
auto.arima(hsales2)
diff(log(hsales2))
adf.test(hsales2, alternative = "stationary")

##Compute the 12-period difference of the dataset hsales2 and store the differenced time-series in h.D12 
hsales2 = ts(hsales2)
h.D12 = diff(log(hsales2), lag= 12)
tsdisplay(h.D12)

adf.test(h.D12, alternative ="stationary")

PartD = Arima(hsales2, order = c(1,0,0), seasonal = c(1,1,0))
summary(PartD)
tsdisplay(residuals(PartD))

AA.hsales = auto.arima(hsales2)
summary(AA.hsales)


##Use the forecast(.) function and the best model you obtained above to prepare a plot of the two-year forecast (h=24) of the mean, 80% and 95% confidence intervals of the sales of single-family new-houses in the US.
forecasts = forecast(AA.hsales, h = 24)
summary(forecasts)

par(mfrow = c(1,1))
plot(forecasts, ylab = "Sales")

##Consider an ARIMA(1,0,0) (0,1,1)_12 model  for the forecasting of the hsales2 time-series.
ArimaQ10 = Arima(hsales2, order = c(1,0,0), seasonal = c(0,1,1))
summary(ArimaQ10)

##Fit the ARIMA(1,1,0) (0,1,1)_12 model to the hsales2 time-series 
ArimaQ11= Arima(hsales2, order = c(1,1,0), seasonal = c(0,1,1))
summary(ArimaQ11)

tail(fitted(ArimaQ11), n = 2) - tail(hsales2,n =2)