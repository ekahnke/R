library(fpp)
sales = read.csv("US Electronics and Appliance Sales.csv")
sales = ts(sales[,-1], start = 2000, frequency = 12)

#plot this demand time series 
plot(sales, ylab='Demand')

#fit a seasonal ARIMA model to the entire time series and report the summary fit statistics and the residual diagnostics
L<-BoxCox.lambda(sales)
L 
#best value of Lambda is 0.09028898

AArimaQ2 <- auto.arima(sales, lambda = L)
summary(AArimaQ2)
tsdiag(AArimaQ2)

#Examine the ACF and PACF of this time series and difference it enough times until the time series becomes stationary and it passes the ADF test.
y.PRE = window(sales,end=2008.55)
tsdisplay(y.PRE)
ndiffs(y.PRE)
y.PRE.diff = diff(y.PRE,1)
adf.test(y.PRE.diff, alternative ="stationary")

#fit a model to the pre-crisis demand. Report the summary fit data and residual diagnostics.
AArimaQ4 <- auto.arima(y.PRE, lambda = L)
summary(AArimaQ4)
tsdiag(AArimaQ4)

#Quantify the impact of the financial crisis on the sales of this segment of the economy.
forecastQ5 = forecast(AArimaQ4, h = 24)
forecastQ5$mean

#monthly impact
forecastQ5$mean - window(sales,start=2008.55,end=2010.55)

#annual impact
forecastQ5$mean[1:12] - window(sales,start=2008.55,end=2009.55)
year08_09 = forecastQ5$mean[1:12] - window(sales,start=2008.55,end=2009.55)
sum(year08_09)
#13393.47

forecastQ5$mean[13:24] - window(sales,start=2009.55,end=2010.55)
year09_10 = forecastQ5$mean[13:24] - window(sales,start=2009.55,end=2010.55)
sum(year09_10)
#19735.76

#Obtain the step intervention model 
x.D <-  1*(seq(sales)>=104)
#ARIMA(2,1,0)(1,1,0)[12] 
ArimaQ6 = Arima(sales, order = c(2,1,0), seasonal = c(1,1,0), lambda = L, xreg = x.D)
summary(ArimaQ6)
tsdiag(ArimaQ6)

#obtain a 24-month ahead forecast
forecastQ7 = forecast(AArimaQ2, h = 24)
forecastQ7
forecastQ7$mean
sum(forecastQ7$mean)
mean(forecastQ7$mean)
sd(forecastQ7$mean)

#obtain a 24-month ahead forecast of US sales of electronics and appliance stores
x.FD <- c(rep(1,24))
forecastQ8 = forecast(ArimaQ6, h = 24, xreg = x.FD)
forecastQ8
forecastQ8$mean
sum(forecastQ8$mean)
mean(forecastQ8$mean)
sd(forecastQ8$mean)