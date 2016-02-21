library(fpp)
GSS <- read.csv("GSSales.csv")
GSS <- ts(GSS, start= 1992, frequency=12)[,1]
y.TR <- window(GSS, start=1992, end=2013)
y.TE <- window(GSS, start=2013.083)

#fit a Holt-Winters exponential smoothing model to the training subset of the supermarket sales data
fit.HW <- ets(y.TR, model="AAM", damped=FALSE, restrict=FALSE)
summary(fit.HW)
plot(y.TR, main="Holt-Winters Fitted Model")
lines(fit.HW$fitted, col="orange")

#prepare a two-year ahead forecast for the sales of supermarkets in the US
fcst.HW <- forecast(fit.HW,h=24)
plot(fcst.HW,main="Holt-Winters Forecast Model Based on Training Data")
points(y.TE, pch=16)
lines(fit.HW$fitted, col="orange")
accuracy(fcst.HW,y.TE)

#Fit Optimal Model
fit.OV <- ets(y.TR, model="ZZZ", restrict =FALSE)
summary(fit.OV)

#compute the two-year ahead forecast for this model and compute the RMSE for this model with respect to the testing data set.
fct.OV <- forecast(fit.OV,h=24)
summary(fct.OV)
accuracy(fct.OV,y.TE)

#BoxCox Transformation
L<-BoxCox.lambda(y.TR)
fit.OPT <- ets(y.TR, model="ZZZ", restrict =FALSE, lambda=L)
summary(fit.OPT,h=24)
plot(y.TR, main="Optimal Fitted Model - Box Cox Transformation")
lines(fit.OPT$fitted, col="orange")
#compute the two-year ahead forecast and the RMSE for this model
fct.OPT <- forecast(fit.OPT,h=24)
summary(fct.OPT)
accuracy(fct.OPT,y.TE)

#Fit HW Model to the Box Cox re-scaled training data
fit.HWRT <- ets(y.TR, model="AAM", damped=FALSE, restrict =FALSE, lambda=L)
summary(fit.HWRT)
#compute the two-year ahead forecast and the RMSE for this model
fct.HWRT <- forecast(fit.HWRT,h=24)
summary(fct.HWRT)
accuracy(fct.HWRT,y.TE)

#look at test and training accuracy metrics for 4 models
accuracy(fcst.HW,y.TE)
accuracy(fct.OV,y.TE)
accuracy(fct.OPT,y.TE)
accuracy(fct.HWRT,y.TE)

#Compare plots of the two-year ahead forecasts 
plot(fct.OV)
points(y.TE, pch=16)
lines(fit.OV$fitted, col="orange")
plot(fct.OPT,main="Optimal Forecast Model Based on Rescaled Training Data")
points(y.TE, pch=16)
lines(fit.HWRT$fitted, col="orange")

plot(fct.OPT,main="Optimal Forecast Model Based on Rescaled Training Data")
points(y.TE, pch=16)
lines(fit.HWRT$fitted, col="orange")
plot(fct.HWRT,main="Holt-Winters Forecast Model Based on Rescaled Training Data")
points(y.TE, pch=16)
lines(fit.HWRT$fitted, col="orange")

#Print the mean, 80% and 95% monthly prediction intervals for 1 through 24 months ahead starting in February 2015 using a Box-Cox transformed model and the optimized model 
fit.OPT9 <- ets(GSS, model="ZZZ", restrict =FALSE, lambda=L)
forecast(fit.OPT9,h=24)
fit.HWRT9 <- ets(GSS, model="AAM", damped=FALSE, restrict =FALSE, lambda=L)
forecast(fit.HWRT9,h=24)
