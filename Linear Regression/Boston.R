library(MASS)
?Boston
Boston
load(file="BostonDerived.RData")
boxplot(Boston$lstat)
boxplot(Boston$medv)
quantile(Boston$lstat, 0.25)
median(Boston$lstat)
quantile(Boston$lstat, 0.75)
quantile(Boston$medv, 0.25)
median(Boston$medv)
quantile(Boston$medv, 0.75)
plot(Boston$medv,Boston$lstat)
Bostrain <- Boston[1:300, ] 
Bostest <- Boston[301:506, ]
mlr = lm(medvDerived ~ lstat + rm + chas + indus + tax + rad + black, data=Bostrain)
sum = summary(mlr)
sum
summary(mlr)$coefficients
mean(sum$residuals^2)
mean((Bostrain$medvDerived - mlr$fitted.values)^2)
test <- predict(mlr,newdata=Bostest)
summary(test)
mean((Bostest$medvDerived - test)^2)
mean((Bostest$medvDerived - test)^2)


load(file="rrdata.RData")
rr.mlr <- lm(crime ~ poverty + single, data = rrdata)
summary(rr.mlr)
rr.huber <- rlm(crime ~ poverty + single, data = rrdata)
summary(rr.huber)
plot(rr.mlr$fitted.values,rr.mlr$residuals)
plot(rr.huber$fitted.values,rr.huber$residuals)
outlier = identify(rr.mlr$fitted.values,rr.mlr$residuals, n=3)
rrdataNoOutlier = rrdata[-outlier,]
rr.mlr <- lm(crime ~ poverty + single, data = rrdataNoOutlier)
summary(rr.mlr) 
rr.huber <- rlm(crime ~ poverty + single, data = rrdataNoOutlier)
summary(rr.huber)
plot(rrdata$crime,rr.mlr$fitted.values)
abline(a=0,b=1)
plot(rrdata$crime,rr.huber$fitted.values)
abline(a=0,b=1)
plot(rrdataNoOutlier$crime,rr.mlr$fitted.values)
abline(a=0,b=1)
plot(rrdataNoOutlier$crime,rr.huber$fitted.values)
abline(a=0,b=1)