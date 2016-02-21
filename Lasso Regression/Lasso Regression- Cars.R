library(glmnet)
Cars = read.csv("Cars.csv")
car2 = Cars[,-c(1,3,5,8,10,11,12,13,16)] # lose irrelevant columns
attach(car2)
n = dim(car2)[1]
tr = sample(1:n,5000) 
logprice = log(price)

## create a full matrix of interactions (only necessary for linear model)
## do the normalization only for main variables.
XXca <- model.matrix(logprice~.*mileage, data=car2)[,-c(1,86)]
CAdata = data.frame(logprice,XXca)
XXca = scale(XXca)

Lasso.Fit = glmnet(XXca[tr,],XXca[tr])
Ridge.Fit = glmnet(XXca[tr,],XXca[tr],alpha=0)
par(mfrow=c(1,2))
plot(Lasso.Fit)
plot(Ridge.Fit)

CV.L = cv.glmnet(XXca[tr,], log(price[tr]),alpha=1)
CV.R = cv.glmnet(XXca[tr,], log(price[tr]),alpha=0)
sqrt(CV.L$cvm[38])

#1434.042
LamR = CV.R$lambda.1se
LamL = CV.L$lambda.1se

par(mfrow=c(1,2))
plot(log(CV.R$lambda),sqrt(CV.R$cvm),main="Ridge CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamR),lty=2,col=2,lwd=2)
plot(log(CV.L$lambda),sqrt(CV.L$cvm),main="LASSO CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamL),lty=2,col=2,lwd=2)


coef.R = predict(CV.R,type="coefficients",s=LamR)
coef.L = predict(CV.L,type="coefficients",s=LamL)

lasso.L = predict(CV.L,s=LamL, newx=XXca[-tr,])

sqrt(mean((exp(lasso.L)-price[-tr])^2))