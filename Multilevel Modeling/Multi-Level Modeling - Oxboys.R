library(ggplot2)
library(nlme)
oxboys <- read.csv('oxboys.csv')
attach(oxboys)

ggplot(oxboys, aes(x=year, y=height)) + geom_point() + stat_smooth(method="lm")

ggplot(oxboys, aes(x=year, y=height, group=id)) + geom_point() + stat_smooth(method="lm")

#split on first 2 years
oxboys.train <- subset(oxboys, year < 3)
oxboys.test <- subset(oxboys, year >= 3)

#global
global_lm <- lm(height~year,data=oxboys.train)
predict_global_lm <- predict(global_lm,newdata=oxboys.test)
mean((predict_global_lm-oxboys.test$height)^2)

#local
for( key in unique(oxboys.train$id)){
  oxboys.train.ind <- subset(oxboys.train, id==key)
  oxboys.test.ind <- subset(oxboys.test, id==key)
  local_lm <- lm(height~year,data=oxboys.train.ind)
  predict_local_lm <- predict(local_lm,newdata=oxboys.test.ind)
  local_mse <- mean((predict_local_lm-oxboys.test.ind$height)^2)
  print(cat('ID: ',oxboys.train.ind$id,', MSE: ',local_mse))
}

#multilevel
mlm.obj <- lme(height~year, data=oxboys.train, random=list(id=pdDiag(~year)))
predict_mlm <- predict(mlm.obj, newdata=oxboys.test, level=1)
mlm_mse <- mean((predict_mlm-oxboys.test$height)^2)
print(mlm_mse)

#split on first 6 years
oxboys.train <- subset(oxboys, year < 7)
oxboys.test <- subset(oxboys, year >= 7)

#global
global_lm <- lm(height~year,data=oxboys.train)
predict_global_lm <- predict(global_lm,newdata=oxboys.test)
mean((predict_global_lm-oxboys.test$height)^2)

#local
for( key in unique(oxboys.train$id)){
  oxboys.train.ind <- subset(oxboys.train, id==key)
  oxboys.test.ind <- subset(oxboys.test, id==key)
  local_lm <- lm(height~year,data=oxboys.train.ind)
  predict_local_lm <- predict(local_lm,newdata=oxboys.test.ind)
  local_mse <- mean((predict_local_lm-oxboys.test.ind$height)^2)
  print(cat('ID: ',oxboys.train.ind$id,', MSE: ',local_mse))
}

#multilevel
mlm.obj <- lme(height~year, data=oxboys.train, random=list(id=pdDiag(~year)))
predict_mlm <- predict(mlm.obj, newdata=oxboys.test, level=1)
mlm_mse <- mean((predict_mlm-oxboys.test$height)^2)
print(mlm_mse)
