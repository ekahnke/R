library (randomForest)
library (doParallel)
library(foreach)

Cars <- read.csv("Cars.csv")

prices <- Cars$price
Cars = Cars [,-c(1,7,14)]
auto = Cars
auto = auto [-14]

cl<-makeCluster(4)

registerDoParallel(4)

rf1<- foreach (ntree=rep(300,4), .combine = combine, .packages = 'randomForest') %dopar%
randomForest(x = auto, y = prices, ntree=ntree, mtry = 4, nodesize = 25, sampsize = 10000)

auto_cv <- auto[sample(nrow(auto))

cv1<- cvFit(rf, x = auto,y = price, K = 5, R = 1, foldType = "random")
cv2<- cvFit(rf, x = auto,y = prices, K = 5, R = 1, foldType = "random")