library(glmnet)
library(caret)
library(ROCR)
library(grid)
library(ggplot2)

train_std <- read.csv("diabetes_train-std.csv")
test_std <- read.csv("diabetes_test-std.csv")
train_log <- read.csv("diabetes_train-log.csv")
test_log <- read.csv("diabetes_test-log.csv")

x_train_std <- as.matrix(train_std[,-9])
y_train_std <- train_std[,9]
x_train_log <- as.matrix(train_log[,-9])
y_train_log <- train_log[,9]

x_test_std <- as.matrix(test_std[,-9])
y_test_std <- test_std[,9]
x_test_log <- as.matrix(test_log[,-9])
y_test_log <- test_log[,9]

logridge_std <- cv.glmnet(x_train_std, y_train_std)
predict_std = round(predict.cv.glmnet(logridge_std,newx = x_test_std), digits=0)
confusionMatrix(predict_std, y_test_std)

train_predict_std = round(predict.cv.glmnet(logridge_std,newx = x_train_std), digits=0)
confusionMatrix(train_predict_std, y_train_std)

logridge_log <- cv.glmnet(x_train_log, y_train_log)
predict_log = round(predict.cv.glmnet(logridge_log,newx = x_test_log), digits=0)
confusionMatrix(predict_log, y_test_log)

train_predict_log = round(predict.cv.glmnet(logridge_log,newx = x_train_log), digits=0)
confusionMatrix(train_predict_log, y_train_log)

## generate std ROC
roc_std <- prediction(predict_std, y_test_std)
roc_std.perf <- performance(roc_std,"tpr","fpr")
tpr_std.points <- attr(roc_std.perf, "y.values")[[1]]
fpr_std.points <- attr(roc_std.perf,"x.values")[[1]]
auc_std <- attr(performance(roc_std, "auc"), "y.values")[[1]]
auc_std

perf_std <- performance(roc_std, measure = "tpr", x.measure = "fpr") 
plot(perf_std, col="black", main="ROC for Standardized & Log Transformed Models")

## generate log ROC
roc_log <- prediction(predict_log, y_test_log)
roc_log.perf <- performance(roc_log,"tpr","fpr")
tpr_log.points <- attr(roc_log.perf, "y.values")[[1]]
fpr_log.points <- attr(roc_log.perf,"x.values")[[1]]
auc_log <- attr(performance(roc_log, "auc"), "y.values")[[1]]
auc_log

perf_log <- performance(roc_log, measure = "tpr", x.measure = "fpr") 
plot(perf_log, col="black", add = TRUE)

## generate LIFT curve 
lift_std.perf <- performance(roc_std, "lift", "rpp")
lift_log.perf <- performance(roc_log, "lift", "rpp")

plot(lift_std.perf, main="Lift Curves", colorize = TRUE)
plot(lift_log.perf, add = TRUE, colorize = TRUE)
