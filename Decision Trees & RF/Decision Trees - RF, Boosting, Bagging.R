##We are going to analyze the same diabetes dataset to predict whether or not an individual suffers from diabetes. We will be using a decision tree classifier with and without the meta methods - Bagging and Boosting.
  
library(rpart)
library(caret)
library(randomForest)
library(gbm)

train = read.csv('diabetes_train-std.csv')
test = read.csv('diabetes_test-std.csv')

#Fit a classification tree. Plot the tree, and report the mean error rate (fraction of incorrect labels) on test data. Report the confusion matrix.
fit = rpart(classvariable~., method="class", data=train)
par(mar=c(1,1,1,1))
plot(fit, uniform=TRUE, main = "Classification Tree - Diabetes")
text(fit, use.n=TRUE, all=TRUE, cex=.5)
pred = predict(fit,test, type="class")
confusionMatrix(test$classvariable, pred)
cf_dt = confusionMatrix(test$classvariable, pred)
cf_dt$overall['Accuracy']
1-cf_dt$overall['Accuracy']

#Analyze the data using random forests. Report the mean error rate and the confusion matrix.
#for loop for finding optimal number of trees
a = array()
num_tree = c(25,50,75,100,125,150,175,200,225,250)
for (i in num_tree){
  set.seed(77)
  rf = randomForest(as.factor(classvariable)~., data= train, ntree = i)
  predrf = predict(rf, test, type = "class")
  cfrf = confusionMatrix(test$classvariable, predrf)
  a  <- append(a, 1-cfrf$overall['Accuracy'])
}
par(mar=c(5,5,5,3))
plot(a[-1], main = "Mean Error Rate w.r.t. Number of Trees", xlab = "Number of Trees", xaxt = "n",ylab = "Mean Error Rate", type = "l")
axis(1, at = 1:length(num_tree), labels = num_tree)
set.seed(77)
rf75 = randomForest(as.factor(classvariable)~., data = train, ntree=75)
pred75 = predict(rf75, test, type = "class")
confusionMatrix(test$classvariable, pred75)
cf_rf = confusionMatrix(test$classvariable, pred75)
cf_rf$overall['Accuracy']
1-cf_rf$overall['Accuracy']

#Use gradient boosted decision tree to analyze the data. Report the mean error rate and the confusion matrix.
gbmGrid = expand.grid(.interaction.depth = (1:5)*2, .n.trees = seq(2,10,2)*10, 
                      .shrinkage = c(0.01, 0.05, 0.1, 0.2),
                      .n.minobsinnode = c(1,5,10,20))
fitcontrol = trainControl(method = "cv", number =10)
gbmFit = train(as.factor(classvariable)~., data = train, method = "gbm", 
               trControl = fitcontrol,
               tuneGrid = gbmGrid)
predGBM = predict(gbmFit,newdata = test)
confusionMatrix(test$classvariable, predGBM)
cf_gbdt = confusionMatrix(test$classvariable, predGBM)
cf_gbdt$overall['Accuracy']
1-cf_gbdt$overall['Accuracy']
