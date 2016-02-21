###use SGD to estimate the parameters of an MLR problem
## load the data
train = read.csv('bostonderived_train.csv')
test = read.csv('bostonderived_test.csv')

getRMSE <- function(pred, actual) {
  RMSE = sqrt(sum((pred-actual)^2)/length(pred))
  ## TODO: given a vector or predicted values and actual values, calculate MSE
  return (RMSE)
}

addIntercept <- function(mat) {
  ## add intercept to the matrix
  allones= rep(1, nrow(mat))
  return(cbind(Intercept=allones, mat))
}

predictSamples <- function(beta, mat) {
    ## TODO: compute the predicted value using matrix multiplication
    ## Note that for a single row of mat, pred = sum_i (beta_i * feature_i)
    return (beta%*%t(mat))
}

MAX_EPOCH = 100

sgd <- function(learn.rate, train, test, epoch=MAX_EPOCH) {
  ## convert the train and test to matrix format
  train.mat = as.matrix(train) 
  test.mat = as.matrix(test)

  ##  DONE TODO: get the number of rows in the train matrix
  
  N = nrow(train.mat)
  ##  DONE TODO: get the number of dimensions (columns) in the train matrix
  d = ncol(train.mat)

  ## standardize the columns of both matrices
  for (i in 1:d){
  ## DONE TODO: standardize the train and test matrices
  
    train.mat[, i]= (train.mat[, i] - mean(train.mat[, i]))/sd(train.mat[,i])
    test.mat[, i]= (test.mat[, i] - mean(train.mat[, i]))/sd(train.mat[,i])
    
  }

  ## add a feature to represent the intercept
  #-d takes out predictor variable medvDerived
  tmat <- addIntercept(train.mat[, -d])
  testmat <- addIntercept(test.mat[, -d])

  ## initialize all the coefficients to be 0.5
  beta = rep(0.5,d)
  beta = t(beta)
  j = 1
  mse.df <- NULL
  
  # predict training residuals
  pred_train =predictSamples(beta, tmat)
  pred_test = predictSamples(beta, testmat)
  tMse = getRMSE(pred_train, train$medvDerived)
  testMSE = getRMSE(pred_test, test$medvDerived)
  mse.df <- rbind(mse.df, data.frame(epoch=j, train=tMse, test=testMSE))
  
  alpha = learn.rate #learning rate
  
  while(j < MAX_EPOCH){  
    j=j+1;
    # for each row in the training data
    gradient <- (1/N)* (t(tmat) %*% ((tmat %*% t(beta)) -train$medvDerived ))
    beta <- beta - alpha  * t(gradient)
    
    pred_train = predictSamples(beta, tmat)
    pred_test = predictSamples(beta, testmat)
    tMse = getRMSE(pred_train, train$medvDerived)
    testMSE = getRMSE(pred_test, test$medvDerived)
    mse.df <- rbind(mse.df, data.frame(epoch=j, train=tMse, test=testMSE))
  } 
  
  return(mse.df)
}

## learning rate 1
l1.df <- sgd(0.0025, train, test)
plot(l1.df,main="Learning rate=0.0025")

## learning rate 2
l2.df <- sgd(0.01, train, test)
plot(l2.df,main="Learning rate=0.01")

## learning rate 3
l3.df <- sgd(0.095, train, test)
plot(l3.df,main="Learning rate=0.095")

## TODO: fit an MLR model to it
fit <- lm(medvDerived ~1, data=train)
## calculate RMSE for LM model
lm.mse = getRMSE(predict(fit, test), test$medvDerived)
## RMSE of batch test data
lm.mse

lm.train.mse = getRMSE(predict(fit,train), train$medvDerived)
## RMSE of batch training data
lm.train.mse