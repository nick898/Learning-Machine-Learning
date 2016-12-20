#----------------------------BEGIN LOAD PACKAGES/FUNCTIONS/DATA---------------------------------#
#Load libraries
library(randomForest)
library(ggplot2)
library(ModelMetrics)
library(caret)

#Read cleaned data
# Data was cleaned by removing all NAs via the method outline here:
# https://www.kaggle.com/bisaria/house-prices-advanced-regression-techniques/handling-missing-data
train = read.csv("cleanedData.csv")

#Format train data frame and display structure
train = train[1:1460,3:82]
train$GarageYrBlt = as.numeric(train$GarageYrBlt)
str(train)

#----------------------------END LOAD PACKAGES/FUNCTIONS/DATA---------------------------------#

#----------------------------BEGIN CV TRAINING/TEST SET CREATION---------------------------------#

#Let's perform 5-fold cross validation so we partition our training data set
set.seed(100)
folds = createFolds(train$SalePrice,k = 10)
train1 = train[-folds[[1]],1:80]
train2 = train[-folds[[2]],1:80]
train3 = train[-folds[[3]],1:80]
train4 = train[-folds[[4]],1:80]
train5 = train[-folds[[5]],1:80]
train6 = train[-folds[[6]],1:80]
train7 = train[-folds[[7]],1:80]
train8 = train[-folds[[8]],1:80]
train9 = train[-folds[[9]],1:80]
train10 = train[-folds[[10]],1:80]


#Let's create our cross validation "test" set...note that this 
#data includes the target value "SalePrice"
cvTest1 = train[folds[[1]],1:80]
cvTest2 = train[folds[[2]],1:80]
cvTest3 = train[folds[[3]],1:80]
cvTest4 = train[folds[[4]],1:80]
cvTest5 = train[folds[[5]],1:80]
cvTest6 = train[folds[[6]],1:80]
cvTest7 = train[folds[[7]],1:80]
cvTest8 = train[folds[[8]],1:80]
cvTest9 = train[folds[[9]],1:80]
cvTest10 = train[folds[[10]],1:80]

#----------------------------END CV TRAINING/TEST SET CREATION---------------------------------#


#----------------------------BEGIN BUILDING MODELS/MAKING PREDICTIONS---------------------------------#
outRMSE = rep(0,length(seq(5,40,5)))
count = 1
pb <- txtProgressBar(min = 0, max = 14, style = 3)
ptm <- proc.time()

for (i in seq(5,40,5)){

  #Build models
  modelRF1 = randomForest(SalePrice ~ ., data = train1, nodesize = i)
  modelRF2 = randomForest(SalePrice ~ ., data = train2, nodesize = i)
  modelRF3 = randomForest(SalePrice ~ ., data = train3, nodesize = i)
  modelRF4 = randomForest(SalePrice ~ ., data = train4, nodesize = i)
  modelRF5 = randomForest(SalePrice ~ ., data = train5, nodesize = i)
  modelRF6 = randomForest(SalePrice ~ ., data = train6, nodesize = i)
  modelRF7 = randomForest(SalePrice ~ ., data = train7, nodesize = i)
  modelRF8 = randomForest(SalePrice ~ ., data = train8, nodesize = i)
  modelRF9 = randomForest(SalePrice ~ ., data = train9, nodesize = i)
  modelRF10 = randomForest(SalePrice ~ ., data = train10, nodesize = i)
  
  #Make predictions using cross validation "test" sets
  Pred1 = predict(modelRF1, cvTest1[,1:79])
  Pred2 = predict(modelRF2, cvTest2[,1:79])
  Pred3 = predict(modelRF3, cvTest3[,1:79])
  Pred4 = predict(modelRF4, cvTest4[,1:79])
  Pred5 = predict(modelRF5, cvTest5[,1:79])
  Pred6 = predict(modelRF1, cvTest6[,1:79])
  Pred7 = predict(modelRF2, cvTest7[,1:79])
  Pred8 = predict(modelRF3, cvTest8[,1:79])
  Pred9 = predict(modelRF4, cvTest9[,1:79])
  Pred10 = predict(modelRF5, cvTest10[,1:79])

  RMSE1 = rmse(cvTest1$SalePrice, Pred1)
  RMSE2 = rmse(cvTest2$SalePrice, Pred2)
  RMSE3 = rmse(cvTest3$SalePrice, Pred3)
  RMSE4 = rmse(cvTest4$SalePrice, Pred4)
  RMSE5 = rmse(cvTest5$SalePrice, Pred5)
  RMSE6 = rmse(cvTest6$SalePrice, Pred6)
  RMSE7 = rmse(cvTest7$SalePrice, Pred7)
  RMSE8 = rmse(cvTest8$SalePrice, Pred8)
  RMSE9 = rmse(cvTest9$SalePrice, Pred9)
  RMSE10 = rmse(cvTest10$SalePrice, Pred10)
  
  
  avg = mean(c(RMSE1,RMSE2,RMSE3,RMSE4,RMSE5,RMSE6,RMSE7,RMSE8,RMSE9,RMSE10))
  
  outRMSE[count] = avg
  count = count + 1
  setTxtProgressBar(pb, count)
  
}
close(pb)
proc.time() - ptm

#----------------------------END BUILDING MODELS/MAKING PREDICTIONS---------------------------------#


