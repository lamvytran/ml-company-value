library(RPostgres)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(Matrix)
library(glmnet)
library(stats) 
library(dplyr) 
library(randomForest) 
library(forecast)
library(e1071)
library(plyr)
library(readr)
library(neuralnet)
library(nnet)
library(xgboost)
library(caTools)
library(foreach)
library(doParallel)
library(iterators)
library(parallel)
library(adabag)





pca.x_train <-subset(pca_train,select=-c(objective))
pca.y_train <-factor(pca_train$objective,levels = c(0,1),labels=c("No","Yes"))
pca.x_valid <-subset(pca_valid,select=-c(objective))
pca.y_valid <- factor(pca_valid$objective,levels = c(0,1),labels=c("No","Yes"))
pca.x_test <-subset(pca_test,select=-c(objective))
pca.y_test <- factor(pca_test$objective,levels = c(0,1),labels=c("No","Yes"))


#Classification Tree
pca.tree_model <- rpart(objective ~ ., data = pca_train, method = "class")
pca.tree_predict <- factor(predict(pca.tree_model, newdata = pca_valid, type = "class"))
pca.tree_matrix <- confusionMatrix(factor(pca_valid$objective),pca.tree_predict)


#Neural Network 
set.seed(10)
nnet_params <- trainControl(method = "repeatedcv", number = 10, repeats=5)

nnet_model <- train(pca.x_train, pca.y_train,
                    method = "nnet",
                    trControl= nnet_params,
                    preProcess=c("scale","center"),
                    na.action = na.omit,
                    tuneGrid = expand.grid(size = 5, decay = 0.01)#adjust the layers
)
pca.nnet_predict <-predict(nnet_model, pca_valid)
pca.nnet_matrix <- confusionMatrix(pca.y_valid,pca.nnet_predict)
pca.nnet_matrix

#XGBoost
set.seed(9)
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "binary:logistic",
  eval_metric = "logloss")

pca.xgb.train = xgb.DMatrix(data=as.matrix(pca.x_train),label=pca_train$objective)
pca.xgb.valid = xgb.DMatrix(data=as.matrix(pca.x_valid),label=pca_valid$objective)
pca.xgb_model <- xgb.train(data=pca.xgb.train,params = xgb_params,nrounds = 100,verbose = 1)
pca.xgb_predict<- predict(pca.xgb_model,pca.xgb.valid,reshape=TRUE)
pca.xgb_predict<-ifelse(pca.xgb_predict>=0.5,1,0)
pca.xgb_matrix<- confusionMatrix(factor(pca_valid$objective),factor(pca.xgb_predict))















