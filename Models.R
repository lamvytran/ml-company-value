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



#Build a dump model working as a benchmark
table(final_df$objective)
dumb_predict <- factor(c(rep(0,nrow(df_test)/2),rep(1,nrow(df_test)/2)))
accuracy <- mean(dumb_predict == df_test$objective)
df_test$objective = factor(df_test$objective)
dumb_matrix <- confusionMatrix(df_test$objective,dumb_predict)


x_train <-subset(df_train,select=-c(objective))
y_train <-factor(df_train$objective,levels = c(0,1),labels=c("No","Yes"))
x_valid <-subset(df_valid,select=-c(objective))
y_valid <- factor(df_valid$objective,levels = c(0,1),labels=c("No","Yes"))
x_test <-subset(df_test,select=-c(objective))
y_test <- factor(df_test$objective,levels = c(0,1),labels=c("No","Yes"))

#10-fold Cross Validation to determine the optimal value for lambda
x_train.matrix <- model.matrix(~.,data=x_train)
x_valid.matrix <- model.matrix(~.,data=x_valid)
x_test.matrix <- model.matrix(~.,data=x_test)

#Lasso Regression, alpha = 1
lasso.fit <- cv.glmnet(x_train.matrix, y_train,type.measure ='deviance',alpha=1,family="binomial")
lasso.predict <- predict(lasso.fit, s = lasso.fit$lambda.1se, newx= x_valid.matrix)
mean((y_valid - lasso.predict)^2)

non_zero_coef_indices <- which(coef(lasso.fit) != 0)
non_zero_coef_names <- rownames(coef(lasso.fit))[non_zero_coef_indices]

#Ridge Regression, alpha = 0
ridge.fit <- cv.glmnet(x_train.matrix, y_train,type.measure ='deviance',alpha=0,family="binomial")
ridge.predict <- predict(ridge.fit, s = ridge.fit$lambda.1se, newx= x_valid.matrix, type='class')
ridge_matrix <- confusionMatrix(factor(df_valid$objective),factor(ridge.predict))

accuracy_df <- data.frame(alpha = numeric(), accuracy = numeric())

# Loop through different alpha values
for (alpha_value in seq(0, 1, by = 0.1)) {
  alpha.fit <- cv.glmnet(x_train.matrix, y_train, type.measure = 'deviance', alpha = alpha_value, family = "binomial")
  alpha_predict <- predict(alpha.fit, s = alpha.fit$lambda.1se, newx = x_valid.matrix, type = 'class')
  alpha_matrix <- confusionMatrix(factor(df_valid$objective), factor(alpha_predict))
  accuracy_df <- rbind(accuracy_df, data.frame(alpha = alpha_value, accuracy = alpha_matrix$overall['Accuracy']))
}
accuracy_df #Choosing lasso

#Classification Tree
tree_model <- rpart(objective ~ ., data = df_train, method = "class")
tree_predict <- factor(predict(tree_model, newdata = df_valid, type = "class"))
tree_matrix <- confusionMatrix(factor(df_valid$objective),tree_predict)

#Hyperparameter and Pruned Decision Tree
hyper_control <- rpart.control(minsplit =15, minbucket=5,maxdepth=10)
hyper_tree_model <- rpart(objective~.,data=df_train,method="class",control= hyper_control)
hyper_tree_predict <- factor(predict(hyper_tree_model, newdata = df_valid, type = "class"))
hyper_tree_matrix <- confusionMatrix(factor(df_valid$objective),hyper_tree_predict)

printcp(pruned_tree_model)

pruned_tree <- prune(hyper_tree_model,cp = 0.01)
pruned_tree_predict <- factor(predict(pruned_tree, newdata = df_valid, type = "class"))
pruned_tree_matrix<-confusionMatrix(factor(df_valid$objective),pruned_tree_predict)

#Support Vector Machine (slow)
set.seed(1)
scale.df_train<- scale(df_train)
svm_model <- svm(objective ~ ., data = scale.df_train,type='C-clasification', kernel = "linear")
svm_predict <- predict(svm_model, newdata = df_valid)
svm_matrix <- confusionMatrix(df_valid$objective,svm_predict)

#Neural Network 
set.seed(10)
nnet_params <- trainControl(method = "repeatedcv", number = 10, repeats=5)

nnet_model <- train(x_train, y_train,
                    method = "nnet",
                    trControl= nnet_params,
                    preProcess=c("scale","center"),
                    na.action = na.omit,
                    tuneGrid = expand.grid(size = 5, decay = 0.01)#adjust the layers
)
nnet_predict <-predict(nnet_model, df_valid)
nnet_matrix <- confusionMatrix(y_valid,nnet_predict)
nnet_matrix

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

xgb.train = xgb.DMatrix(data=as.matrix(x_train),label=df_train$objective)
xgb.valid = xgb.DMatrix(data=as.matrix(x_valid),label=df_valid$objective)
xgb_model <- xgb.train(data=xgb.train,params = xgb_params,nrounds = 100,verbose = 1)
xgb_predict<- predict(xgb_model,xgb.valid,reshape=TRUE)
xgb_predict<-ifelse(xgb_predict>=0.5,1,0)
xgb_matrix<- confusionMatrix(factor(df_valid$objective),factor(xgb_predict))

#ADA Boost
set.seed(10)
ada.df_train <- df_train
ada.df_train$objective<- factor(ada.df_train$objective)
ada.df_valid<-df_valid
ada.df_valid$objective <- factor(ada.df_valid$objective)
ada_model<- boosting(objective~.,data=ada.df_train,boos=TRUE,mfinal=3) 
#mfinal-control number of base learners (weak classifiers)
ada_predict <-predict(ada_model,ada.df_valid)
ada_matrix <- confusionMatrix(ada.df_valid$objective,factor(ada_predict))


#Results on test set

test.lasso.predict <- predict(lasso.fit, s = lasso.fit$lambda.1se, newx= x_test.matrix, type='class')
test.lasso.predict <- ifelse(test.lasso.predict=="Yes",1,0)
test.lasso_matrix <- confusionMatrix(factor(df_test$objective),factor(test.lasso.predict))


xgb.test = xgb.DMatrix(data=as.matrix(x_test),label=df_test$objective)
test.xgb_predict<- predict(xgb_model,xgb.test,reshape=TRUE)
test.xgb_predict<-ifelse(test.xgb_predict>=0.5,1,0)
test.xgb_matrix<- confusionMatrix(factor(df_test$objective),factor(test.xgb_predict))

test.tree_predict <- factor(predict(tree_model, newdata = df_test, type = "class"))
test.tree_matrix <- confusionMatrix(factor(df_test$objective),test.tree_predict)

test.nnet_predict <-predict(nnet_model, df_test)
test.nnet_matrix <- confusionMatrix(y_test,test.nnet_predict)
