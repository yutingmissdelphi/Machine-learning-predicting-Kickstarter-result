# load libraries
library(gmodels);
require(ggplot2);
library(C50);
library(randomForest);
require(caret);

# set work directory
setwd('d:\\★★学习工作\\Life in Maryland\\INST 737 Intro to Data Science\\kick');

# load training and testing data, column of binary label is state
train_set = read.csv('data/kick_training_set.csv');
test_set = read.csv('data/kick_testing_set.csv');
n_ftr = ncol(train_set[-c(1,3,4)])


# fillna
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mean_or_mode = function(x) ifelse(class(x)=="numeric",mean(x, na.rm = TRUE), Mode(x))
fillna = lapply(train_set, mean_or_mode)
for(i in 7:ncol(train_set)){
  train_set[is.na(train_set[,i]), i] <- fillna[i]
}
for(i in 7:ncol(test_set)){
  test_set[is.na(test_set[,i]), i] <- fillna[i]
}


# ====== Wrappers ======
# wrap cross table parameters
ctbl = function(truth,	pred){
  CrossTable(truth,	pred, 
             prop.chisq	=	FALSE,	prop.c	=	FALSE,	
             prop.r	=	FALSE,	dnn	=	c('actual	state',	'predicted state'))	}

# boostrap with 10 trials
boot_wrap = function(train_set, test_set, n){
  print(paste0('training boot trials=',n))
  model = C5.0(train_set[-c(1,3,4)], train_set$state, trials=n);
  # train statitistic
  train_pred = predict(model, train_set[-c(1,3,4)])
  train_cnMat = confusionMatrix(train_pred, train_set$state, positive = "successful")
  train_pre_recall = confusionMatrix(train_pred, train_set$state, positive = "successful", mode = "prec_recall")
  # test statitistic
  pred = predict(model, test_set[-c(1,3,4)]);
  cnMat = confusionMatrix(pred, test_set$state, positive = "successful")
  pre_recall = confusionMatrix(pred, test_set$state, positive = "successful", mode = "prec_recall")
  return(list('model'=model, 'pred'=pred, 'conMat'=cnMat,'pre_recall' = pre_recall,
              train_cnMat=train_cnMat, train_pre_recall=train_pre_recall))
}


# bagging or random forest
bagging_wrap = function(n, train_set, test_set, bag_or_rf=1){
  print(paste0('training ', ifelse(bag_or_rf==1, 'bagging', 'random forest'), ', ntree=',n))
  print(dim(train_set))
  model = randomForest(state~.,data=train_set[-c(1,3)],mtry=n_ftr/bag_or_rf,importance=TRUE, ntree	=	n)
  # train statitistic
  train_pred = predict(model, train_set[-c(1,3,4)])
  train_cnMat = confusionMatrix(train_pred, train_set$state, positive = "successful")
  train_pre_recall = confusionMatrix(train_pred, train_set$state, positive = "successful", mode = "prec_recall")	
  # test statitistic
  pred = unlist(predict(model,newdata=test_set[-c(1,3)]))
  cnMat = confusionMatrix(unlist(pred), test_set$state, positive = "successful")
  pre_recall = confusionMatrix(pred, test_set$state, positive = "successful", mode = "prec_recall")
  return(list('model'=model, 'pred'=pred, 'conMat'=cnMat,'pre_recall' = pre_recall,
              train_cnMat=train_cnMat, train_pre_recall=train_pre_recall))
}

# ====== Train models ======
# decision Tree 
dt_model = C5.0(train_set[-c(1,3,4)], train_set$state);
dt_pred = predict(dt_model, test_set[-c(1,3,4)]);

# train and pred with boot， max is 100
boot_10 = boot_wrap(train_set,test_set,10)
boot_25 = boot_wrap(train_set,test_set,25)
boot_100 = boot_wrap(train_set,test_set,100)

# train and pred with bagging
bag_10 = bagging_wrap(10, train_set, test_set)
bag_25 = bagging_wrap(25, train_set, test_set)
bag_100 = bagging_wrap(100, train_set, test_set)
bag_250 = bagging_wrap(250, train_set, test_set)
bag_500 = bagging_wrap(500, train_set, test_set)

#random forest
rf_10 = bagging_wrap(10, train_set, test_set, 2)
rf_25 = bagging_wrap(25, train_set, test_set, 2)
rf_100 = bagging_wrap(100, train_set, test_set, 2)
rf_250 = bagging_wrap(250, train_set, test_set, 2)
rf_500 = bagging_wrap(500, train_set, test_set, 2)

# ====== view result ======
# dt
summary(dt_model)
ctbl(test_set$state,	dt_pred)	
# boot
boot_10$train_pre_recall
boot_10$pre_recall
boot_25$train_pre_recall
boot_25$pre_recall
boot_100$train_pre_recall
boot_100$pre_recall
#bagging
bag_10$train_pre_recall
bag_10$pre_recall
bag_25$train_pre_recall
bag_25$pre_recall
bag_100$train_pre_recall
bag_100$pre_recall
bag_250$train_pre_recall
bag_250$pre_recall
bag_500$train_pre_recall
bag_500$pre_recall
# random forest
rf_10$train_pre_recall
rf_10$pre_recall
rf_25$train_pre_recall
rf_25$pre_recall
rf_100$train_pre_recall
rf_100$pre_recall
rf_250$train_pre_recall
rf_250$pre_recall
rf_500$train_pre_recall
rf_500$pre_recall

# feature importance of random forest
varImpPlot(rf_100$model)
 
