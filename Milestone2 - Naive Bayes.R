## Naive Bayes
# categorize all the variables 
install.packages('arules')
library(arules)

discrete_dataframe = function(df, num_intervals){
  for(i in 1:33){
    if(class(df[,i])!='factor'){
      col_i = df[,i]
      if (length(unique(col_i[!is.na(col_i)]))==1){
        df[,i] = factor(df[,i])
      }else{
        df[,i] = discretize(df[,i], categories = num_intervals)
      }
    }
  }
  df
}

test_d2 = discrete_dataframe(test_set, 100)
train_d2 = discrete_dataframe(train_set, 100)

### Libary for NB
library(e1071)

##Train Naive Bayes model based on all the features

NB = naiveBayes(state~.,data= train_d2)

# Predict on the test dataset 
NB_pred=predict(NB,test_d2)
### Library for crosstab testing the result
library(gmodels)
#generate confusion matrix to compared the result accuracy
CrossTable(test_d2$state, NB_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

## Adding laplace estimator 
NB = naiveBayes(train_d2, train_d2$state, laplace = 1)
NB_pred = predict(NB, test_d2)

#generate confusion matrix to compared the result accuracy
CrossTable(test_d2$state, NB_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

## NB for socio economics 
NB1 = naiveBayes(state~Income+factor(currency)+category_parent,data= train_d2)
NB1_pred=predict(NB1,test_d2)
table(NB1_pred, test_d2$state)
CrossTable(test_d2$state, NB1_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

NB1 = naiveBayes(state~Income+factor(currency)+category_parent,data= train_d2,laplace = 1)

NB1 = naiveBayes(train_d2[,c(5,6,33)], train_d2$state, laplace = 1)
NB1_pred = predict(NB1, test_d2)
CrossTable(test_d2$state, NB1_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 
## NB for campaign properties
NB2 = naiveBayes(state~goal_magnitude+factor(category_parent)+factor(staff_pick)+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt,data= train_d2)
NB2_pred=predict(NB2,test_d2)
CrossTable(test_d2$state, NB2_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 
NB2 = naiveBayes(train_d2[,c('goal_magnitude','category_parent','staff_pick','num_rewards','num_with_limits','num_with_limits_pcnt','money_max','money_min','money_median','money_std','money_max_pcnt','money_min_pcnt','money_median_pcnt','money_std_pcnt')], train_d2$state, laplace = 1)
NB2_pred = predict(NB2, test_d2,laplace = 1)
CrossTable(test_d2$state, NB2_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

train_d2[, c('X.bio_backed','X.bio_created')]

## NB for social engagement
NB3 = naiveBayes(state~X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= train_d2)
NB3_pred=predict(NB3,test_d2)
CrossTable(test_d2$state, NB3_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

NB3 = naiveBayes(train_d2[,c('X.backer','X.bio_backed','X.bio_created','X.collaborators','X.comments','X.new_backer','X.websites','X.return_backer','X.friend_facebook','X.upd')], train_d2$state, laplace = 1)
NB3_pred = predict(NB3, test_d2)
CrossTable(test_d2$state, NB3_pred, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

#generate confusion matrix to compared the result accuracy
CrossTable(test_d2$state, NB_pred2, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

convert_counts=function(x){
  x<- ifelse(x>0,1,0)
  x=factor(x,levels=c(0,1),labels=c("No","Yes"))
  }

sms_train= apply(sms_train,MARGIN=2,convert_counts)
sms_test= apply(sms_test,MARGIN=2,convert_counts)

sms_classifier=naiveBayes(sms_train,sms_raw_train$type)


