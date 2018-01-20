#Milestone 3 - Support Vector Machines
# set work directory
rm(list=ls())
setwd ("/Users/Edith/INST737/")
#load data 
raw_data = read.csv("kick_all.csv")
nrow(raw_data)
##DATA CLEAN-UP/PROCESSING
# get multi class based on pledge ratio
raw_data$multi_class = cut(raw_data[, col_regression], c(-Inf, 0.8, 1, 1.2, Inf), labels=c('failed', 'close to launch', 'slight win', 'big win'))
# row without any NA
keep_complete_rows = complete.cases(raw_data)
sum(keep_complete_rows==TRUE)
# rows have unreasonable goal magnitude, too small(less than 100) or too large(more than 1 million)
rm_extreme_goal_rows = raw_data$goal_magnitude<2 |raw_data$goal_magnitude>6
sum(rm_extreme_goal_rows==TRUE)
# rows with outliers pledge ratio determined by boxplot stats
rm_extrem_ratio_rows = raw_data$pledge_ratio %in% boxplot.stats(raw_data$pledge_ratio)$out
sum(rm_extrem_ratio_rows==TRUE)
# number of rows considered as outlier(unreasonable goal magnitude and pledge ratio)
sum(rm_extrem_ratio_rows==TRUE|rm_extreme_goal_rows==TRUE)
# clean data whose rows are complete and not outliers
data = raw_data[keep_complete_rows & !rm_extreme_goal_rows & !rm_extrem_ratio_rows,]
data
# creating training and testing data sets
testidx <-which(1:nrow(data)%%4==0)
train_data <-data[-testidx,]
test_data <-data[testidx,] 

# load library
library(kernlab)
library(caret)
SVM - multiclass
## SVM with social economic features
#using vanilla kernel
classifier_socioeco<-ksvm(multi_class~Income+currency,data=data_train,kernel="vanilladot")
econ_predictions <- predict(classifier_socioeco,data_test)
table(econ_predictions,data_test$multi_class)
agreement_econ <- econ_predictions == data_test$multi_class
table(agreement_econ)
prop.table(table(agreement_econ))
#confusion matrix
confusionMatrix(econ_predictions, data_test$multi_class)
#using rbf
classifier_socioeco2<-ksvm(multi_class~Income+currency,data=data_train,kernel="rbfdot")
econ_predictions2 <- predict(classifier_socioeco2,data_test)
table(econ_predictions2,data_test$multi_class)
agreement_econ2 <- econ_predictions2 == data_test$multi_class
table(agreement_econ2)
prop.table(table(agreement_econ2))
#confusion matrix
confusionMatrix(econ_predictions2, data_test$multi_class)
## SVM with campaign properties 
#using vanilla kernel
classifier_campaign<-ksvm(multi_class~goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt,data=data_train,kernel="vanilladot")
campaign_predictions <- predict(classifier_campaign,data_test)
table(campaign_predictions,data_test$multi_class)
agreement_campaign <- campaign_predictions == data_test$multi_class
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(campaign_predictions, data_test$multi_class)
#using rbf kernel
classifier_campaign<-ksvm(multi_class~goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt,data=data_train,kernel="rbfdot")
campaign_predictions <- predict(classifier_campaign,data_test)
table(campaign_predictions,data_test$multi_class)
agreement_campaign <- campaign_predictions == data_test$multi_class
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(campaign_predictions, data_test$multi_class)
## SVM with social engagement features
#using vanilla kernel
classifier_social<-ksvm(multi_class~X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="vanilladot")
social_predictions <- predict(classifier_social,data_test)
table(social_predictions,data_test$multi_class)
agreement_campaign <- social_predictions == data_test$multi_class
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(social_predictions, data_test$multi_class)
#using rbf kernel
classifier_social<-ksvm(multi_class~X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="rbfdot")
social_predictions <- predict(classifier_social,data_test)
table(social_predictions,data_test$multi_class)
agreement_campaign <- social_predictions == data_test$multi_class
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(social_predictions, data_test$multi_class)
## SVM with ALL features
#using vanilla kernel
classifier_all<-ksvm(multi_class~Income+currency+goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="vanilladot")
all_predictions <- predict(classifier_all,data_test)
table(all_predictions,data_test$multi_class)
agreement_campaign <- all_predictions == data_test$multi_class
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(all_predictions, data_test$multi_class)
#using rbf kernel
classifier_all<-ksvm(multi_class~Income+currency+goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="rbfdot")
all_predictions <- predict(classifier_all,data_test)
table(all_predictions,data_test$multi_class)
agreement_campaign <- all_predictions == data_test$multi_class
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(all_predictions, data_test$multi_class)
