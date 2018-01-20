## SVM with social economic features
#using vanilla kernel
classifier_socioeco <- ksvm(state~Income+currency,data=data_train,kernel="vanilladot")
econ_predictions <- predict(classifier_socioeco,data_test)
table(econ_predictions,data_test$state)
agreement_econ <- econ_predictions == data_test$state
table(agreement_econ)
prop.table(table(agreement_econ))

#confusion matrix
confusionMatrix(econ_predictions, data_test$state)

#using rbf
classifier_socioeco2<-ksvm(state~Income+currency,data=data_train,kernel="rbfdot")
econ_predictions2 <- predict(classifier_socioeco2,data_test)
table(econ_predictions2,data_test$state)
agreement_econ2 <- econ_predictions2 == data_test$state
table(agreement_econ2)
prop.table(table(agreement_econ2))

#confusion matrix
confusionMatrix(econ_predictions2, data_test$state)

## SVM with campaign properties 
#using vanilla kernel
classifier_campaign<-ksvm(state~goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt,data=data_train,kernel="vanilladot")
campaign_predictions <- predict(classifier_campaign,data_test)
table(campaign_predictions,data_test$state)
agreement_campaign <- campaign_predictions == data_test$state
table(agreement_campaign)
prop.table(table(agreement_campaign))

#confusion matrix
confusionMatrix(campaign_predictions, data_test$state)
#using rbf kernel
classifier_campaign<-ksvm(state~goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt,data=data_train,kernel="rbfdot")
campaign_predictions <- predict(classifier_campaign,data_test)
table(campaign_predictions,data_test$state)
agreement_campaign <- campaign_predictions == data_test$state
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(campaign_predictions, data_test$state)
## SVM with social engagement features
#using vanilla kernel
classifier_social<-ksvm(state~X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="vanilladot")
social_predictions <- predict(classifier_social,data_test)
table(social_predictions,data_test$state)
agreement_social <- social_predictions == data_test$state
table(agreement_social)
prop.table(table(agreement_social))
#confusion matrix
confusionMatrix(social_predictions, data_test$state)
#using rbf kernel
classifier_social<-ksvm(state~X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="rbfdot")
social_predictions <- predict(classifier_social,data_test)
table(social_predictions,data_test$state)
agreement_campaign <- social_predictions == data_test$state
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(social_predictions, data_test$state)

## SVM with ALL features
#using vanilla kernel
classifier_all<-ksvm(state~Income+currency+goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="vanilladot")
all_predictions <- predict(classifier_all,data_test)
table(all_predictions,data_test$state)
agreement_campaign <- all_predictions == data_test$state
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(all_predictions, data_test$state)
#using rbf kernel
classifier_all<-ksvm(state~Income+currency+goal_magnitude+staff_pick+category_parent+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,data= data_train, kernel="rbfdot")
all_predictions <- predict(classifier_all,data_test)
table(all_predictions,data_test$state)
agreement_campaign <- all_predictions == data_test$state
table(agreement_campaign)
prop.table(table(agreement_campaign))
#confusion matrix
confusionMatrix(all_predictions, data_test$state)
