# Milestone 3 Neural Network
## Normalize dataframe
normalize_dataframe = function(df){
  for (i in 1:ncol(df)){
    if(class(df[,i])!='factor'){
      col_i = df[,i]
      if (length(unique(col_i[!is.na(col_i)]))!=1){
        df[,i] = (df[,i] - min(df[,i])) / (max(df[,i] - min(df[,i])))
      }
    }
  }
  df
}
data_norm = normalize_dataframe(data)
## Splitting data into train and test dataset
testidx<-which(1:nrow(data_norm)%%4==0)
data_train <- data_norm[-testidx,]
data_test <- data_norm[testidx,]
library(neuralnet)
## create a matrix where all the qualitative variables were to dummy variables
m <- model.matrix( ~ goal_magnitude + pledge_ratio +state + currency +category_parent+staff_pick+X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.pledges+X.return_backer+X.upd+
                     X.websites+is_small_community+has_facebook+X.friend_facebook+bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+Income+multi_class, data = data_train)
##converting the test data to matrix with dummy variables
m_test <- model.matrix( ~ goal_magnitude + pledge_ratio +state + currency +category_parent+staff_pick+X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.pledges+X.return_backer+X.upd+
                          X.websites+is_small_community+has_facebook+X.friend_facebook+bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+Income+multi_class, data = data_test)
### SOCIAL ECONOMIC FACTORS
kick_model_SE<-neuralnet(formula=pledge_ratio~currencyCAD+currencyCHF+currencyDKK+currencyEUR+currencyGBP+currencyHKD+currencyMXN+currencyNOK+currencyNZD+currencySEK+currencySGD+currencyUSD+Income,m)
model_results_SE <- compute(kick_model_SE,m_test[,c('currencyCAD','currencyCHF','currencyDKK','currencyEUR','currencyGBP','currencyHKD','currencyMXN','currencyNOK','currencyNZD','currencySEK','currencySGD','currencyUSD','Income')])

predicted_pledgeratio_SE = model_results_SE$net.result
#examine the correlation between predicted and actual values
cor(predicted_pledgeratio_SE,data_test$pledge_ratio)
#0.02851458997
# increase the hidden layers
kick_model_SE<-neuralnet(formula=pledge_ratio~currencyCAD+currencyCHF+currencyDKK+currencyEUR+currencyGBP+currencyHKD+currencyMXN+currencyNOK+currencyNZD+currencySEK+currencySGD+currencyUSD+Income,m,hidden = 5)
model_results_SE <- compute(kick_model_SE,m_test[,c('currencyCAD','currencyCHF','currencyDKK','currencyEUR','currencyGBP','currencyHKD','currencyMXN','currencyNOK','currencyNZD','currencySEK','currencySGD','currencyUSD','Income')])
predicted_pledgeratio_SE = model_results_SE$net.result
#examine the correlation between predicted and actual values
cor(predicted_pledgeratio_SE,data_test$pledge_ratio)
# 0.03467985318
### CAMPAIGN PROPERTIES
kick_model_CP<-neuralnet(formula=pledge_ratio~goal_magnitude+staff_pick+category_parentcomics+ category_parentcrafts+ category_parentdance+category_parentdesign+category_parentfashion+category_parentgames+category_parentjournalism+category_parentmusic+category_parentphotography+category_parentpublishing+category_parenttechnology+category_parenttheater+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt, m, hidden = 4)
model_results_CP <- compute(kick_model_CP,m_test[,c('goal_magnitude','staff_pick','category_parentcomics','category_parentcrafts','category_parentdance','category_parentdesign','category_parentfashion','category_parentgames','category_parentjournalism','category_parentmusic','category_parentphotography','category_parentpublishing','category_parenttechnology','category_parenttheater','num_rewards','num_with_limits','num_with_limits_pcnt','money_max','money_min','money_median','money_std','money_max_pcnt','money_min_pcnt','money_median_pcnt','money_std_pcnt')])
predicted_pledgeratio_CP = model_results_CP$net.result
cor(predicted_pledgeratio_CP,data_test$pledge_ratio)
##hidden layer = 5
# overfit [1,] 0.4799280614
### SOCIAL ENGAGEMENT
kick_model_SEN<-neuralnet(formula=pledge_ratio~X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd, m, hidden = 1, stepmax = 1E7)
model_results_SEN <- compute(kick_model_SEN,m_test[,c('X.bio_backed','X.bio_created','X.collaborators','X.comments','X.new_backer','X.websites','X.return_backer','X.friend_facebook','X.upd')])
predicted_pledgeratio_SEN = model_results_SEN$net.result
cor(predicted_pledgeratio_SEN,data_test$pledge_ratio)
#hidden = 1
0.4782546143
#hidden = 2
0.507116449
#3
0.5092694124
### Creating a model with all the feasures available
kick_model<-neuralnet(formula=pledge_ratio~goal_magnitude+currencyCAD+currencyCHF+currencyDKK+currencyEUR+currencyGBP+currencyHKD+currencyMXN+currencyNOK+currencyNZD+currencySEK+currencySGD+currencyUSD+staff_pick+X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.pledges+X.return_backer+X.upd+X.websites+is_small_community+has_facebook+X.friend_facebook+bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+Income+category_parentcomics+ category_parentcrafts+ category_parentdance+category_parentdesign+ category_parentfashion+category_parentgames+category_parentjournalism+category_parentmusic+category_parentphotography+category_parentpublishing+category_parenttechnology+category_parenttheater, m, stepmax = 1E8)
plot(kick_model)
### Predicting on Test data 
model_results <- compute(kick_model,m_test[,c('goal_magnitude','currencyCAD','currencyCHF','currencyDKK','currencyEUR','currencyGBP','currencyHKD','currencyMXN','currencyNOK','currencyNZD','currencySEK','currencySGD','currencyUSD','staff_pick','X.backer','X.bio_backed','X.bio_created','X.collaborators','X.comments','X.new_backer','X.pledges','X.return_backer','X.upd','X.websites','is_small_community','has_facebook','X.friend_facebook','bio_auto_verified','num_rewards','num_with_limits','num_with_limits_pcnt','money_max','money_min','money_median','money_std','money_max_pcnt','money_min_pcnt','money_median_pcnt','money_std_pcnt','Income','category_parentcomics','category_parentcrafts', 'category_parentdance','category_parentdesign','category_parentfashion', 'category_parentgames','category_parentjournalism', 'category_parentmusic','category_parentphotography','category_parentpublishing','category_parenttechnology','category_parenttheater')])
predicted_pledgeratio = model_results$net.result
#examine the correlation between predicted and actual values
cor(predicted_pledgeratio,data_test$pledge_ratio)
#yielding accuracy of 0.7206459678
###Step 2 Improving Model performance
##hidden = 1
kick_model_hidden1<-neuralnet(formula=pledge_ratio~goal_magnitude+currencyCAD+currencyCHF+currencyDKK+currencyEUR+currencyGBP+currencyHKD+currencyMXN+currencyNOK+currencyNZD+currencySEK+currencySGD+currencyUSD+staff_pick+X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.pledges+X.return_backer+X.upd+X.websites+is_small_community+has_facebook+X.friend_facebook+bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+Income+category_parentcomics+ category_parentcrafts+ category_parentdance+category_parentdesign+ category_parentfashion+category_parentgames+category_parentjournalism+category_parentmusic+category_parentphotography+category_parentpublishing+category_parenttechnology+category_parenttheater, m, hidden = 1)
model_results_hidden1 <- compute(kick_model_hidden1,m_test[,c('goal_magnitude','currencyCAD','currencyCHF','currencyDKK','currencyEUR','currencyGBP','currencyHKD','currencyMXN','currencyNOK','currencyNZD','currencySEK','currencySGD','currencyUSD','staff_pick','X.backer','X.bio_backed','X.bio_created','X.collaborators','X.comments','X.new_backer','X.pledges','X.return_backer','X.upd','X.websites','is_small_community','has_facebook','X.friend_facebook','bio_auto_verified','num_rewards','num_with_limits','num_with_limits_pcnt','money_max','money_min','money_median','money_std','money_max_pcnt','money_min_pcnt','money_median_pcnt','money_std_pcnt','Income','category_parentcomics','category_parentcrafts', 'category_parentdance','category_parentdesign','category_parentfashion', 'category_parentgames','category_parentjournalism', 'category_parentmusic','category_parentphotography','category_parentpublishing','category_parenttechnology','category_parenttheater')])
predicted_pledgeratio_hidden1 = model_results_hidden1$net.result
#examine the correlation between predicted and actual values
cor(predicted_pledgeratio_hidden1,data_test$pledge_ratio)
# 0.7209561323
##hidden = 3
kick_model_hidden3<-neuralnet(formula=pledge_ratio~goal_magnitude+currencyCAD+currencyCHF+currencyDKK+currencyEUR+currencyGBP+currencyHKD+currencyMXN+currencyNOK+currencyNZD+currencySEK+currencySGD+currencyUSD+staff_pick+X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.pledges+X.return_backer+X.upd+X.websites+is_small_community+has_facebook+X.friend_facebook+bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+Income+category_parentcomics+ category_parentcrafts+ category_parentdance+category_parentdesign+ category_parentfashion+category_parentgames+category_parentjournalism+category_parentmusic+category_parentphotography+category_parentpublishing+category_parenttechnology+category_parenttheater, m, hidden = 3)
model_results_hidden3 <- compute(kick_model_hidden3,m_test[,c('goal_magnitude','currencyCAD','currencyCHF','currencyDKK','currencyEUR','currencyGBP','currencyHKD','currencyMXN','currencyNOK','currencyNZD','currencySEK','currencySGD','currencyUSD','staff_pick','X.backer','X.bio_backed','X.bio_created','X.collaborators','X.comments','X.new_backer','X.pledges','X.return_backer','X.upd','X.websites','is_small_community','has_facebook','X.friend_facebook','bio_auto_verified','num_rewards','num_with_limits','num_with_limits_pcnt','money_max','money_min','money_median','money_std','money_max_pcnt','money_min_pcnt','money_median_pcnt','money_std_pcnt','Income','category_parentcomics','category_parentcrafts', 'category_parentdance','category_parentdesign','category_parentfashion', 'category_parentgames','category_parentjournalism', 'category_parentmusic','category_parentphotography','category_parentpublishing','category_parenttechnology','category_parenttheater')])
predicted_pledgeratio_hidden3 = model_results_hidden3$net.result
#examine the correlation between predicted and actual values
cor(predicted_pledgeratio_hidden3,data_test$pledge_ratio)
#
##hidden = 5
kick_model_hidden5<-neuralnet(formula=pledge_ratio~goal_magnitude+currencyCAD+currencyCHF+currencyDKK+currencyEUR+currencyGBP+currencyHKD+currencyMXN+currencyNOK+currencyNZD+currencySEK+currencySGD+currencyUSD+staff_pick+X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.pledges+X.return_backer+X.upd+X.websites+is_small_community+has_facebook+X.friend_facebook+bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt+Income+category_parentcomics+ category_parentcrafts+ category_parentdance+category_parentdesign+ category_parentfashion+category_parentgames+category_parentjournalism+category_parentmusic+category_parentphotography+category_parentpublishing+category_parenttechnology+category_parenttheater, m, hidden = 5)
model_results_hidden5 <- compute(kick_model_hidden5,m_test[,c('goal_magnitude','currencyCAD','currencyCHF','currencyDKK','currencyEUR','currencyGBP','currencyHKD','currencyMXN','currencyNOK','currencyNZD','currencySEK','currencySGD','currencyUSD','staff_pick','X.backer','X.bio_backed','X.bio_created','X.collaborators','X.comments','X.new_backer','X.pledges','X.return_backer','X.upd','X.websites','is_small_community','has_facebook','X.friend_facebook','bio_auto_verified','num_rewards','num_with_limits','num_with_limits_pcnt','money_max','money_min','money_median','money_std','money_max_pcnt','money_min_pcnt','money_median_pcnt','money_std_pcnt','Income','category_parentcomics','category_parentcrafts', 'category_parentdance','category_parentdesign','category_parentfashion', 'category_parentgames','category_parentjournalism', 'category_parentmusic','category_parentphotography','category_parentpublishing','category_parenttechnology','category_parenttheater')])
predicted_pledgeratio_hidden5 = model_results_hidden5$net.result
#examine the correlation between predicted and actual values
cor(predicted_pledgeratio_hidden5,data_test$pledge_ratio)
#
