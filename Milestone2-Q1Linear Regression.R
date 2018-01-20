rm(list=ls())

setwd ("/Users/Edith/INST737/")
train_set = read.csv("kick_training_set.csv",header=TRUE)
test_set = read.csv("kick_testing_set.csv",header=TRUE)


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


# remove outliers
train_set = subset(train_set, !train_set$goal %in% boxplot.stats(train_set$goal)$out)
train_set = subset(train_set, !train_set$pledge_ratio %in% boxplot.stats(train_set$pledge_ratio)$out)

test_set = subset(test_set, !test_set$goal %in% boxplot.stats(test_set$goal)$out)
test_set = subset(test_set, !test_set$pledge_ratio %in% boxplot.stats(test_set$pledge_ratio)$out)


##Test Socio-economic IVs
lm_eco = lm(pledge_ratio~Income+currency+category_parent, data=train_set)
summary(lm_eco)
pred_eco = predict (lm_eco, test_set)
cor.test(test_set$pledge_ratio, pred_eco)



##Test Rewards IVs
lm_rewards = lm(pledge_ratio~num_rewards+num_with_limits+money_max+money_min+money_median+
                money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt, 
                data=train_set)
summary(lm_rewards)
pred_rewards = predict(lm_rewards, test_set)
cor.test(test_set$pledge_ratio, pred_rewards)


##Test Social Engagement IVs
lm_engage = lm(pledge_ratio~X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+
             X.new_backer+X.websites+X.return_backer+X.upd+X.friend_facebook, data=train_set)
summary(lm_engage)
pred_engage = predict (lm_engage, test_set)
cor.test(test_set$pledge_ratio, pred_engage)


## Test All IVs
lm_all = lm(pledge_ratio~goal_magnitude+currency+category_parent+staff_pick+X.backer+
              X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+
              X.pledges+X.return_backer+X.upd+X.websites+X.friend_facebook+
              bio_auto_verified+num_rewards+num_with_limits+num_with_limits_pcnt+
              money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+
              money_median_pcnt+money_std_pcnt+Income,data=train_set)
summary(lm_all)
pred_all = predict(lm_all, test_set)
cor.test(test_set$pledge_ratio, pred_all)
mean((test_set$pledge_ratio - pred_all) ^ 2)



# getting the residuals of all 4 models
rs_all <- residuals(lm_all)
rs_engage <- residuals(lm_engage)
rs_eco <- residuals(lm_eco)
rs_rewards <- residuals(lm_rewards)


# first 4 plots
par(mfrow=c(2,2))
hist(rs_all)
qqnorm(rs_all);qqline(rs_all)
hist(rs_engage)
qqnorm(rs_engage);qqline(rs_engage)

# second 4 plots
par(mfrow=c(2,2))
hist(rs_eco)
qqnorm(rs_eco);qqline(rs_eco)
hist(rs_rewards)
qqnorm(rs_rewards);qqline(rs_rewards)






