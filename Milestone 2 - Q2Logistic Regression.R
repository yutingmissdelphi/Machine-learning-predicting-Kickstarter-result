##Milestone 2
kick_training_set <- read.csv("~/Desktop/kick_training_set.csv")
#get rid of outliner
d = kick_training_set
d = subset(d, !d$goal %in% boxplot.stats(d$goal)$out)
d = subset(d, !d$pledge_ratio %in% boxplot.stats(d$pledge_ratio)$out)
d = subset(d,!d$Income=="NA")
## all campaigns
income.lm = lm(d$pledge_ratio~d$Income)
summary(income.lm)

qqnorm(resid(income.lm))
qqline(resid(income.lm))
test =data.frame(RunSize!=!seq(55,345,by=10))!
pc = predict(income.lm, int = "c", newdata = kick_testing_set)
pp = predict(income.lm, int = "p", newdata = kick_testing_set)
plot(d$Income, d$pledge_ratio)
matlines(kick_testing_set, pc)
matlines(kick_testing_set, pp)


## success
d_success = subset(d, d$pledge_ratio>1)
income.lm_success = lm(d_success~d$Income)
summary(income.lm_success)
qqnorm(resid(income.lm_success))
qqline(resid(income.lm_success))



## fail
d_fail = subset(d, d$pledge_ratio>1)
income.lm_fail = lm(d_fail~d$Income)
summary(income.lm_fail)
qqnorm(resid(income.lm_fail))
qqline(resid(income.lm_fail))


###############Logistic
library(aod)
library(ggplot2)

mydata = kick_training_set

##get rid of outliers
mydata = subset(mydata, !mydata$goal %in% boxplot.stats(mydata$goal)$out)
mydata = subset(mydata, !mydata$pledge_ratio %in% boxplot.stats(mydata$pledge_ratio)$out)

attach(mydata)
##logistic regression model
mydata$staff_pick <- factor(mydata$staff_pick)
mydata$result <-recode(mydata$state,"'successful'=1;'failed'=0")
model =  glm(mydata$result~mydata$X.bio_backed+mydata$X.bio_created+mydata$X.comments+mydata$Income+mydata$money_max_pcnt+mydata$num_rewards+mydata$num_with_limits+factor(bio_auto_verified)+mydata$num_rewards+mydata$num_with_limits+mydata$money_max+mydata$money_min+factor(mydata$category_parent)+factor(currency),family=binomial(),data=mydata)
summary(model)
exp(coef(model))

### Logistic regression 1 - Socio-economic features
glm_eco = glm(mydata$state~Income+factor(currency)+category_parent,family=binomial(),data=mydata)
summary(glm_eco)
exp(coef(glm_eco))

#predict
kick_testing_set$resultP<- predict(glm_eco,newdata=kick_testing_set,type="response")
#recode to a new variable with predicted lable
kick_testing_set$result_1 = recode(kick_testing_set$resultP,"0:0.5='failed'; 0.5:1='successful'")

#plot
ggplot(kick_testing_set, aes(x = kick_testing_set$Income, y=kick_testing_set$resultP))+geom_line(aes(color = kick_testing_set$currency),size = 1)

# create a crosstab for true lable and predicted lable to get the accuracy
CrossTable(kick_testing_set$state, kick_testing_set$result_1)
# simplify the table
CrossTable(kick_testing_set$state, kick_testing_set$result_1, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

### Logistic regression 2 - Kickstarter campaign property features
glm_prop = glm(mydata$state~goal_magnitude+factor(category_parent)+factor(staff_pick)+num_rewards+num_with_limits+num_with_limits_pcnt+money_max+money_min+money_median+money_std+money_max_pcnt+money_min_pcnt+money_median_pcnt+money_std_pcnt,family=binomial(),data=mydata)
summary(glm_prop)
exp(coef(glm_prop))
#predict on the testing dataset
kick_testing_set$resultP2<- predict(glm_prop,newdata=kick_testing_set,type="response")
kick_testing_set$result_2 = recode(kick_testing_set$resultP2,"0:0.5='failed'; 0.5:1='successful'")
# create a crosstab for true lable and predicted lable to get the accuracy
CrossTable(kick_testing_set$state, kick_testing_set$result_2)
# simplify the table
CrossTable(kick_testing_set$state, kick_testing_set$result_2, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 
#plot
ggplot(kick_testing_set, aes(x = kick_testing_set$goal_magnitude, y=kick_testing_set$resultP2))+geom_line(aes(color = kick_testing_set$num_rewards),size = 1)

#got rid off the outliers of number of reward
testdata = subset(kick_testing_set, !kick_testing_set$num_rewards %in% boxplot.stats(kick_testing_set$num_rewards)$out)

### Logistic regression 3 - Social Engagement features
mydata$has_facebook = factor(mydata$has_facebook)
glm_social = glm(mydata$state~X.backer+X.bio_backed+X.bio_created+X.collaborators+X.comments+X.new_backer+X.websites+X.return_backer+X.friend_facebook+X.upd,family=binomial(),data=mydata)
summary(glm_social)
exp(coef(glm_social))
#predict
kick_testing_set$resultP3<- predict(glm_social,newdata=kick_testing_set,type="response")
ggplot(kick_testing_set, aes(x = kick_testing_set$X.upd, y=kick_testing_set$resultP3))+geom_line(aes(color = kick_testing_set$X.new_backer),size = 1)
# recode the probability result to the lable of  successful or failed 
kick_testing_set$result_3 = recode(kick_testing_set$resultP3,"0:0.5='failed'; 0.5:1='successful'")
# create a crosstab for true lable and predicted lable to get the accuracy
CrossTable(kick_testing_set$state, kick_testing_set$result_3)
# simplify the table
CrossTable(kick_testing_set$state, kick_testing_set$result_3, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 

### Logistic regression 4 - All features
glm_all = glm(state~.,family=binomial(),data=train_set)
summary(glm_all)
exp(coef(glm_social))
test_set$resultPALL<- predict(glm_all,newdata=test_set,type="response")
test_set$resultPA = sapply(test_set$resultPALL, function(x) ifelse(x>0.5, "successful", "failed"))
CrossTable(test_set$state, test_set$resultPALL, 
           prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('actual state', 'predicted state')) 
