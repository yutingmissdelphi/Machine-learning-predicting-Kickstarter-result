# set work directory

raw_data = read.csv('data/kick_all.csv')
nrow (raw_data)

# col indices
col_binary = c(4)
col_regression = c(3)
col_multi_class = c(34)
col_goal_magnitude = c(2)
cols_econ = c(5:6, 33)
cols_engage = c(9:11, 17, 19:21)
cols_reward = c(22:32)
cols_after_launch = c(7,8,12,13,15,16,18)
cols_full_ftrs =c(col_goal_magnitude, cols_econ, cols_engage, cols_reward, cols_after_launch)
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
multi.fun <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
multi.fun(raw_data$multi_class)
multi.fun(data$multi_class)
data_keep_outliers = raw_data[keep_complete_rows,]
# normalize numeric cols
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
data_keep_outliers_norm = normalize_dataframe(data_keep_outliers)
mydata=data[, c(col_multi_class,cols_full_ftrs)]
# ================================================
# model comparision with caret
# ================================================
library(caret)
# cv strategy
control_cv = trainControl(method="cv", number=10)
control_recv = trainControl(method="repeatedcv",	number=10,	repeats=3)	
control_boot = trainControl(method = "boot", number=10)
# train models
# M2: decision tree, boosting, bagging, random forest
# M3: SVM: two vanilladot, rbfdot; NNet
model_name = c("C5.0Tree", "rf", "treebag", "svmRadial", "svmLinear", "mlp")
models_cv = list()
for(i in 1:length(model_name)){
  set.seed(7)
  print(model_name[i])
  print('training cv')
  models_cv[[i]] = caret::train(multi_class ~ ., data=mydata, method=model_name[i], trControl = control_cv)
}
names(models_cv) = model_name
results_cv = resamples(models_cv)
summary(results_cv)
bwplot(results_cv)
dotplot(results_cv)
models_recv = list()
for(i in 1:length(model_name)){
  set.seed(7)
  print(model_name[i])
  print('training recv')
  models_recv[[i]] = caret::train(multi_class ~ ., data=mydata, method=model_name[i], trControl = control_recv)
}
names(models_recv)=model_name
results_recv = resamples(models_recv)
summary(results_recv)
bwplot(results_recv)
dotplot(results_recv)
models_boot = list()
for(i in 1:length(model_name)){
  set.seed(7)
  print(model_name[i])
  print('training boot')
  models_boot[[i]] = caret::train(multi_class ~ ., data=iris, method=model_name[i], trControl = control_boot)
}
names(models_boot)=model_name
results_boot = resamples(models_boot)
summary(results_boot)
bwplot(results_boot)
dotplot(results_boot)
