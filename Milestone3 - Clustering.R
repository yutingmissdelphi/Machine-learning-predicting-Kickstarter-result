# set work directory
setwd('your/directory);
# set work directory
setwd('your/directory);
# ================================================
# data preprocessing
# ================================================
# load data
raw_data = read.csv('data/kick_all.csv');
nrow(raw_data)
# col indices
col_binary = c(4)
col_regression = c(3)
col_multi_class = c(34)
col_goal_magnitude = c(2)
cols_econ = c(5:6, 33)
cols_engage = c(9:11, 17, 19:21)
cols_reward = c(22:32)
cols_after_launch = c(7,8,12,13,15,16,18)
cols_full_ftrs =c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)
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
# ================================================
# DBSCAN
# ================================================
library(dbscan)
set.seed(100)
# exclude outliners 
mat = as.matrix(data[, c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)])
kNNdistplot(mat, k=5)
abline(h=400, col='red', lty=2)
res = dbscan(mat, eps=400, minPts = 5)
res
data$dbscan_all_numeric= res$cluster
mat = as.matrix(data_norm[, c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)])
kNNdistplot(mat, k=5)
abline(h=.3, col='red', lty=2)
res = dbscan(mat, eps=.3, minPts = 5)
res
data_norm$dbscan_all_numeric= res$cluster
# get multiclass percentage for each cluster
cluster_with_labels = function(data, cluster_col, label_col){
  
  stat = multi.fun(data[cluster_col])
  clusters = rownames(stat)
  x = c()
  for (i in 1:length(clusters)){
    name = clusters[i]
    prev_name = rownames(x)
    sub_stat = multi.fun(data[data[cluster_col]==name,label_col])
    x = rbind(x, sub_stat[,2])
    rownames(x) = c(prev_name, name)
  }
  cbind(stat, x)
}
cluster_col = 'dbscan_all_numeric'
cluster_with_labels(data_norm, cluster_col, 'multi_class')
cluster_with_labels(data, cluster_col, 'multi_class')
#keep outliners
mat = as.matrix(data_keep_outliers[, c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)])
kNNdistplot(mat, k=5)
abline(h=800, col='red', lty=2)
res = dbscan(mat, eps=800, minPts = 5)
res
data_keep_outliers$dbscan_all_numeric= res$cluster
multi.fun(data_keep_outliers[rm_extrem_ratio_rows | rm_extreme_goal_rows,]$dbscan_all_numeric)
mat = as.matrix(data_keep_outliers_norm[, c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)])
kNNdistplot(mat, k=5)
abline(h=.3, col='red', lty=2)
res = dbscan(mat, eps=.3, minPts = 5)
res
data_keep_outliers_norm$dbscan_all_numeric= res$cluster
multi.fun(data_keep_outliers_norm[rm_extrem_ratio_rows | rm_extreme_goal_rows,]$dbscan_all_numeric)
# ================================================
# GMM
# ================================================
library(mclust)
set.seed(12345)
M = 1e3
reorder_data= data[, c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)]
gmm_with_sample <- Mclust(reorder_data, initialization=list(subset=sample(1:nrow(reorder_data), size=M)))
summary(gmm_with_sample)
plot(gmm_with_sample, what = c("BIC"))
data$gmm_cluster = gmm_with_sample$classification
reorder_data_norm= data_norm[, c(col_goal_magnitude, cols_econ[3], cols_engage, cols_reward, cols_after_launch)]
gmm_with_sample <- Mclust(reorder_data_norm, initialization=list(subset=sample(1:nrow(reorder_data_norm), size=M)))
summary(gmm_with_sample)
plot(gmm_with_sample, what = c("BIC"))
data_norm$gmm_cluster = gmm_with_sample$classification
cluster_col = 'gmm_cluster'
cluster_with_labels(data_norm, cluster_col, 'multi_class')
cluster_with_labels(data, cluster_col, 'multi_class')
# ================================================
# Kmeans
# ================================================
library(cluster)
set.seed(100)
sample_data = data_norm[sample(nrow(data), 10000),]
mydata=sample_data[, cols_full_ftrs]
# calculate distance matrix
dissE <- daisy(mydata)
# distance ^ 2
dissE2 = dissE^2
# store kmeans result
kms <- vector(mode="list")
# store kmeans statistics
sks = vector(mode='list')
sk2s = vector(mode='list')
sk_mean = c()
sk2_mean = c()
bss = c()
wss <- c()
# run kmeans with k from 2 to 25
for (i in 1:24){
  km <- kmeans(mydata,i+1)
  kms[[i]] = km
  sk = silhouette(km$cluster, dissE)
  sk2 = silhouette(km$cluster, dissE2)
  sks[[i]] = sk
  sk2s[[i]] = sk2
  sk_mean[i] = mean(sk[,3])
  sk2_mean[i] = mean(sk2[,3])
  bss[i] = km$betweenss
  wss[i] = km$tot.withinss
}
# plot stats
plot(2:25, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
plot(2:25, bss, type="b", xlab="Number of Clusters",
     ylab="between groups sum of squares")
plot(2:25, sk_mean, type="b", xlab="Number of Clusters",
     ylab="silhouette")
# best k=2, run multiple times
km_k2s = vector(mode="list")
sk_k2_mean = c()
for (i in 1:24){
  km <- kmeans(mydata,2)
  km_k2s[[i]] = km
  sk = silhouette(km$cluster, dissE)
  sk_k2_mean[i] = mean(sk[,3])
}
plot(1:24, sk_k2_mean, type="b", xlab="Number of Clusters",
     ylab="silhouette")
sk_k2_mean[7]
sample_data$kmeans = km_k2s[[7]]$cluster
cluster_col = 'kmeans'
cluster_with_labels(sample_data, cluster_col, 'multi_class')
cluster_with_labels(sample_data, 'multi_class', 'multi_class')
