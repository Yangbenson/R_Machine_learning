library(cluster) # needed for clusplot
library(fpc)  # needed for plotcluster
library(dplyr)
library(factoextra)
library(rpart)
library(corrplot)


data <- read.csv("~/Desktop/Assigments/2023_DM/HW/thads2013n.txt", stringsAsFactors=TRUE)
data = as.data.frame(data)
summary(data)

# check which column has a negative minimum value
for(col in names(data)) {
  if(is.numeric(data[[col]]) && min(data[[col]], na.rm = TRUE) < 0) {
    print(paste0("Column ", col, " has a negative minimum value."))
  }
}

# ---- handling the missing values----

test = data[,-c(1,3,4,13,16,17,23,26,34,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,75:99)]

for (i in names(test)) {
  if (is.numeric(test[[i]])) {
    test[[i]][test[[i]] < 0] <- NaN
  }
}


test_r_m=test[complete.cases(test),] 
test_r_m
check =nrow(test) - nrow(test_r_m)
check
# 29250 missing values
summary(test_r_m)

# ----translate factor to numeric (not use)----
# for (i in names(test_r_m)) {
#   if (is.factor(test_r_m[[i]])) {
#     test_r_m[[i]] <- as.numeric(substr(as.character(test_r_m[[i]]), 2, 2))
#   }
# }


# # outliner
# test_r_m = test_r_m[test_r_m$VALUE>1,]
# test_r_m = test_r_m[test_r_m$VALUE<2500000,]
# test_r_m = test_r_m[test_r_m$AGE1>1,]
# test_r_m = test_r_m[test_r_m$IPOV>1,]
# # boxplot(test_r_m$TYPE)
# # boxplot(test_r_m$NUNITS)
# # boxplot(test_r_m$WEIGHT)
# test_r_m = test_r_m[test_r_m$WEIGHT<30000,]
# # boxplot(test_r_m$PER)
# test_r_m = test_r_m[test_r_m$PER<20&&test_r_m$PER>1,]
# # boxplot(test_r_m$ZINC2)

# ---- pick out the column----
ncol(test_r_m)

# cormat = cor(test_r_m)
# cormat[cormat < 0.2& cormat > -0.2  ] <- NA
# cor_m = as.data.frame(cormat)

# not use (check correlation)
# cluster_c = (colnames(cor_m[,cor_m['UTILITY'] > 0.3])) 
# cluster_col = c(cluster_c)
# cluster_data = test_r_m[,c("FMR", "L30", "L50","L80","BEDRMS","ROOMS","PER","ZSMHC"
#                         ,"UTILITY","COST06","COST12","COST08","COSTMED","GL30","GL50"
#                         ,"GL80","APLMED","ABL30","ABL50","ABL80","ABLMED","COST06RELAMICAT"
#                         ,"COSTMedRELAMICAT","FMTBEDRMS","FMTCOST06RELAMICAT","FMTCOSTMEDRELAMICAT")]
# cor(cluster_data)


# ---- cluster ----

# d = dist(as.matrix(test_r_m[1:10000,]))
d = dist(as.matrix(test_r_m))
thad = test_r_m

# ---- centroid ----

# ---- test ----
#计算聚合系数的函数，x为分类结果，data为分类数据

agglomeration_coefficients=function(x,data){
  ac=0
  n=length(x)
  for (i in 1:n) {
    pac=0
    m=length(x[[i]])
    x0=data[x[[i]],]
    x0_mean=colMeans(x0)
    for (j in 1:m) {
      pac=pac+sum((x0[j,]-x0_mean)^2)}
    ac=ac+pac}
  return(ac)}

d = dist(as.matrix(test_r_m))
hc <- hclust(d, method="centroid")  
plot(hc, hang = -1)
ac=rep(0,10)
for (i in 1:10) {
  re=rect.hclust(hc,k = (i+1))
  ac[i]=agglomeration_coefficients(re,test_r_m)
}
plot(ac)

# -----------
# hc <- hclust(d, method="centroid")  
plot (hc, main="centroid")

thad$cluster_centroid = cutree(hc, 9)
thad_a_centroid = aggregate(thad,by=list(thad$cluster_centroid),FUN=mean)


# ---- Ward Method ----
hc2<- hclust(d, method="ward.D") 
plot(hc2, main="Ward Method Dendrogram") # display dendogram
plot(hc2, hang = -1)
ac2=rep(0,10)
for (i in 1:10) {
  re=rect.hclust(hc2,k = (i+1))
  ac2[i]=agglomeration_coefficients(re,test_r_m)
}
plot(ac2)

thad$cluster_Ward = cutree(hc2, 7)
thad_a_Ward = aggregate(thad,by=list(thad$cluster_Ward),FUN=mean)

# ----k means----
k_m = test_r_m[1:10000,]

set.seed(100)
fviz_nbclust(k_m, kmeans, method = "wss")
km=kmeans(k_m,4)
fviz_cluster(km,k_m)

thad_a_kmeans = aggregate(k_m,by=list(km$cluster),FUN=mean)
k_m$cluster = km$cluster
k_m$cluster = as.factor(k_m$cluster)
# get cluster means Centroid Plot against 1st 2 discriminant functions
# plotcluster(k_m, k_m$k_cluster, main="Plot Clusters")
# 
# clusplot(test_r_m, hc3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="KMeans Cluster Cars")
c1 = k_m[k_m$cluster == "1",]
c2 = k_m[k_m$cluster == "2",]
c3 = k_m[k_m$cluster == "3",]
c4 = k_m[k_m$cluster == "4",]

cor=cor(k_m)#取相关系数绝对值最大的三个
corrplot(cor)#相关的可视化
rank(abs(cor[,length(cor[1,])]))


model_zinc <- aov(ZINC2 ~ cluster, data = k_m)
summary(model_zinc)
model_UTILITY <- aov(UTILITY ~ cluster, data = k_m)
summary(model_UTILITY)
model_TOTSAL <- aov(TOTSAL ~ cluster, data = k_m)
summary(model_TOTSAL)

TukeyHSD(model_zinc)
TukeyHSD(model_UTILITY)
TukeyHSD(model_TOTSAL)

