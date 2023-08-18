library(FactoMineR)
library(factoextra)
library(corrplot)
library(psych)
library(tidyr)
library(dplyr)
library(ggplot2)

FF <- read.csv("~/Desktop/Assigments/2023_DM/file/fifa(1).csv")
FF_original <- read.csv("~/Desktop/Assigments/2023_DM/file/fifa(1).csv")
FF = as.data.frame(FF)

# clean the unnecessary column

for(i in 29:54){
  FF[,i] = as.numeric(substr(FF[,i], 1, nchar(FF[,i])-2)) #translate to float
  mean = mean(FF[complete.cases(FF[,i]),c(i)]) # set up average
  FF[is.na(FF[, i]), i] = round(mean, digits = 2) #change na to average
  # FF[is.na(FF[, i]), i] = NA #change "" to na 
}
for(i in c(12,13,89)){
  FF[,i] = as.numeric(substr(FF[,i], 2, nchar(FF[,i])-1)) #translate money to float
}
FF[,28] = as.numeric(substr(FF[,28], 1,3)) #translate money to float

# change feet to cm

FF$feet = sapply(strsplit(as.character(FF[,27]), split = "'"), function(x) x[1])
FF$inchs = sapply(strsplit(as.character(FF[,27]), split = "'"), function(x) x[2])
for(i in 1:nrow(FF)){
  FF[i,27] = as.numeric(FF$feet[i])*30.48 + as.numeric(FF$inchs[i])*2.54
}
FF[,27] = as.numeric(FF[,27])

# (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA)
FF_QG = as.data.frame(FF[,c(4,8,9,12:18,23,27:89)])
FF_W_name = as.data.frame(FF[,c(3,4,8,9,10,12:18,23,27:89)])
FF = FF[,c(4,8,9,12:14,16:18,23,27:89)]



# # error using regression model to predict missing value
# 
# FF_train = FF[complete.cases(FF$LS),]
# FF_na = FF[which(is.na(FF$LS), arr.ind = TRUE),]
# summary(FF_train)
# 
# nrow(FF_train)/nrow(FF) # 0.8854836
# colnames(FF_train[14])
# # do stepwise regression column by column 
# for(i in 14:39){
#   col = colnames(FF_train[14])
#   formula = paste(col," ~.")
#   formula = as.formula(formula) #create formula
#   mdl = lm(formula , data=FF_train) # set up formula
#   summary(mdl)
#   mdl.final = step(mdl) # stepwise equation
#   summary(mdl.final)
# }


# original method to dealing missing values 

FF_rm=FF[complete.cases(FF),] #missing values
FF_rm
check =nrow(FF) - nrow(FF_rm)
check

FF_W_name = FF_W_name[complete.cases(FF_W_name),]
# summary(FF)

boxplot(FF$Value)


# (BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB)

#check cor
cormat = cor(FF_rm)

# show out value > 0.5
cormat[cormat < 0.8  ] <- NA
cor_df = as.data.frame(cormat)

# combine related column
FF_rm[,13] = (FF_rm[,13]+FF_rm[,14]+FF_rm[,15]+FF_rm[,16]+FF_rm[,17]+FF_rm[,18]+FF_rm[,19]+FF_rm[,20]+FF_rm[,21]+FF_rm[,22]+FF_rm[,23]+FF_rm[,24]+FF_rm[,28])/13
FF_rm[,25] = (FF_rm[,25]+FF_rm[,26]+FF_rm[,27])/3
FF_rm[,29] = (FF_rm[,29]+FF_rm[,30]+FF_rm[,31]+FF_rm[,32]+FF_rm[,33]+FF_rm[,34]+FF_rm[,35]+FF_rm[,36]+FF_rm[,37]+FF_rm[,38])/10

FF_rm = FF_rm[,c(1:4,7:13,25,29,39:41,46,47,49)] # keep necessary columns

# keep the column which Uniqueness is below 0.8

FF_rm = FF_rm[,c(-4,-7,-9)] # keep necessary columns

colnames(FF_rm)[9] =  "LS~LM,RM"
colnames(FF_rm)[10] = "LCM~RCM"
colnames(FF_rm)[11] = "LWB~RB"

# (DDDDDDDDDDDDDDDDDDDDDDD & EEEEEEEEEEEEEEEEEEEEEEEE)

f = factanal(FF_rm,factors=6, rotation="promax") 
print(f,cutoff=.5)

test = FF_rm
factor_scores <- factor.scores(test, f,  method = "regression")

check = as.data.frame(factor_scores$scores)

FF_O_rm = FF_W_name[complete.cases(FF_W_name),]
check$name = FF_O_rm$Name
colnames(check) <- c("skill", "value", "body","index_LS_LCM", "index_ LWB", "age", "name")

check = check[,-6]

check$overall = check$skill + check$value +check$index_LS_LCM + check$`index_ LWB`


# ---- final ----
# ward
test = FF_rm
d = dist(as.matrix(test))

hc <- hclust(d, method="centroid")  
plot(hc, hang = -1)
ac=rep(0,10)
for (i in 1:10) {
  re=rect.hclust(hc,k = (i+1))
  ac[i]=agglomeration_coefficients(re,test)
}
plot(ac)

test$cluster_centroid = cutree(hc, 4)
test_a_centroid = aggregate(test,by=list(test$cluster_centroid),FUN=mean)



# k-means
k_m = FF_rm
set.seed(100)
fviz_nbclust(k_m, kmeans, method = "wss")
km=kmeans(k_m,5)
fviz_cluster(km,k_m)


thad_a_kmeans = aggregate(k_m,by=list(km$cluster),FUN=mean)
k_m$cluster = km$cluster
k_m$cluster = as.factor(k_m$cluster)
k_m$club = FF_W_name$Club

total_clubs = unique(k_m$club)


clubs <- data.frame(matrix(ncol = 6, nrow = 0))
for (i in total_clubs){
  
  t = table(k_m[k_m$club == i,17])
  newdata = c(i,as.numeric(t[1]),as.numeric(t[2]),as.numeric(t[3]),as.numeric(t[4]),as.numeric(t[5]))
  clubs <- rbind(clubs, newdata) 
}
colnames(clubs) <- c("clubs", "1", "2", "3", "4", "5")

scale_kmeans = as.data.frame(scale(thad_a_kmeans[,-c(1,9)]))
scale_kmeans$cluster = thad_a_kmeans$Group.1

df_long <- scale_kmeans %>%
  pivot_longer(
    cols = -cluster,
    names_to = "variable",
    values_to = "value"
  )





ggplot(data = df_long, aes(x = variable, y = value, color = factor(cluster), group = variable)) +
  geom_line() +
  labs(x = "Variable", y = "Value") +
  theme_bw()


colnames(thad_a_kmeans)



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

# proportion of variance explained
var_exp = function(f_correlation){
  variance_explained <- apply(f_correlation, 2, function(x) sum(x^2)) # sum every value^2 in a row
  print(variance_explained / sum(variance_explained))
}


