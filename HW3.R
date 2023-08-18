library(FactoMineR)
library(factoextra)
library(corrplot)
library(stats)
library(psych)
source("http://blogs.5eanalytics.com/RCode/PCA_functions.R")
source()

# Q1
#b -----------------------------------------------------------------------
D1 <- read_table("~/Desktop/Assigments/2023_DM/file/Domotic1.txt")
D1 = as.data.frame(D1)
summary(D1)
boxplot(D1[,3:24])
boxplot(D1[,15:17])
boxplot(D1[,c(3:18,22,23,24)])

# pca_result = PCA(D1[,3:14,18,22,23,24], scale.unit=T, graph=T, ncp=16)
pca_result = PCA(D1[,c(3:18,22,23,24)], scale.unit=T, graph=T, ncp=18) 

display_pc(pca_result,cutoff = .3)
summary(pca_result)

fviz_screeplot(pca_result, ncp=18)
communality(pca_result)



#c -----------------------------------------------------------------------
loadings.pcarot= varimax(pca_result$var$coord)$loadings
pca_result$var$coord = loadings.pcarot
plot(pca_result, choix  ="var")
display_pc(pca_result,cutoff = 0.3)


pca_loading = round(pca_result$var$coord[,1:5],digits = 3) # show 5 dimensions 
# pca_result$var

D1_PC = as.matrix(D1[,c(3:18,22,23,24)]) %*% as.matrix(pca_result$var$coord[,1:5]) #new dataset
colnames(D1_PC) = c("Light&ME", "Temperature","Humedad", "CO2", "Precipitacion ")
D1_PC


D1_reduced =cbind(D1[,c(3:18,22,23,24)], D1_PC)
round(cor(D1_reduced, use="pairwise.complete.obs"),digit = 3)




#-------------------------------------------------------------------------------

# Q2
DC <- read.csv("~/Desktop/Assigments/2023_DM/file/drug_consumption.csv", stringsAsFactors=TRUE)
# a -----------------------------------------------------------------------

summary(DC)

# check missing value 

DC_rm_miss=DC[complete.cases(DC),]
DC_rm_miss
check =nrow(DC) - nrow(DC_rm_miss)
check
# output : 
# [1] 0


# check outliner

boxplot(DC[,c(-1,-2)])

df = DC_rm_miss
for( i in c(1:ncol(df))){
  
  print(paste("column : ",colnames(DC_rm_miss[i]) ))
  check_outliner(df,df[,c(i)])
  
}

check_outliner = function(df,target){ 
  
  outlier_values <- boxplot.stats(target,2)$out  # outlier values.
  
  result = sort(c(outlier_values))
  
  # print(result)
  print(paste("number of outliners : ",length(result) ))
}

#check cor
cormat = cor(DC_rm_miss[,c(-1,-2)])
# show out value > 0.5
cormat[cormat < 0.5] <- NA
cormat

cortest.bartlett(cormat, n=nrow(DC_rm_miss[,c(-1,-2)])) 
# output : 
# $p.value
# [1] 0

# b -----------------------------------------------------------------------
#Examine number of loadings

f = factanal(DC_rm_miss[,c(-1,-2)],factors=14, rotation="none") 
print(f,cutoff=.4)

f = factanal(DC_rm_miss[,c(-1,-2)],factors=6, rotation="varimax") 
print(f,cutoff=.4)

f = factanal(DC_rm_miss[,c(-1,-2)],factors=6, rotation="promax") 
print(f,cutoff=.4)

f = factanal(DC_rm_miss[,c(-1,-2)],factors=6, rotation="none") 
print(f,cutoff=.4)

f = factanal(DC_rm_miss[,c(-1,-2)],factors=6, rotation="varimax") 
print(f,cutoff=.4)

f = factanal(DC_rm_miss[,c(-1,-2)],factors=6, rotation="promax") #-----use this one-----all factors exceed 1
print(f,cutoff=.4)

#Optimization removed it cuz uniquenesses are above 0.9
f_num = 5
# use this one, cuz loadings are more higher
f = factanal(DC_rm_miss[,c(-1,-2,-4,-5,-7,-15,-18,-19,-20,-21,-30,-32,-33)],factors= f_num, rotation="none") 
print(f,cutoff=.5)

f = factanal(DC_rm_miss[,c(-1,-2,-4,-5,-7,-15,-18,-19,-20,-21,-30,-32,-33)],factors= f_num, rotation="promax") 
print(f,cutoff=.5)

f = factanal(DC_rm_miss[,c(-1,-2,-4,-5,-7,-15,-18,-19,-20,-21,-30,-32,-33)],factors= f_num, rotation="varimax") 
print(f,cutoff=.5)
cor(f$loadings)

f_v2 = fa(DC_rm_miss[,c(-1,-2,-4,-5,-7,-15,-18,-19,-20,-21,-30,-32,-33)], nfactors = 5, rotate="varimax",cor = TRUE)
cor(f_v2$loadings)
f_v2$residual



# factor.stats(DC_rm_miss[,c(-1,-2,-7,-15,-18,-19,-20,-21,-30,-32,-33)],f)

KMO(f$loadings)


ncol(DC_rm_miss[,c(-1,-2,-4,-5,-7,-15,-18,-19,-20,-21,-30,-32,-33)])













