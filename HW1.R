A = matrix(c(-3,4,-1,3), nrow=2, byrow=T)
A
B = matrix(c(0,-5,2,6,4,2), nrow=3, byrow=T)
B
C = matrix(c(2,9,3), nrow=3, byrow=T)
C
D = matrix(c(1,2,2,-2), nrow=2, byrow=T)
D
E = matrix(c(9,13,12,4,3,10,4,6,12), nrow=3, byrow=T)
E
sigma = matrix(c(4,2,0,2,8,3,0,3,12), nrow=3, byrow=T)
sigma

# a
B%*%A

# b
t(A)
t(A)%*%D%*%t(B)

# c
det(D)

# d
(A%*%A)%*%t(A)

# e
matrix.trace(E)

# f
SigmaCov = sd(sigma)
SigmaCov

# g
# cov2cor(SigmaCov)

SigmaCor =  diag(x = sqrt(diag(sigma)), nrow = nrow(sigma), ncol = ncol(sigma))
SigmaCor  # Standard deviation matrix
SigmaCor_inv = solve(SigmaCor)

rho = SigmaCor_inv %*% sigma %*% SigmaCor_inv
rho

# h
det(sigma)

#------------------------------------------------------------------------------------------------

# PCA MODULE

library(FactoMineR)
library(factoextra)
library(corrplot)
source("http://blogs.5eanalytics.com/RCode/PCA_functions.R")


ff <- read.csv("/Users/bensonyang/Desktop/Assigments/2023_DM/file/forestfires.csv")
cadata_fixed = ff[ff$FFMC < 800]
cadata_fixed[,5:13]
summary(cadata_fixed)


boxplot(cadata_fixed)
pca_result = PCA(ff[,5:13], scale.unit=T, graph=T, ncp=9)
# pca_result = imputePCA(cadata_fixed, ncp=10, scale =T)
pca_result
# pca_result$eig
# pca_result$var
# pca_result$var$cos2
# pca_result$var$coord

corrplot(pca_result$var$coord, is.corr=FALSE)

# res.var$cor:变量和主成分的相关系数
# res.var$coord: 变量在主成分投影上的坐标，下面会结合图说明，因为进行了标准化，所以和相关系数结果一样，其数值代表了主成分和变量之间的相关性
# res.var$cos2: 是coord的平方，也是表示主成分和变量间的相关性，同一个变量所有cos2的总和是1
# res.var$contrib: 变量对主成分的贡献


summary(pca_result)
fviz_screeplot(pca_result, ncp=10)
fviz_pca_var(pca_result)
# Get the communalities
communality(pca_result)
#Nicely display the PC's
# display_pc(pca_result,cutoff = .2)
display_pc(pca_result)
pca_result$var
#Rotation 

#varimax Rotation and place back 
loadings.pcarot= varimax(pca_result$var$coord)$loadings
pca_result$var$coord = loadings.pcarot
plot(pca_result, choix  ="var")
display_pc(pca_result)

#compute 
pca_result$var$coord[,1:5] # show 3 dimensions 
PC = as.matrix(cadata_fixed) %*% as.matrix(pca_result$var$coord[,1:5])
colnames(PC) = c("FWI", "Humid", "Wind", "Area", "FFMC")
cadata_fixed =cbind(cadata_fixed, PC)
round(cor(ff[,5:13], use="pairwise.complete.obs"),digit = 3)
summary(pca_result)

pca_result$var$contrib


#------------------------------------------------------------------------------------------------









#Usage: communality(data)---------------------------------------------

communality <- function(pca) {
  pca2 = pca$var$cor^2
  p=ncol(pca2)
  n=nrow(pca2)
  m = matrix(c(0), ncol=p,nrow=n)
  colnames(m) = colnames(pca2)
  rownames(m) = rownames(pca2)
  for (i in 1:n){
    m[i,1] = pca2[i,1]
    for (j in 2:p) {
      m[i,j] = m[i,j-1] + pca2[i,j]
    }
  }
  round(m,4) 
}
# communality(data)---------------------------------------------------







