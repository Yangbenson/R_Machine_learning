install.packages("mice")
library(mice)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(car)
library(stats)
library(psych)
library(stringr)
source("http://blogs.5eanalytics.com/RCode/PCA_functions.R")
source("/Users/bensonyang/Desktop/Side-project/R/function.R")

# 1 Question 1 - Linear Algebra (15 Pts)----
m_A = matrix(c(8,3,5,4), nrow=2, byrow=T)
m_B = matrix(c(4,1,6,2,3,3), nrow=3, byrow=T)
m_F = matrix(c(9,5,7,2,6,8,3,6,3), nrow=3, byrow=T)
sigma = matrix(c(9,3,0,3,4,2,0,2,8), nrow=3, byrow=T)

m_A
m_B
m_F
sigma

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

# i
det(m_A)
det(m_B)
det(m_F)
det(sigma)
solve(sigma)
sigma%*%solve(sigma)
m_F%*%solve(m_F)




# 2 Question 2 - PCA (25 Pts)----

### FIELDS
#0 Date (DD/MM/YYYY)
#1 Time (HH.MM.SS)
#2 True hourly averaged concentration CO in mg/m^3 (reference analyzer)
#3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)
#4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer)
#5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer)
#6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)
#7 True hourly averaged NOx concentration in ppb (reference analyzer)
#8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted)
#9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)
#10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)
#11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted)
#12 Temperature in Â°C
##13 Relative Humidity (%)
#14 AH Absolute Humidity

AirQualityUCI <- read.csv("~/Desktop/Assigments/2023_DM/file/AirQualityUCI.csv")
AQ = as.data.frame(AirQualityUCI)

#(a)

summary(AQ)
str(AQ)

AQ = AQ[,c(-1,-2,-3)] # remove Date,Time column

boxplot(AQ) # outliner
boxplot(AQ[,c(3,9)])
AQ = AQ[AQ[,7]<2000,]
AQ = AQ[AQ[,5]<2000,]
AQ = AQ[AQ[,6]<1250,]
AQ = AQ[AQ[,3]<1000,]
AQ = AQ[AQ[,2]<2000,]

for(i in c(1,2,4,5,7:13)){
  AQ = AQ[AQ[,i]>-200,]
}

AQ_r_m=AQ[complete.cases(AQ),] #missing values
AQ_r_m
check =nrow(AQ) - nrow(AQ_r_m)
check

#(b) PCA

# 1

# change data to numeric
# for (i in 1:ncol(AQ)){
#   AQ[,i] = as.numeric(AQ[,i])
# }

# PCA none rotate
pca_result = PCA(AQ, scale.unit=T, graph=T, ncp=13) 

display_pc(pca_result,cutoff = .6) # 1,2
summary(pca_result)

fviz_screeplot(pca_result, ncp=13) # 3
communality(pca_result) # 4 

# PCA varimax rotate

loadings.pcarot= varimax(pca_result$var$coord)$loadings
pca_result$var$coord = loadings.pcarot
plot(pca_result, choix  ="var")
display_pc(pca_result,cutoff = 0.6)

# compute values for each PCA dimension
AQ.final= as.matrix(AQ) %*% as.matrix(pca_result$var$coord[,1:4]) #compute AQ %*% pca dim 1~4
AQ.final
cor(AQ.final, use="pairwise.complete.obs")

AQ =cbind(AQ, AQ.final)
cor(AQ, use="pairwise.complete.obs")






























# 3 Question 3 - Factor Analysis (25 Pts)----

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

FF_rm = FF_rm[,c(1:5,7:13,25,29,39:41,46,47,49)] # keep necessary columns

FA = fa(FF_rm, nfactors = 6, rotate="varimax",cor = TRUE)
cor(FA$loadings)
FA$residual

f = factanal(FF_rm,factors=6, rotation="varimax") 
print(f,cutoff=.6)

# keep the column which Uniqueness is below 0.8

FF_rm = FF_rm[,c(-4,-7,-9)] # keep necessary columns

colnames(FF_rm)[9] =  "LS~LM,RM"
colnames(FF_rm)[10] = "LCM~RCM"
colnames(FF_rm)[11] = "LWB~RB"
  
FA = fa(FF_rm, nfactors = 6, rotate="varimax",cor = TRUE)
cor(FA$loadings)
FA$residual

f = factanal(FF_rm,factors=6, rotation="varimax") 
print(f,cutoff=.6)


# (CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC)

f = factanal(FF_rm,factors=6, rotation="varimax") 
print(f,cutoff=.4)

f = factanal(FF_rm,factors=6, rotation="promax") 
print(f,cutoff=.4)

# (DDDDDDDDDDDDDDDDDDDDDDD & EEEEEEEEEEEEEEEEEEEEEEEE)

f = factanal(FF_rm,factors=6, rotation="promax") 
print(f,cutoff=.5)

# (GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG)

# Split the data set into two parts (Left footed players and right footed players).

FF_QG=FF_QG[complete.cases(FF),] #missing values

FF_QG[,14] = (FF_QG[,14]+FF_QG[,15]+FF_QG[,16]+FF_QG[,17]+FF_QG[,18]+FF_QG[,19]+FF_QG[,20]+FF_QG[,21]+FF_QG[,22]+FF_QG[,23]+FF_QG[,24]+FF_QG[,25]+FF_QG[,29])/13
FF_QG[,26] = (FF_QG[,26]+FF_QG[,27]+FF_QG[,28])/3
FF_QG[,30] = (FF_QG[,30]+FF_QG[,31]+FF_QG[,32]+FF_QG[,33]+FF_QG[,34]+FF_QG[,35]+FF_QG[,36]+FF_QG[,37]+FF_QG[,38]+FF_QG[,39])/10
FF_QG = FF_QG[,c(1:5,7:14,26,30,40:42,47,48,50)] # keep necessary columns

FF_QG = FF_QG[,c(-4,-8,-10)] # keep necessary columns

colnames(FF_QG)[9] =  "LS~LM,RM"
colnames(FF_QG)[10] = "LCM~RCM"
colnames(FF_QG)[11] = "LWB~RB"

FF_R = FF_QG[FF_QG$Preferred.Foot == "Right",]
FF_L = FF_QG[FF_QG$Preferred.Foot == "Left",]
FF_R = FF_R[,-5]
FF_L = FF_L[,-5]

# # Right foot
# f = factanal(FF_R,factors=6, rotation="none") 
# print(f,cutoff=.5)
# 
# #left foot
# f = factanal(FF_L,factors=6, rotation="none") 
# print(f,cutoff=.5)

# Right foot varimax
FVR = factanal(FF_R,factors=6, rotation="varimax") 
print(FVR,cutoff=.5)
var_exp(FVR$correlation)

#left foot varimax
FVL = factanal(FF_L,factors=6, rotation="varimax") 
print(FVL,cutoff=.5)
var_exp(FVL$correlation)

# Right foot promax
FPR = factanal(FF_R,factors=6, rotation="promax")
print(FPR,cutoff=.5)
var_exp(FPR$correlation)

#left foot promax
FPL = factanal(FF_L,factors=6, rotation="promax")
print(FPL,cutoff=.5)
var_exp(FPR$correlation)

dif = var_exp(FVR$correlation)- var_exp(FVL$correlation)

names(FF_original)

# proportion of variance explained
var_exp = function(f_correlation){
  variance_explained <- apply(f_correlation, 2, function(x) sum(x^2)) # sum every value^2 in a row
  print(variance_explained / sum(variance_explained))
}





# 4 Question 4 - Decision Tree (25 Pts)----

library(party)
library("rpart.plot") 
library("rpart")
library("readr")

GD <- read.csv("~/Desktop/Assigments/2023_DM/file/general_data(1).csv")
MSD <- read.csv("~/Desktop/Assigments/2023_DM/file/manager_survey_data(1).csv")
ESD <- read.csv("~/Desktop/Assigments/2023_DM/file/employee_survey_data(1).csv")

f_df <- merge(merge(GD, MSD, by = "EmployeeID"), ESD, by = "EmployeeID")

#aaaaaaaaaaaaaaaaaaaaaaaaaaaa & bbbbbbbbbbbbbbbbbbbbbbbbbbbb
summary(f_df)

#missing values
f_r_m=f_df[complete.cases(f_df),]
check =nrow(f_df) - nrow(f_r_m)
check

f_df = f_r_m[,c(-1,-9,-16,-18)]

boxplot(f_df[,c(-2,-3,-4,-7,-8,-10,-11)])

f_df$Attrition = as.factor(f_df$Attrition)
f_df$BusinessTravel = as.factor(f_df$BusinessTravel)
f_df$Department = as.factor(f_df$Department)
f_df$EducationField = as.factor(f_df$EducationField)
f_df$Gender = as.factor(f_df$Gender)
f_df$JobRole = as.factor(f_df$JobRole)
f_df$MaritalStatus = as.factor(f_df$MaritalStatus)

library(Hmisc)  # Needed for %nin%
totalrows = nrow(f_df)
f_df$id= rownames(f_df)
pickrows = round(runif(totalrows*.80, 1, totalrows),0)

trainf_df_ctree = f_df[pickrows, ]
testf_df_ctree = f_df[-pickrows, ]

trainf_df_ctree = trainf_df_ctree[,-26]
testf_df_ctree = testf_df_ctree[,-26]

trainf_df_c50 = trainf_df_ctree
testf_df_c50 = testf_df_ctree

trainf_df = trainf_df_ctree
testf_df = testf_df_ctree


# -----------------------------------------

colname_string = ''
for(i in colnames(f_df)){
  colname_string <- str_c(colname_string,  i , sep = "+")
}
colname_string

#develop decision tree uses CART Method
fullmodel = 'Attrition ~.'
fullmodel = 'Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + Education + EducationField + Gender + JobLevel + JobRole + MaritalStatus + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + PerformanceRating + EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance'
cfit = rpart(fullmodel, data = trainf_df, method = "class")

#1 without string
f = as.formula('Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + Education + EducationField + Gender + JobLevel + JobRole + MaritalStatus + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + PerformanceRating + EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance')

cfit = rpart(f, data = trainf_df, method = "class")

#2
f = as.formula('Attrition ~ 	TotalWorkingYears+MonthlyIncome+JobRole+ Age +EducationField+ YearsAtCompany+NumCompaniesWorked +DistanceFromHome  +  YearsWithCurrManager+ PercentSalaryHike+    Education+  StockOptionLevel+MaritalStatus+TrainingTimesLastYear +BusinessTravel+ Department +YearsSinceLastPromotion+EnvironmentSatisfaction ' )
cfit = rpart(f, data = trainf_df, method = "class")

#print the model 
# print(cfit)
summary(cfit)  #print complete information
cfit$variable.importance
#plot the model
rpart.plot(cfit, main =fullmodel,
           box.palette="Blues")
#output some nice formatted rules and get probabilities
rpart.rules(cfit)

# plotcp(carfit)
# printcp(carfit)

testf_df$predict = predict(cfit, newdata=testf_df, type='class')

#Counts
tab = table(testf_df$Attrition, testf_df$predict)
tab
#Predict Table as Pct
pcttab <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
print(round(pcttab, 3))


# cormat = cor(f_df)
# # show out value > 0.5
# cormat[cormat < 0.3  ] <- NA
# cor_df = as.data.frame(cormat)



# summary(trainf_df_ctree)
# summary(testf_df_ctree)

#develop decision tree against training set using CTree Method--
output.tree <- ctree(
  f, 
  data = trainf_df_ctree)

# summary(output.tree)
#Alternatively you can plot
testf_df_ctree$predict = predict(output.tree, newdata=testf_df_ctree, type='response')

#Counts
tab = table(testf_df_ctree$Attrition, testf_df_ctree$predict)
tab
#Predict Table as Pct
pcttab <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
print(round(pcttab, 3))
plot(output.tree)



library(C50)
# Now use the C50 Algorithm --
ruleModel <- C5.0(f, data = trainf_df_c50)
#summary(ruleModel)
#Alternatively you can plot
testf_df_c50$predict = predict(ruleModel, newdata=testf_df_c50, type='class')

#Counts
tab = table(testf_df_c50$Attrition, testf_df_c50$predict)
tab
#Predict Table as Pct
pcttab <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
print(round(pcttab, 3))
plot(ruleModel)

 