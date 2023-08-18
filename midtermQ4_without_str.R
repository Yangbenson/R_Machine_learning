
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

#2
f = as.formula('Attrition ~ 	TotalWorkingYears+MonthlyIncome+ Age+ YearsAtCompany+NumCompaniesWorked +DistanceFromHome  +  YearsWithCurrManager+ PercentSalaryHike+    Education+  StockOptionLevel+TrainingTimesLastYear+YearsSinceLastPromotion+EnvironmentSatisfaction ' )
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



