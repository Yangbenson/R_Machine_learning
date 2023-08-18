library(dplyr)
library(party)
library("rpart.plot") 
library("rpart")
library("readr")
library(C50)
library(stringr)
library(ggplot2)


# ------ 1 ------
GD <- read.csv("~/Desktop/Assigments/2023_DM/file/general_data(1).csv")
MSD <- read.csv("~/Desktop/Assigments/2023_DM/file/manager_survey_data(1).csv")
ESD <- read.csv("~/Desktop/Assigments/2023_DM/file/employee_survey_data(1).csv")

f_df <- merge(merge(GD, MSD, by = "EmployeeID"), ESD, by = "EmployeeID")

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


unique(f_df[,3])
f_df$BusinessTravel <- recode(f_df$BusinessTravel, "Non-Travel" = 0, "Travel_Frequently" = 2, "Travel_Rarely" = 1)

# gender discrimination and equitability

bias = f_df[,c(1,4,8,9,12,14,17,19,21,22)]
bias$JobLevel = as.numeric(bias$JobLevel)
colnames(bias)
bias_cormat = as.data.frame(cor(bias[,-c(2,3,4)]))

# split the dataset
bias_M = bias[bias$Gender == 'Male',]
bias_F = bias[bias$Gender == 'Female',]

cm_m = cor(bias_M[,-c(2,3)])
cm_f = cor(bias_F[,-c(2,3)])

cm_diff = cm_f - cm_m

summary(bias_M)
summary(bias_F)

unique(bias_M$Department)

# ---- satisfaction ----
#  clean data
df_sa = f_r_m[,c(2,4,6,7,11,13,14,15,17,19:25,27:29)]

df_sa_r_m=df_sa[complete.cases(df_sa),]
check =nrow(df_sa) - nrow(df_sa_r_m)
check

df_sa$BusinessTravel = recode(df_sa$BusinessTravel, "Non-Travel" = 0, "Travel_Frequently" = 2, "Travel_Rarely" = 1)
df_sa$MaritalStatus = recode(df_sa$MaritalStatus,"Divorced" = 2, "Married" = 1, "Single" = 0)


# 
# df_sa$MonthlyIncome_group <- recode(df_sa$MonthlyIncome,
#                                     `0:18990` = "1",
#                                     `18991:37980` = "2",
#                                     `37981:56970` = "3",
#                                     `56971:75960` = "4",
#                                     `75961:94950` = "5",
#                                     `94951:113940` = "6",
#                                     `113941:132930` = "7",
#                                     `132931:151920` = "8",
#                                     `151921:170910` = "9",
#                                     `170911:199990` = "10",
#                                     .default = "1")
# Income_interval = (max(df_sa$MonthlyIncome)-min(df_sa$MonthlyIncome))/10
# Income_interval = (max(df_sa$YearsWithCurrManager)-min(df_sa$YearsWithCurrManager))
# 
# df_sa$MonthlyIncome_group <- cut(df_sa$MonthlyIncome, breaks = 10, labels = FALSE)
# df_sa$age_group <- cut(df_sa$Age, breaks = 5, labels = FALSE)
# df_sa$YearsWithCurrManager_group <- cut(df_sa$YearsWithCurrManager, breaks = 4, labels = FALSE)


# df_sa <- as.data.frame(lapply(df_sa, factor))

summary(df_sa)

totalrows = nrow(df_sa)
df_sa$id= rownames(df_sa)
pickrows = round(runif(totalrows*.80, 1, totalrows),0)
train_df = df_sa[pickrows, ]
test_df = df_sa[-pickrows, ]

f = as.formula('JobSatisfaction ~ Age+BusinessTravel+DistanceFromHome+Education+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+PercentSalaryHike+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+YearsAtCompany+YearsSinceLastPromotion+YearsWithCurrManager+JobInvolvement+EnvironmentSatisfaction+WorkLifeBalance')

# stepwise equation
mdl.final = step(lm(f , data=train_df)) 
summary(mdl.final)

mdlg.final = step(glm(f , data=train_df)) 
summary(mdlg.final)

mdl.final$coefficients

coef_df <- data.frame(Coefficient = names(mdlg.final$coefficients),
                      Estimate = mdlg.final$coefficients,
                      Std_Error = summary(mdlg.final)$coefficients[, "Std. Error"])
coef_df = coef_df[-1,]
# 绘制棒图

ggplot(data = coef_df, aes(x = Coefficient, y = Estimate, fill = factor(sign(Estimate)))) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  geom_errorbar(aes(ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error), width = 0.4, color = "black") +
  coord_flip() +
  labs(title = "Coefficients of Linear Regression Model",
       x = "",
       y = "Estimate") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

















