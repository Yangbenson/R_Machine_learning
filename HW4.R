library (readr) 
library(car)
library(stringr)
library(nortest)


mpg <- read.csv("~/Desktop/Assigments/2023_DM/file/mpg.csv", stringsAsFactors=TRUE)
mpg = as.data.frame(mpg)


# 1. mpg:           continuous
# 2. cylinders:     multi-valued discrete
# 3. displacement:  continuous
# 4. horsepower:    continuous
# 5. weight:        continuous
# 6. acceleration:  continuous
# 7. model year:    multi-valued discrete
# 8. origin:        multi-valued discrete
# 9. car name:      string (unique for each instance)

boxplot(mpg)

# ---- separate car name ----

cars = unique(mpg[order(mpg$car.name),10])
cars
cars_string = ''
for(i in cars){
  column_name = str_remove_all(string = i,pattern = '"' )
  mpg = cbind(mpg,i = as.numeric(mpg$car.name==i))
  names(mpg)[ncol(mpg)] <- column_name
  cars_string <- str_c(cars_string,  column_name , sep = "+")

}



# ---- clean column  ----

mpg = mpg[,c(-1,-10,-11,-12)]

# ---- separate to two groups----

summary(mpg)

mpg$rndnum = runif(194, 1,100)
mpg_train = mpg [ mpg[, "rndnum"] <= 80    ,     ]
mpg_test = mpg [ mpg[, "rndnum"] > 80    ,     ]

# ---- train----


# f1 = "mpg  ~  cylinders + displacement + horsepower + weight + acceleration + model_year + origin"
# f1 =  str_c(f1,cars_string)
# f1
# str_remove_all(string = i,pattern = '"' )
# 
# mdl = lm( f1 , data=mpg_train)
# summary(mdl)

f = "mpg  ~ ."
f = "mpg  ~ weight+model_year+amc +buick+chevrolet+chrysler+ford+mercury"
mdl = lm( f , data=mpg_train) # use this one
summary(mdl)

# Regression Assumptions
vif(mdl) 
mean(mdl$residuals) # Approximately 0

hist(mdl$residuals)
shapiro.test(mdl$residuals) # W = 0.96747, p-value = 0.0008836 Ha
ad.test(mdl$residuals) # A = 1.1187, p-value = 0.006094 Ha

bptest(mdl) # Ha

durbinWatsonTest(mdl) #Ha

# ------------------train modual -------------------------

mpg_train$pred =  predict( mdl,newdata=mpg_train)
mpg_train$err = mpg_train$pred - mpg_train$mpg
mpg_train$pcterr = mpg_train$err / mpg_train$mpg
mpg_train$abs_err = abs(mpg_train$err)
mpg_train$abs_pcterr = abs(mpg_train$pcterr)

#ME
#MAE
#MPE
#MAPE
mean(mpg_train$err) # == 0
mean(mpg_train$abs_err)
mean(mpg_train$pcterr)
mean(mpg_train$abs_pcterr)

# ------------------test modual -------------------------

mpg_test$pred =  predict( mdl,newdata=mpg_test)
mpg_test$err = mpg_test$pred - mpg_test$mpg
mpg_test$pcterr = mpg_test$err / mpg_test$mpg
mpg_test$abs_err = abs(mpg_test$err)
mpg_test$abs_pcterr = abs(mpg_test$pcterr)

#ME
#MAE
#MPE
#MAPE

mean(mpg_test$err, na.rm=T)
mean(mpg_test$abs_err, na.rm=T)
mean(mpg_test$pcterr, na.rm=T)
mean(mpg_test$abs_pcterr, na.rm=T)


#---------- test-----------
# row = mpg$car.name=='"amc'
# row
# mpg = cbind(mpg,amc = as.numeric(row))
# 
# f1 = "mpg  ~  cylinders + displacement + horsepower + weight + acceleration + model_year + origin"
# f1 =  str_c(f1,cars_string)
# f1
# str_remove_all(string = f1 ,pattern = 'cylinders' )
# mdl$coefficients = na.omit(mdl$coefficients)

# step_model <- step(mdl, direction = "forward", scope = list(lower = ~ 1, upper = ~ x1 + x2 + x3), criterion = "BIC")

# 1. mpg:           continuous
# 2. cylinders:     multi-valued discrete
# 3. displacement:  continuous
# 4. horsepower:    continuous
# 5. weight:        continuous
# 6. acceleration:  continuous
# 7. model year:    multi-valued discrete
# 8. origin:        multi-valued discrete
# 9. car name:      string (unique for each instance)


mpg <- read.csv("~/Desktop/Assigments/2023_DM/file/mpg.csv", stringsAsFactors=TRUE)
mpg = as.data.frame(mpg)

boxplot(mpg)

# ---- separate car name ----

cars = unique(mpg[order(mpg$car.name),10])
cars
cars_string = ''
for(i in cars){
  column_name = str_remove_all(string = i,pattern = '"' )
  mpg = cbind(mpg,i = as.numeric(mpg$car.name==i))
  names(mpg)[ncol(mpg)] <- column_name
  cars_string <- str_c(cars_string,  column_name , sep = "+")
  
}



# ---- clean column  ----

mpg = mpg[,c(-1,-10,-11,-12)]

# ---- separate to two groups----

summary(mpg)

mpg$rndnum = runif(194, 1,100)
mpg_Matrix = mpg [ mpg[, "rndnum"] <= 80    ,     ]
mpg_New = mpg [ mpg[, "rndnum"] > 80    ,     ]

# set up storage 

SA = data.frame(matrix(ncol = 6))
colnames(SA) <- c("row_data", "err","train_pcterr","abs_err","abs_pcterr","regression_mdl")


mdl = lm(mpg  ~. , data=mpg_Matrix) #fisrt full equation after train set
# summary(mdl)
# print(mdl)
mdl = stepwise_regression(mdl,mpg_Matrix,'test')



# ---- do stepwise regression ----
stepwise_regression <- function(model,df,row_data){
  
  # stepwise

  model.final = step(model) # stepwise equation
  # summary(model.final)
  
  # check Regression Assumptions
  # vif(model.final) 
  # mean(model.final$residuals) 
  # shapiro.test(model.final$residuals) 
  # ad.test(model.final$residuals) 
  # bptest(model.final) 
  # durbinWatsonTest(model.final) 

  # add new pre & error
  df$pred =  predict( model.final,newdata=df)
  df$err = df$pred - df$mpg
  df$pcterr = df$err / df$mpg
  df$abs_err = abs(df$err)
  df$abs_pcterr = abs(df$pcterr)
  
  # check error
  err = mean(df$err) # == 0
  pctErr = mean(df$pcterr)
  absErr = mean(df$abs_err)
  abspctErr = mean(df$abs_pcterr)
  
  records = c(
    
        row_data,
       #  vif(model.final),
       # mean(model.final$residuals),
       # shapiro.test(model.final$residuals),
       # ad.test(model.final$residuals),
       # bptest(model.final),
       # durbinWatsonTest(model.final),
       format(err, digits = 6, nsmall = 4, scientific = FALSE) ,
       format(pctErr, digits = 6, nsmall = 4, scientific = FALSE) ,
       format(absErr, digits = 6, nsmall = 4, scientific = FALSE),
       format(abspctErr, digits = 6, nsmall = 4, scientific = FALSE) ,
       paste(model.final$call,collapse = ",")
             )
  print(records)
  SA = rbind(SA,records)
  # print(summary(model.final))
  
  # print(paste(checks, collapse = ","))
  new_model = model.final
  
return(new_model)
}

# start to add rows

for (i in 1:nrow(mpg_New)) {
  add_row = mpg_New[i,] # pick out each rows
  print(add_row )
  row_str <- paste(mpg_New[i, ], collapse = ",") # row's call
  mpg_Matrix <- rbind(mpg_Matrix, add_row)
  
}



# ---- test ----

  mdl = lm(mpg  ~. , data=mpg_Matrix) #fisrt full equation after train set
  # stepwise

  model.final = step(mdl) # stepwise equation
  # summary(model.final)

  # check Regression Assumptions
  # vif(model.final)
  # mean(model.final$residuals)
  # shapiro.test(model.final$residuals)
  # ad.test(model.final$residuals)
  # bptest(model.final)
  # durbinWatsonTest(model.final)

  # add new pre & error
  mpg_Matrix$pred =  predict( model.final,newdata=mpg_Matrix)
  mpg_Matrix$err = mpg_Matrix$pred - mpg_Matrix$mpg
  mpg_Matrix$pcterr = mpg_Matrix$err / mpg_Matrix$mpg
  mpg_Matrix$abs_err = abs(mpg_Matrix$err)
  mpg_Matrix$abs_pcterr = abs(mpg_Matrix$pcterr)

  # check error
  err = mean(mpg_Matrix$err) # == 0
  pctErr = mean(mpg_Matrix$pcterr)
  absErr = mean(mpg_Matrix$abs_err)
  abspctErr = mean(mpg_Matrix$abs_pcterr)

  records = c(

    "row_data",
    # paste(vif(model.final),collapse = ","),
    # mean(model.final$residuals),
    # shapiro.test(model.final$residuals),
    # ad.test(model.final$residuals),
    # bptest(model.final),
    # durbinWatsonTest(model.final),
    err,
    pctErr,
    absErr,
    abspctErr,
    paste(model.final$call,collapse = ",")

  )
  print(records)
  SA = rbind(SA,records)
  # print(summary(model.final))

  # print(paste(checks, collapse = ","))
  new_model = model.final




# ------------------train modual -------------------------

mpg_train$pred =  predict( mdl.final,newdata=mpg_train)
mpg_train$err = mpg_train$pred - mpg_train$mpg
mpg_train$pcterr = mpg_train$err / mpg_train$mpg
mpg_train$abs_err = abs(mpg_train$err)
mpg_train$abs_pcterr = abs(mpg_train$pcterr)

#ME
#MPE
#MAE
#MAPE
mean(mpg_train$err) # == 0
mean(mpg_train$pcterr)
mean(mpg_train$abs_err)
mean(mpg_train$abs_pcterr)

# ------------------test modual -------------------------



mpg_test$pred =  predict( mdl.final,newdata=mpg_test)
mpg_test$err = mpg_test$pred - mpg_test$mpg
mpg_test$pcterr = mpg_test$err / mpg_test$mpg
mpg_test$abs_err = abs(mpg_test$err)
mpg_test$abs_pcterr = abs(mpg_test$pcterr)

#ME
#MAE
#MPE
#MAPE

mean(mpg_test$err, na.rm=T)
mean(mpg_test$pcterr, na.rm=T)
mean(mpg_test$abs_err, na.rm=T)
mean(mpg_test$abs_pcterr, na.rm=T)






















