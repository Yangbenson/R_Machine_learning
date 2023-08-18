library (readr) 
library(car)
library(stringr)
library(nortest)


mpg <- read.csv("~/Desktop/Assigments/2023_DM/file/mpg.csv", stringsAsFactors=FALSE)
mpg = as.data.frame(mpg)

# ---- separate car name ----

cars = unique(mpg[order(mpg$car.name),c(10)])
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
mpg_Matrix = mpg_Matrix[-36]
mpg_New = mpg_New[-36]


# ----stepwise regression function ----
stepwise_regression <- function(model,df,row_data){
  
  # stepwise
  
  model.final = step(model, na.action = na.exclude) # stepwise equation

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
    
    as.character(row_data),
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
  # print(summary(model.final))
  
  # print(paste(checks, collapse = ","))
  new_model = model.final
  
  return(records)
}




# -------- start --------

# do 80% train set regression first
formula = as.formula(mpg  ~. )
mdl = lm(formula , data=mpg_Matrix) #fisrt full equation after train set
summary(mdl)

# store the first equation and accuracy data

pre_val = list()


pred =  predict( mdl,newdata=mpg_Matrix)
err = pred - mpg_Matrix$mpg
pcterr = err / mpg_Matrix$mpg
abs_err = abs(err)
abs_pcterr = abs(pcterr)

pre_matrix = mean(pred)
err = mean(err) # == 0
pctErr = mean(pcterr )
absErr = mean(abs_err )
abspctErr = mean(abs_pcterr )

# set up dataframe for accuracy values

SA = data.frame(
  row_data = c("80% train set"),
  err = c(err),
  pcterr = c(pctErr),
  abs_err = c(absErr),
  abs_pcterr = c(abspctErr),
  regression_mdl = c(paste(mdl$call,collapse = ","))
)

pre_val = append(pre_val,pre_matrix)

# start to add rows

for (i in 1:nrow(mpg_New)) {
  
  add_row = mpg_New[i,] # pick out each rows
  row_str <- paste(mpg_New[i, ], collapse = ",") # row's call
  mpg_Matrix <- rbind(mpg_Matrix, add_row) #add row to matrix set
  
  formula = as.formula(mpg  ~. ) #create formula
  mdl = lm(formula , data=mpg_Matrix) # set up formula
  regression = stepwise_regression(mdl,mpg_Matrix,row_str) #do train set regression
  
  newrow_record = NULL
  
  # get formula and matrix accuracy values
  for( i in (1:length(regression))){
    # print(i)
    # newrow_record = c(newrow_record, regression[i])
    if (i == 6) {
      formula_str = regression[i]
      formula = as.formula(strsplit(regression[i], ",")[[1]][[2]]) #get new formula
    }
  }
  
  # set up test formula
  mdl_new = lm(formula , data=mpg_Matrix)
  
  newRow_pred =  predict(mdl_new,newdata=add_row) # predict new row & measure accuracy
  pre_val[[1]] = append(pre_val[[1]],as.character(newRow_pred))
  newRow_err = newRow_pred - add_row$mpg
  newRow_pcterr = newRow_err / add_row$mpg
  newRow_abs_err = abs(newRow_err)
  newRow_abs_pcterr = abs(newRow_pcterr)
  
  newrow_record = c(row_str, newRow_err, newRow_pcterr, newRow_abs_err, newRow_abs_pcterr,formula_str)
  SA = rbind(SA,newrow_record) #import accuracy values
  
}
# import predict values and index
SA<- cbind(SA, predict_val= pre_val)
SA<- cbind(SA, index = 1:nrow(SA))

# get average of new dataset

SA$err = as.numeric(SA$err)
SA$pcterr = as.numeric(SA$pcterr)
SA$abs_err = as.numeric(SA$abs_err)
SA$abs_pcterr = as.numeric(SA$abs_pcterr)

paste("New dataset err average",mean(SA$err))
paste("New dataset pcterr average",mean(SA$pcterr))
paste("New dataset abs_err average",mean(SA$abs_err))
paste("New dataset abs_pcter average",mean(SA$abs_pcterr))

# print chart
par(mfrow = c(2, 2))
# par(mar=c(3,3,2,1)) # set margin

plot(SA$index,format(SA$err,scientific=FALSE), type = "l", main = "err line chart", xlab ="index" , ylab = "values", col = "blue")
plot(SA$index,SA$abs_err, type = "l", main = "abs_err line chart", xlab ="index" , ylab = "values", col = "red")
plot(SA$index,SA$pcterr, type = "l", main = "abs_err line chart", xlab = "index", ylab = "values", col = "orange")
plot(SA$index,SA$abs_pcterr, type = "l", main = "abs_err line chart", xlab ="index", ylab = "values", col = "green")








