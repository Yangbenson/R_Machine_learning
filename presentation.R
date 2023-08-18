library (readr) 
library(car)
library(stringr)
library(nortest)

spotify_datamining = read.csv("~/Desktop/Assigments/2023_DM/file/spotify_datamining.csv")
spotify_data = as.data.frame(spotify_datamining)



# ---- clean column  ----
spotify_data_F=spotify_data[complete.cases(spotify_data),] #missing values
check =nrow(spotify_data) - nrow(spotify_data_F)
check

s_data = spotify_data_F[,-c(1,2,14:18,22,23,25,26)]

colnames(s_data_New)
# ---- final check ----
summary(s_data)
boxplot(s_data[,-c(14,16)])


# ---- separate to two groups----

summary(s_data)

s_data$rndnum = runif(nrow(s_data), 1,1000)
# s_data$genre = recode(s_data$genre, "Non-Travel" = 0, "Travel_Frequently" = 2, "Travel_Rarely" = 1)
s_data_train = s_data [ s_data[, "rndnum"] <= 800,]
s_data_New = s_data [ s_data[, "rndnum"] > 800,]
s_data_train = s_data_train[,-(ncol(s_data_train))]
s_data_New = s_data_New[,-ncol(s_data_New)]

s_train = s_data_train[,-14]
s_New = s_data_New[,-14]

# ----stepwise regression function ----
stepwise_regression <- function(model,df,row_data){
  
  # stepwise
  model.final = step(model, na.action = na.exclude) # stepwise equation
  
  # add new pre & error
  df$pred =  predict( model.final,newdata=df)
  df$err = df$pred - df$popularity
  df$pcterr = df$err / df$popularity
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
formula = as.formula(popularity  ~. )
mdl = lm(formula , data=s_train) #fisrt full equation after train set
summary(mdl)

# store the first equation and accuracy data

pre_val = list()

pred =  predict( mdl,newdata=s_train)
err = pred - s_train$popularity
pcterr = err / s_train$popularity
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
  abs_pcterr = c(abspctErr)
)

pre_val = append(pre_val,pre_matrix)

# start to add rows
time_stamp = system.time({
for (i in 1:nrow(s_New)) {
  
  add_row = s_New[i,] # pick out each rows
  row_str <- paste(s_New[i, ], collapse = ",") # row's call
  s_train <- rbind(s_train, add_row) #add row to matrix set
  
  formula = as.formula(popularity  ~. ) #create formula
  mdl = lm(formula , data=s_train) # set up formula
  # regression = stepwise_regression(mdl,s_train,row_str) #do train set regression
  model.final = step(mdl, na.action = na.exclude) # stepwise equation
  
  formula(model.final)
  
  newrow_record = NULL
  
  # set up test formula
  mdl_new = lm(formula(model.final) , data=s_train)
  
  newRow_pred =  predict(mdl_new,newdata=add_row) # predict new row & measure accuracy
  pre_val[[1]] = append(pre_val[[1]],as.character(newRow_pred))
  newRow_err = newRow_pred - add_row$popularity
  newRow_pcterr = newRow_err / add_row$popularity
  newRow_abs_err = abs(newRow_err)
  newRow_abs_pcterr = abs(newRow_pcterr)
  
  newrow_record = c(row_str, newRow_err, newRow_pcterr, newRow_abs_err, newRow_abs_pcterr)
  SA = rbind(SA,newrow_record) #import accuracy values
  
}
})
# import predict values and index
SA<- cbind(SA, predict_val= pre_val)
SA<- cbind(SA, index = 1:nrow(SA))



# print chart
par(mfrow = c(2, 2))
# par(mar=c(3,3,2,1)) # set margin

# filter Inf value
SA[SA$pcterr ==Inf,] = NA
SA=SA[complete.cases(SA),] #missing values


plot(SA$index,format(SA$err,scientific=FALSE), type = "l", main = "err line chart", xlab ="index" , ylab = "values", col = "blue")
plot(SA$index,SA$abs_err, type = "l", main = "abs_err line chart", xlab ="index" , ylab = "values", col = "red")
plot(SA$index,SA$pcterr, type = "l", main = "abs_err line chart", xlab = "index", ylab = "values", col = "orange")
plot(SA$index,SA$abs_pcterr, type = "l", main = "abs_err line chart", xlab ="index", ylab = "values", col = "green")

# get average of new dataset

SA$err = as.numeric(SA$err)
SA$pcterr = as.numeric(SA$pcterr)
SA$abs_err = as.numeric(SA$abs_err)
SA$abs_pcterr = as.numeric(SA$abs_pcterr)

paste("New dataset err average",mean(SA$err))
paste("New dataset pcterr average",mean(SA$pcterr))
paste("New dataset abs_err average",mean(SA$abs_err))
paste("New dataset abs_pcter average",mean(SA$abs_pcterr))

summary(mdl_new)

# chart :

coef_df <- data.frame(Coefficient = names(mdl_new$coefficients),
                      Estimate = mdl_new$coefficients,
                      Std_Error = summary(mdl_new)$coefficients[, "Std. Error"])
coef_df = coef_df[-1,]

ggplot(data = coef_df, aes(x = Coefficient, y = Estimate, fill = factor(sign(Estimate)))) +
  geom_bar(stat = "identity", position = "identity", color = "black") +
  geom_errorbar(aes(ymin = Estimate - 2 * Std_Error, ymax = Estimate + 2 * Std_Error), width = 0.4, color = "black") +
  coord_flip() +
  labs(title = "Coefficients of Song feature",
       x = "",
       y = "Estimate") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
