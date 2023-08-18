library(stringr)
library(neuralnet)  # Other packages include nnet , and RSNNS
library(RSNNS)

source("http://blogs.fiveelementanalytics.com/RCode/min_max_normalization.R")

# mnist_raw = read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_raw = read.csv("~/Desktop/Assigments/2023_DM/file/mnist_train.csv")

### SECTION 1 -----------
mnist_raw[,-1] = as.data.frame(lapply(mnist_raw[,-1], min_max_normal))
mnist_raw = replace(mnist_raw, is.na(mnist_raw), 0)


### SECTION 2-----------
mnist_raw[,"X1"]
mnist_raw[, "zero"] = as.numeric(mnist_raw[,"X5"] == 0)
mnist_raw[, "one"] = as.numeric(mnist_raw[,"X5"] == 1)
mnist_raw[, "two"] = as.numeric(mnist_raw[,"X5"] == 2)
mnist_raw[, "three"] = as.numeric(mnist_raw[,"X5"] == 3)
mnist_raw[, "four"] = as.numeric(mnist_raw[,"X5"] == 4)
mnist_raw[, "five"] = as.numeric(mnist_raw[,"X5"] == 5)
mnist_raw[, "seven"] = as.numeric(mnist_raw[,"X5"] == 7)
mnist_raw[, "eight"] = as.numeric(mnist_raw[,"X5"] == 8)
mnist_raw[, "nine"] = as.numeric(mnist_raw[,"X5"] == 9)
summary(mnist_raw[,c(785:794)])
### SECTION 3-----------
mnist_raw$actual = mnist_raw[,1]
mnist_raw = as.data.frame(mnist_raw[,-1])

### SECTION 4 Improve 降低修改column的難度，如果formula裡沒有的值要取出來
n = colnames(mnist_raw)

target = 'zero + one + two + three + four + five + six + seven + eight + nine ~ '
vec = str_split(target, " ~ ", simplify = TRUE)[[1]]
variables = str_split(vec, " \\+ ", simplify = TRUE)

target_t = paste(paste(variables[variables %in% n], collapse = " + "),"~")

for (i in 1:784) {
  
  target_t = paste(target_t, n[i],sep="+")
  
}
frm = target_t
# 
# vec = strsplit(target_t, "~")[[1]]
# frm = paste(vec[1],sub("+", "",vec[2]))
# frm

### SECTION 5-----------
mnist_raw_train = mnist_raw[1:500,]
ncol(mnist_raw_train)

### SECTION 6 -----------

neuron_estimate  =  3 
neuron_estimate

### SECTION 6 Improve

# Number of inputs is number of columns minus one since the first row has the classification
inputs = ncol(mnist_raw_train) - 1

# Number of outputs is the number of possible classifiers
outputs = 1
k = c(2:10)
for(i in k){
  inputs = ncol(mnist_raw_train) - 1
  outputs = 1
  neuron_estimate  = ceiling(nrow(mnist_raw_train) / (i * (inputs + outputs)))
  print(neuron_estimate)
}

### SECTION 7-----------
# nn = neuralnet(frm,data=mnist_raw_train[,-794],hidden=c(5,3),linear.output=F)
# plot(nn)
# predict_num = compute(nn, covariate = mnist_raw_train[,-(785:794)])$net.result
# 
# names(mnist_raw_train)[783:795] #?????
# 
# tmp_output = cbind(mnist_raw_train[1:100,"actual"], round(predict_num[1:100,],4))
# colnames(tmp_output) = c("Actual", "Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine")
# tmp_output

### SECTION 7 improve讓names自動化
nn = neuralnet(frm,data=mnist_raw_train[,-794],hidden=c(5,3),linear.output=F)
# plot(nn)
predict_num = compute(nn, covariate = mnist_raw_train[,-(785:794)])$net.result

tmp_output = cbind(mnist_raw_train[1:100,"actual"], round(predict_num[1:100,],2))
tmp_target = variables[variables %in% n]
tmp_target = c("actual",tmp_target)
colnames(tmp_output) = tmp_target
# tmp_output

tmp_output = as.data.frame(tmp_output)
features = tmp_output[,-1]
features[features == 0] = NA

numbers = c(0,1,2,3,4,5,7,8,9)
features = is.na(features)
# pre_num = c()
for (i in seq_len(nrow(features))){
  for(j in seq_along(numbers)){
    if (features[i,j] == FALSE){
      # pre_num = c(pre_num,numbers[j])
      tmp_output[i,"pre"] = numbers[j]
    }
  }
}
tmp_output = tmp_output[,c(1,ncol(tmp_output))]

accuracy = 0
for (i in 1:nrow(tmp_output)){
  if (!is.na(tmp_output[i,2]) && tmp_output[i,1] == tmp_output[i,2]){
    accuracy = accuracy + 1
  }
}
print(paste("accuracy : ",accuracy/nrow(tmp_output)))



# ---------------------------創建超參數組合----------------------------
learning_rates = c(0.1, 0.01, 0.001)
momentum = c(0, 0.5, 0.9)
hidden_nodes = c(5, 10, 15)
hyperparameters = expand.grid(learning_rate = learning_rates,
                               hidden_nodes = hidden_nodes)

h1 = c(3:5)
h2 = c(2:4)
hidden_Ns = data.frame()
for (i in seq_len(length(h1))){
  for (j in seq_len(length(h2)))
  hidden_Ns = rbind(hidden_Ns, c(h1[i],h2[j]))
}

hidden_Ns[1,1]

# 創建空的結果框架
results = data.frame()
# results = data.frame(hyperparameters = character(),
#                       accuracy = numeric(),
#                       stringsAsFactors = FALSE)

results = data.frame(hiddens= character(),
                     stringsAsFactors = FALSE)

# 迭代超參數組合，訓練模型，並測試性能
for (t in seq_len(nrow(hidden_Ns))) {
  # 創建神經網路模型
  # nn = neuralnet(frm,data=mnist_raw_train[,-794],hidden=hyperparameters[t, "hidden_nodes"],
  #                 err.fct = "sse", linear.output = FALSE, act.fct = "logistic",
  #                learningrate = hyperparameters[t, "learning_rates"],
  #                rep = 1)
  
  nn = neuralnet(frm,data=mnist_raw_train[,-794],
                 hidden=c(hidden_Ns[t,1],hidden_Ns[t,2]),
                 err.fct = "sse", 
                 linear.output = FALSE, 
                 act.fct = "logistic",
                 rep = 1)
   # 進行預測
  
  predict_num = compute(nn, covariate = mnist_raw_train[,-(785:794)])$net.result
  
  tmp_output = cbind(mnist_raw_train[1:500,"actual"], round(predict_num[1:500,],2))
  tmp_target = variables[variables %in% n]
  tmp_target = c("actual",tmp_target)
  colnames(tmp_output) = tmp_target
  # tmp_output
  
  tmp_output = as.data.frame(tmp_output)
  features = tmp_output[,-1]
  features[features == 0] = NA
  
  numbers = c(0,1,2,3,4,5,7,8,9)
  features = is.na(features)
  for (i in seq_len(nrow(features))){
    for(j in seq_along(numbers)){
      if (features[i,j] == FALSE){
        tmp_output[i,"pre"] = numbers[j]
      }
    }
  }
  tmp_output = tmp_output[,c(1,ncol(tmp_output))]
  
  accuracy = 0
  for (i in 1:nrow(tmp_output)){
    if (!is.na(tmp_output[i,2]) && tmp_output[i,1] == tmp_output[i,2]){
      accuracy = accuracy + 1
    }
  }
  print(paste("accuracy : ",accuracy/nrow(tmp_output)))
  
  # 計算準確性
  accuracy = accuracy/nrow(tmp_output)
  # 將結果加入結果框架
  results = rbind(results, data.frame(hiddeens = paste("hidden1=", hidden_Ns[t,1],"hidden2=",hidden_Ns[t,1]),
                                       accuracy = accuracy, stringsAsFactors = FALSE))
}

# 找到最佳模型
best_model = data.frame()
best_model = results[which.max(results$accuracy), ]
print(paste("Best model:", best_model$hiddeens, "- Accuracy:", best_model$accuracy))

 
