library(stringr)
library(neuralnet)  # Other packages include nnet , and RSNNS

source("http://blogs.fiveelementanalytics.com/RCode/min_max_normalization.R")

# mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
mnist_raw <- read.csv("~/Desktop/Assigments/2023_DM/file/mnist_train.csv")
mnist_test <- read.csv("~/Desktop/Assigments/2023_DM/file/mnist_test(1).csv")

### SECTION 1 -----------

mnist_raw[,-1] = as.data.frame(lapply(mnist_raw[,-1], min_max_normal))
mnist_raw = replace(mnist_raw, is.na(mnist_raw), 0)
mnist_test[,-1] = as.data.frame(lapply(mnist_test[,-1], min_max_normal))
mnist_test = replace(mnist_test, is.na(mnist_test), 0)


### SECTION 2-----------
mnist_raw[, "zero"] = as.numeric(mnist_raw[,"X5"] == 0)
mnist_raw[, "one"] = as.numeric(mnist_raw[,"X5"] == 1)
mnist_raw[, "two"] = as.numeric(mnist_raw[,"X5"] == 2)
mnist_raw[, "three"] = as.numeric(mnist_raw[,"X5"] == 3)
mnist_raw[, "four"] = as.numeric(mnist_raw[,"X5"] == 4)
mnist_raw[, "five"] = as.numeric(mnist_raw[,"X5"] == 5)
mnist_raw[, "six"] = as.numeric(mnist_raw[,"X5"] == 6)
mnist_raw[, "seven"] = as.numeric(mnist_raw[,"X5"] == 7)
mnist_raw[, "eight"] = as.numeric(mnist_raw[,"X5"] == 8)
mnist_raw[, "nine"] = as.numeric(mnist_raw[,"X5"] == 9)
summary(mnist_raw[,c(785:795)])
### SECTION 3-----------
mnist_raw$actual = mnist_raw[,1]
mnist_raw = as.data.frame(mnist_raw[,-1])

### SECTION 4-----------
# frm = 'zero + one + two + three + four + five + six + seven + eight + nine ~ '
# for (i in 1:784) {
#   
#   frm = paste(frm, names(mnist_raw)[i],sep="+")
#   
# }
# frm

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
mnist_raw_train = mnist_raw[1:2000,]
ncol(mnist_raw_train)

### SECTION 6 -----------

neuron_estimate  =  3 
neuron_estimate

### SECTION 6 Improve

# Number of inputs is number of columns minus one since the first row has the classification
inputs = ncol(mnist_raw_train) - 1

# Number of outputs is the number of possible classifiers
outputs = 9

neuron_estimate  = ceiling(nrow(mnist_raw_train) / (2 * (inputs + outputs)))

### SECTION 7 improve讓names自動化
# nn = neuralnet(frm,data=mnist_raw_train[,-794],hidden=c(3),linear.output=F)
start_time = system.time({
  nn = neuralnet(frm,data=mnist_raw_train[,-795],
                 hidden=c(2), 
                 linear.output=F,
                 stepmax = 1000000,
                 rep = 1)
})
plot(nn)

predict_num = compute(nn, covariate = mnist_raw_train[,-(785:795)])$net.result
predict_num = compute(nn, covariate = mnist_test[c(1:2000),-1])$net.result


tmp_output = cbind(mnist_raw_test[,"X7"], round(predict_num,2))

tmp_output = cbind(mnist_raw_train[,"actual"], round(predict_num,2))
tmp_target = variables[variables %in% n]
tmp_target = c("actual",tmp_target)
colnames(tmp_output) = tmp_target
# tmp_output

tmp_output = as.data.frame(tmp_output)
features = tmp_output[,-1]

numbers = c(0,1,2,3,4,5,6,7,8,9)
# features = is.na(features)

for (i in seq_len(nrow(features))){
    max_col = apply(features[i,], 1, which.max)
    tmp_output[i,"pre"] = numbers[max_col]
}

tmp_output = tmp_output[,c(1,ncol(tmp_output))]

accuracy = 0
for (i in 1:nrow(tmp_output)){
  if (!is.na(tmp_output[i,2]) && tmp_output[i,1] == tmp_output[i,2]){
    accuracy = accuracy + 1
  }
}
print(paste("accuracy : ",accuracy/nrow(tmp_output)))
  
  
  
  
  
  
  
  
## You can use these to display any image. 
## Run the function and then you can use it as below (just pass in a row)
## note that the function is 90 rotated, Ill work to fix this 
## YOU DO NOT NEED TO COMMENT THIS SECTION BELOW. 

display_digit = function( x ) {
  
  tmp = matrix(x,ncol=28, nrow=28, byrow = T) 
  tmp = apply(tmp,2,as.numeric)
  
  image(1:28,1:28,tmp, col = grey.colors(255))

} 


display_digit(mnist_raw_train[1, 1:784])

