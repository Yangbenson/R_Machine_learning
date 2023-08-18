


RW <- read.csv("~/Desktop/Assigments/2023_DM/file/red-wine.csv", stringsAsFactors=TRUE)
RW = as.data.frame(RW)
RW

# add id column
# if row.names are strings
# df$idu <- row.names(df)
# if row numbers are integers (most likely!)
# RW$idu <- as.numeric(row.names(RW))

# a 

boxplot(RW)


# b

RW_rm_miss=RW[complete.cases(RW),]
RW_rm_miss
check =nrow(RW) - nrow(RW_rm_miss)
check



#-------------------------------------------------------------------------------
# c 
# outliner:
df = RW_rm_miss
for( i in c(1:ncol(df))){
  
  print(paste("column : ",colnames(RW[i]) ))
  check_outliner(df,df[,c(i)])
  
}

check_outliner = function(df,target){
  
  outlier_values <- boxplot.stats(target)$out  # outlier values.
  
  result = sort(c(outlier_values))
  
  # print(result)
  print(paste("number of outliners : ",length(result) ))
}

# missing observation:
rownames(RW[!complete.cases(RW),])

#-------------------------------------------------------------------------------
#d
df = RW_rm_miss

df_scale = scale(df[,c(1:7,12)])
df_scale

cor(df_scale)
cov(df_scale)


#---------------------------------- test ---------------------------------------

# c



df = RW_rm_miss
rows = list()
for( i in c(1:ncol(df))){

  print(paste("column : ",colnames(RW[i]) ))
  check_outliner(df,df[,c(i)])

}

check_outliner = function(df,target){

  outlier_values = boxplot.stats(RW_rm_miss$total.sulfur.dioxide)$out  # outlier values.
  out_points <- filter(RW_rm_miss, RW_rm_miss$total.sulfur.dioxide %in% outlier_values)
  OP = data.frame(out_points)
  OP = OP[complete.cases(OP),]
  rownames(OP)
# 
#   for( i in  c(outlier_values)){
# 
#     id = which(target == i)
#     # print(c(id))
#     rows = append(rows, id)
#     print(paste("id: ",id))
#   }
  result = sort(c(outlier_values))

  print(result)
}
