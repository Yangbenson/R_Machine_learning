

# 1
RW <- read.csv("~/Desktop/Assigments/2023_DM/file/red-wine.csv", stringsAsFactors=TRUE)

RW = as.data.frame(RW)
summary(RW)
boxplot(RW$total.sulfur.dioxide)
boxplot(RW[RW$total.sulfur.dioxide < 40,]$total.sulfur.dioxide)

