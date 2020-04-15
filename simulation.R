#setwd("~/Document")

cor <- function(datafile){
data <- read.csv(datafile,sep=",")
#1st variable col # = replace 8
var1 <- as.numeric(data[,8])
#2nd variable col # = replace 9
var2 <- as.numeric(data[,9])
cor.sum=cor.test(var1,var2,method="pearson")
print(cor.sum)
print("correlation")
cor.sum$estimate
#print("p-value")
#cor.sum$p.value
}



