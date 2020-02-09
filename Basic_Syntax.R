#To setup working directory
setwd("D:/HenryHarvin/R code")
getwd()

#To check the existing files in directory
dir()

#For help
help(solve)

#syntax for installing pacakages
install.packages("e1071")
library(e1071)

sum(z<-1:50)
print(sum)

P<-10000
N<-5
R<-15
Interest<-(P*N*R)/100
print(Interest)

floor(5.3)
ceiling(5.3)

x <- "2.4"
class(x)
as.numeric(x)+3

b <- c(1, 0.5, 4, 5,2,6,8,9,3)
b
mean(b)
min(b)
max(b)
sd(b)
var(b)
quantile(b)

#loading CSV file and checking dataset
titanicdata <- read.csv("train.csv")
head(titanicdata)
View(titanicdata) #displays data in excel format
head(titanicdata$PassengerId)
names(titanicdata)
str(titanicdata)

class(titanicdata)
class(titanicdata$Pclass)
sapply(titanicdata, class)
nrow(titanicdata)
ncol(titanicdata)

Name<-titanicdata["Name"]



