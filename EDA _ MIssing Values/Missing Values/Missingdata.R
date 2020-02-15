###complete cases###
library(readr)

setwd("F:/Henry/IIT Mumbai/EDA & MIssing Values/Missing Values")
salespop <- read.csv("salespop.csv",header =TRUE)
sum(complete.cases(salespop)==FALSE)
salespoplm<-lm(sales~.,data=salespop)
summary(salespoplm)
#### available cases###

str(salespop)
salespop$area <- as.integer(salespop$area)


cor(salespop)
cor(salespop,use="complete.obs")

Marks<-c(88,95,85,NA,76,69,78,NA,70,68)

## Fails to calculate before NA is removed
mean(Marks)
median(Marks)
sd(Marks)
Marks[is.na(Marks)]

## Calculate the below after NA is removed
mean(Marks[!is.na(Marks)])
mean(Marks,na.rm = TRUE)
sd(Marks[!is.na(Marks)])
 
###Missing value imputation##
Math<-Marks
Math[is.na(Math)]<-mean(Marks[!is.na(Marks)])
mean(Math)
sd(Math)#### Variance decrease when we use mean imputation
Math[is.na(Math)]<-median(Marks[!is.na(Marks)])

### imputation techniques##
install.packages("HotDeckImputation")
require(HotDeckImputation)
salespop1<-as.matrix(salespop)
impute.mean(DATA=salespop1)

##### Regression imputtaion###
library(readr)

fitness <- read.csv("fitness0.csv",header=TRUE)

fitness$RunPulse <- as.numeric(fitness$RunPulse)
fitness$RunTime <- as.numeric(fitness$RunTime)

str(fitness)

x2onx1<-lm(RunTime~Oxygen,data=fitness)

new<-data.frame(Oxygen=c(59.571,50.541,47.273))
predict(x2onx1,new)

x3onx1<-lm(RunPulse~Oxygen,data=fitness)
predict(x3onx1,new)


x3onx1x2<-lm(RunPulse~RunTime+Oxygen,data=fitness)
new2<-data.frame(Oxygen=c(49.874,49.091,46.672,46.774,45.118),RunTime=c(19.874,9.091,8.672,11.774,10.118))
predict(x3onx1x2,new2)

#### hotdeck imputation###

library(readr)
fitness5 <- read.csv("fitness5.csv",header=TRUE)
require(HotDeckImputation)
fitness1<-as.matrix(fitness5)
impute.NN_HD(DATA=fitness1,distance="man")
####k-nearest neighbor approach###
install.packages("yaImpute")
library(yaImpute)
data(fitness)
set.seed(1)
refs=sample(rownames(fitness),
              + c(1,2,3,6,7,9,10,12,13,15,16,17,20:24))
x <- as.matrix(fitness[, 1])
y <- fitness[, 2:3]
raw <- yai(x = x, y = y, method = "euclidean")
plot(raw)
tail(impute(raw))
#### Maximum likelihood estimation###
install.packages("mvnmle")
library(mvnmle)

cov(fitness)
mlest(fitness)
#####multiple imputation

library(readr)
fitness2 <- read.csv("fitness2.csv",header=TRUE)

install.packages("mice")
library(mice)
library(lattice)

require(mice)
require(lattice)

imp<-mice(fitness2,m=5,maxit=2)
mat<-complete(imp)
mat
bwplot(imp)

### test for MCAR##
install.packages("BaylorEdPsych")
require(BaylorEdPsych)
LittleMCAR(fitness2)
### small p value would imply NOT MCAR###
library(mice)
attach(airquality)
names(airquality)
dim(airquality)
airquality[1:7, ]
##Removing categorical variables Month and Day
airqu<-airquality[-c(5,6)]
summary(airqu)
md.pattern(airqu)
tempData <- mice(airqu,m=5,maxit=50,meth='pmm',seed=500)
tempData$imp$Ozone
##1"means the rst imputed data set
completedData <- complete(tempData,1)
install.packages("lavaan")
library(lavaan)

lavaan()### latent value analysis
