getwd()
setwd("F:/Henry/IIT Mumbai/EDA & MIssing Values")

df = read.csv("train2.csv")

View(df)

#round(cor(df), 3)

sapply(df, class)

dfInt <- Filter(is.numeric, df) 

sapply(dfInt, class)  

dfFactor <- Filter(is.factor, df)
sapply(dfFactor, class)  

## Remove columns fron dfInt which are like ID and categorical in nature and representative in integer

dfInt$Id <- NULL 
dfInt$YearBuilt <- NULL 
dfInt$YearRemodAdd <- NULL 
dfInt$MoSold <- NULL
dfInt$YrSold <- NULL 
dfInt$MSSubClass <- NULL

names(dfInt)

cor(dfInt$SalePrice, as.matrix(dfInt[sapply(dfInt, is.numeric)]))

## High Positive and High Negative Correlations are important
## Anything close to zero is not that important

## Correlation is between -1 and +1
## So pick those which have corrleation from +.5 to +1 and -.5 to -1 

install.packages("corrplot")
library(corrplot)

names(dfInt)

cordfInt <- cor(dfInt)

corrplot(cordfInt, type = "lower", tl.col = "black")

head(dfInt)

## Categorical Variables

names(dfFactor)


library(funModeling) 
library(tidyverse) 
library(Hmisc)


freq(dfFactor)