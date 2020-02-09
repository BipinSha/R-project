#Setting working directory
setwd("C:/Users/bipin/OneDrive/Documents/R-project/Linear regression")
getwd()

#Loading dataset
omni = read.csv("salesqty.csv")

#Checking dataset
head(omni)
str(omni)
nrow(omni); ncol(omni)
dim(omni)
sapply(omni, class)

omni

#Splitting into Train and Test data
set.seed(1)
row.number <- sample(1:nrow(omni), 0.8*nrow(omni))
train = omni[row.number,]
test = omni[-row.number,]
dim(train)
dim(test)

#MLR creating multiple linear regression
# we want to see how Sales Qty depend on Price and Promotion Values
fit = lm(sqty ~ price + promotion, data=train)
summary(fit)

#understand values : R2, AdjR2, Fstats pvalue, Coeff, ***, Residuals
#F Stats pvalue = 2.86e-10 < 0.05 : Model Exists

par(mar = rep(2, 4))
plot(fit)

## Predict the SalesQty for the test data

## Remove the SalesQty column from test data
PredTest <- data.frame(test$price, test$promotion)
PredTest
nrow(PredTest)

pred <- predict(fit, newdata = test)
dfComp <- data.frame(test$sqty,pred)
dfComp

install.packages("DMwR")
library(DMwR)

DMwR::regr.eval(dfComp$test.sqty, dfComp$pred)
