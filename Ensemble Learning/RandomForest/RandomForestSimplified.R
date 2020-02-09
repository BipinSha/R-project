library(randomForest)
require(caTools)

setwd("F:/Henry/IIT Mumbai/Supervised Learning/RandomForest")
getwd()
dir()

data <- read.csv(
  "processed.cleveland.data",
  header=FALSE
)

dim(data)

names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")

head(data)

## we’re only going to attempt to distinguish the presence of heart disease 
## (values 1,2,3,4) from absence of heart disease (value 0). 
## Therefore, we replace all labels greater than 1 by 1.

data$num[data$num > 1] <- 1
summary(data)
sapply(data, class)

data$sex

## In R, a categorical variable (a variable that takes on a finite amount of values)
## is a factor. As we can see, sex is incorrectly treated as a 
## number when in reality it can only be 1 if male and 0 if female. 
## We can use the transform method to change the in built type of each feature.



data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)

sapply(data, class)

summary(data)

## Replacing ? with NA which is programmably identifiable NULL in R

data[ data == "?"] <- NA
colSums(is.na(data))

data <- data[!(data$ca %in% c(NA)),]
data <- data[!(data$thai %in% c(NA)),]


sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(
  num ~ .,
  data=train
)

library(caret)
varImp(rf)

pred = predict(rf, newdata=test[-14])

cm = table(test[,14], pred)
cm











