# Logistic Regression : Predict Purchase

setwd("C:\\Users\\bipin\\OneDrive\\Documents\\R-project\\Logistic regression")

#Loading CSV dataset
df = read.csv("PurchaseData.csv")

#Checking dataset
head(df)
str(df)
summary(df)
dim(df)
View(df)
names(df)
sapply(df, class)

# Split the dataset into the Training set and Test set
#install.packages('caTools')
#library(caTools)
set.seed(2000)
split = sample.split(df$purchased, SplitRatio = 0.75)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

dim(df); dim(training_set); dim(test_set)

# Logisitic Model on Training Set
logitmodel1 = glm(purchased ~ gender + age + salary, family = binomial,  data = training_set)
summary(logitmodel1)

# Predicting the Test set results from testset
head(test_set)
prob_pred = predict(logitmodel1, type = 'response', newdata = test_set)
summary(prob_pred)
head(cbind(test_set,prob_pred ),10)

#if prob > 0.5 make it 1, else 0
y_pred = ifelse(prob_pred > 0.5, 1, 0)
head(cbind(test_set$purchased, y_pred),10)

# Making the Confusion Matrix
cm = table(test_set[,5], y_pred)
cm
library(caret)
caret::confusionMatrix(cm)

## Future Prediction. 
#predict on sample data
test_set2 = data.frame(age=c(40,65), gender=c('Male', 'Female'), salary=c(40000, 50000))
test_set2
(prob_pred2 = predict(logitmodel1, type = 'response', newdata = test_set2))
head(cbind(test_set2, prob_pred2))

y_pred2 = ifelse(prob_pred2 > 0.5, 1, 0)
head(cbind(test_set2, prob_pred2, y_pred2))

