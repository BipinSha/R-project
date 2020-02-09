
install.packages("dplyr")
install.packages("FactoMineR")
install.packages("ggplot2")
install.packages("funModeling")

library(dplyr)
library(FactoMineR)
library(ggplot2)
library(funModeling)


setwd("C:\\Users\\bipin\\OneDrive\\Documents\\R-project\\K-means Clustering")
getwd()
dir()

cust <- read.csv("Mall_Customers.csv")
names(cust) <- c("CustomerID","Gender","Age","Annual_Income", "Spending_Score")
head(cust)

## Data description:
##CustomerID: Unique ID assigned to the customer.
## Gender: Gender of the customer.
## Age: Age of the customer.
## Annual_Income (k$): Annual Income of the customer.
## Spending_Score (1-100): Score assigned by the mall based on customer behavior and spending nature.

## Inspect Data
## Check Missing Value

colSums(is.na(cust))

## Explanotory Data Analysis (EDA)


## Customer Distribution by Age

hist(cust$Age,
     col="orange",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

## Scaling Data
## Choosing Annual Income & Spending score for clustering subject and scale the data

cust.sc <-scale(cust[,c(4,5)])

## Use Elbow method to find the optimum k value 

wss <- function(data, maxCluster = 10) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares", pch=19)
}

set.seed(100)
wss(cust.sc)


## Create 5 clusters as per the elbow method's suggestion. 

cust.KM<-kmeans(cust.sc,5)  
cust.KM

# Adding 'Cluster' column 
cust$Cluster <- cust.KM$cluster
cust

c_Clust=cust[,c(4,5)]
ggplot(c_Clust, aes(x = Annual_Income, y = Spending_Score)) + 
  geom_point(stat = "identity", aes(color = as.factor(cust.KM$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
  ggtitle("Customer Cluster")+
  xlab("Annual Income")+ylab("Spending Score")

#Interpretation for the customer cluster/segment:
#Cluster 1. Customers with high annual income but low spending score.
#Cluster 2. Customers with medium annual income and medium spending score.
#Cluster 3. Customers with low annual income and low spending score.
#Cluster 4. Customers with low annual income but high spending score.
#Cluster 5. Customers with high annual income and high spending score

cust %>% group_by(Cluster,Gender) %>% 
  summarise(med_age=median(Age),med_income = median(Annual_Income), med_spend = median(Spending_Score))

cust %>% group_by(Cluster,Gender) %>% 
  summarise(med_age=mean(Age),med_income = mean(Annual_Income), med_spend = mean(Spending_Score))

## Summary 

## We could see from the EDA part that the female customers percentage (56%) is slightly higher 
## than male customers (44%),with this information we could targeting the male customers more for marketing campaign 
## or promotions than female customers even though the percentage different is not too big. 
## This case we also can choose male customer target wisely with combined factors from their age and cluster.

## We can doing marketing campaigns/loyalty program to customer in cluster 5 & 4 which are customer 
## who have high spending scores, especially customer at age 20’s & 30’s, 
## to maintain such customer and raising possibility of sales.

## Cluster 1 & 3 in general have low spending scores, despite their income levels, 
## and generally the customers are above 40s. With those data, we could consider to 
## research and adding some brands that are popular among customers at those ages, 
## and running campaigns to target them with the right products.

## As we can see from the above data, cluster 2 have medium spending score, 
## in order to increase sales in this customer cluster, we could give them some promotions 
## to encourage them to buy more products.

## How to extract the members of a Clusters

cust[cust.KM$cluster==1,]
cust[cust.KM$cluster==2,]
cust[cust.KM$cluster==3,]
cust[cust.KM$cluster==4,]
cust[cust.KM$cluster==5,]

