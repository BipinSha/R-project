# Conjoint Analysis on ISP Offerings
# Bundle option: 10GBs,12GBs,15GBs
# Days valid: 7days,10days,12days
# Free WhatsApp: Yes, No
# Free Youtube: Yes, No
# Price: 1000, 1500, 2000

## Combinations 
## Combination 1 - 10GB, Days_Valid = 10 , Free Whatsapp = No,  Free YouTube = No,   Price = 1000 
## Combination 2 - 15GB, Days_Valid = 12 , Free Whatsapp = Yes, Free YouTube = No,   Price = 1000 
## Combination 3 - 10GB, Days_Valid = 7  , Free Whatsapp = Yes, Free YouTube = Yes,  Price = 1000 
## Combination 4 - 12GB, Days_Valid = 7  , Free Whatsapp = No,  Free YouTube = No,   Price = 1500 
## Combination 5 - 10GB, Days_Valid = 10 , Free Whatsapp = Yes, Free YouTube = No,   Price = 1500 
## Combination 6 - 15GB, Days_Valid = 10 , Free Whatsapp = No,  Free YouTube = Yes,  Price = 1500 
## Combination 7 - 15GB, Days_Valid = 7  , Free Whatsapp = Yes, Free YouTube = No,   Price = 2000 
## Combination 8 - 10GB, Days_Valid = 12 , Free Whatsapp = No,  Free YouTube = Yes,  Price = 2000 
## Combination 9 - 12GB, Days_Valid = 10 , Free Whatsapp = Yes, Free YouTube = Yes,  Price = 2000 


library(conjoint)
library(dplyr)
library(AlgDesign)
library(ggplot2)
library(knitr)
library(kableExtra)

setwd("F:/Henry/IIT Mumbai/Marketing Analytics/Conjoint Analyssi")
dir()
profile_data = read.csv("RankingData.csv")
nrow(profile_data)
names(profile_data)

combined_attributes=gen.factorial(c(3,3,2,2,3),varNames=c("Bundles","Days_valid","Free_Whatsapp","Free_Youtube","Price"),factors = "all")

combined_attributes<-combined_attributes %>%
  mutate(Bundles=factor(Bundles,labels=c("10GBs","12GBs","15GBs"),levels=c(1,2,3)),
         Days_valid=factor(Days_valid,labels=c("7days","10days","12days"),levels=c(1,2,3)),
         Free_Whatsapp=factor(Free_Whatsapp,labels = c("No","Yes"),levels=c(1,2)),
         Free_Youtube=factor(Free_Youtube,labels=c("No","Yes"),levels=c(1,2)),
         Price=factor(Price,labels=c("1000","1500","2000"),levels=c(1,2,3)))

levels=c("10GBs","12GBs","15GBs","7days","10days","12days","Yes","No","Yes","No","1000","1500","2000")
levels=data.frame(levels)

few_combinations = read.csv("Bundles.csv") 
head(few_combinations)


head(profile_data,10) %>%
  kable("html") %>%
  kable_styling(font_size=10) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

dev.off()


#####GGplot of important features
Importance = data.frame(Feature = c("Bundles","Days_valid","Free_Whatsapp","Free_Youtube","Price"), 
                        Importance = caImportance(y=profile_data[,2:10],x=few_combinations))


dev.off()

ggplot(data = Importance, aes(x = reorder(Feature,-Importance), y = Importance)) + 
  geom_bar(stat= "identity", fill = "skyblue2", width = 0.7) +
  ggtitle("Importance of different features") + xlab("")

## Which combination will the Customer prefer the most ?

util = caUtilities(y=profile_data[,2:10],x=few_combinations,z =levels)

util

bundle_utility=util[2:4]
valid_days=util[5:7]
Free.Whatsapp=util[8:9]
Free.Youtube=util[10:11]
price=util[12:14]

names(bundle_utility)=c("10GBs","12GBs","15GBs")
names(valid_days)=c("7days","10days","12days")
names(Free.Whatsapp)=c("No","Yes")
names(Free.Youtube)=c("No","Yes")
names(price)=c("1000","1500","2000")

barplot(bundle_utility,col="red",main="Bundle type")
barplot(valid_days,col="brown",main="Valid days")
barplot(Free.Whatsapp,col="grey",main="Free WhatsApp")
barplot(Free.Youtube,col="orange",main="Free Youtube")
barplot(price,col="yellow",main="Price tags")

