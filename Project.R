getwd()
setwd("C:/Users/anami/Desktop/MS BA/Stats for BA/Group Project/Final-Project-R")
hotels=read.csv("hotel_bookings.csv")


########################### Exploratory Data Analysis ###################################
install.packages("funModeling") 
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("DataExplorer")
install.packages("ggplot2")
library(funModeling)
library(tidyverse)
library(Hmisc)
library(DataExplorer)
library(ggplot2)


plot_str(hotels)         #show data structure
introduce(hotels)        #show basic description
plot_intro(hotels)       #plot basic description
glimpse(hotels)          #show data overview
plot_missing(hotels)     #show missing data
describe(hotels)
summary(hotels)
df_status(hotels)
dim(hotels)


#Handling missing data
#Replacing  missing values in Children column from the corresponding Babies column
n <- length(hotels$children)
for (i in 1:n) {
  if (is.na(hotels$children[i]))
    hotels$children[i] <- hotels$babies[i]
}

#Replacing undefined as SC .Both means no meal package.
hotels <-fix(hotels)
hotels$meal <-replace(hotels$meal,hotels$meal=='Undefined','SC')
hotels$meal <- factor(hotels$meal)

#Defining mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Replacing Undefined with mode for market segment column
modeM <- getmode(hotels$market_segment)
modeM
hotels$market_segment <- replace(hotels$market_segment,hotels$market_segment=='Undefined',modeM)
hotels$market_segment <- factor(hotels$market_segment)

#Replacing Undefined with mode for distribution channel column
modeD <- getmode(hotels$distribution_channel)
modeD
hotels$distribution_channel <- replace(hotels$distribution_channel,hotels$distribution_channel=='Undefined',modeD)
hotels$distribution_channel <- factor(hotels$distribution_channel)

#Removing missing country rows
hotels$country[hotels$country=='NULL']= NA
sum(is.na(hotels$country))
hotels=hotels[!is.na(hotels$country),]
sum(is.na(hotels$country))

#Droping company column( 91% missing values)
install.packages("dplyr")
library(dplyr)
hotels=select(hotels, -company)

#checking final dataset
describe(hotels)
dim(hotels)


#Univariate stats
plot_num(hotels)         #plot histogram of all numeric data
plot_histogram(hotels)
plot_bar(hotels)         #barplot
plot_density(hotels)     #show density plot of all data
plot_correlation(hotels)
barplot(table(hotels$arrival_date_year))
barplot(table(hotels$arrival_date_month))
plot_boxplot(hotels)


#Bivariate stats
hotels$is_canceled=as.numeric(hotels$is_canceled)
#cancellations for both the hotels
ggplot(data = hotels,aes(is_canceled))+ geom_histogram(binwidth = 0.5, 
            col='black', fill='blue',alpha=0.4) + facet_wrap(~hotel)

#cancellation leadtime
ggplot(data=hotels,aes(lead_time))+ geom_histogram(binwidth = 0.5)
+facet_wrap(~is_canceled)

#stay duration for both the hotels
ggplot(data=hotels, aes(stays_in_weekend_nights + stays_in_week_nights))
+geom_density(col="red")+facet_wrap(~hotel)+theme_bw()

#Average daily rate for both hotels
ggplot(data=hotels,aes(x=adr,fill=hotel,color=hotel))+geom_histogram(aes(y=..density..),
      position = position_dodge(),binwidth=80)+geom_density(alpha=0.2)+
  labs(title = "Average Daily rate by Hotel", x = "Hotel Price(in Euro)",y = "Count")+ 
  scale_color_brewer(palette = "Paired") + theme_classic() + theme(legend.position = "top")

#Hotel preference by customer type
ggplot(data=hotels,aes(customer_type,fill=hotel)) +geom_bar(stat="count",position = position_dodge())




################################## Model Building #######################################
############### Model to predict total requests from customers ##########################
#1) using random forest package

install.packages("randomForest")
library(randomForest)
sapply(hotels, class)
str(hotels)
hotels$reservation_status_date=as.integer(hotels$reservation_status_date)
hotels$total_of_special_requests= as.factor(hotels$total_of_special_requests)
hotels$is_repeated_guest=as.factor(hotels$is_repeated_guest)
hotels$arrival_date_year=as.factor(hotels$arrival_date_year)
hotels$is_canceled=as.factor(hotels$is_canceled)

#Removing country and column
hotels=hotels[-24]
hotels=hotels[-14]
dim(hotels)

#splitting dataset into training and testing data
set.seed(0)
n=nrow(hotels)
shuffled=hotels[sample(n),]
trainSet=shuffled[1:round(0.8 * n),]
testSet = shuffled[(round(0.8 * n) + 1):n,]
summary(trainSet)
summary(testSet)

#model
model=randomForest(total_of_special_requests ~.,data = trainSet, importance = TRUE)
model
#prediction and confusion matrix for training data
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
p1=predict(model,trainSet)
confusionMatrix(p1,trainSet$total_of_special_requests)
#prediction and confusion matrix for testing data
p2=predict(model,testSet)
confusionMatrix(p2,testSet$total_of_special_requests)
#Error rate
plot(model)
#tuning model
tuneRF(trainSet[,-27],trainSet[,27],stepFactor = 0.5,plot=TRUE,ntreeTry = 200,trace=TRUE,improve = 0.05)
#improved model
#rf=randomForest(total_of_special_requests ~., data = trainSet,ntree=200,mtry=5,importance = TRUE,proximity=TRUE)
#rf
#number of nodes for the trees
hist(treesize(model),main = "No of nodes for the trees",col = "green")
#variable importance
varImpPlot(model)
varUsed(model)


#using Ordinal Logistic Regression
install.packages("MASS")
library(MASS)
hotels$total_of_special_requests= as.ordered(hotels$total_of_special_requests)

#model
m<- polr(total_of_special_requests ~ is_canceled + hotel + lead_time + stays_in_weekend_nights+stays_in_week_nights + market_segment + previous_cancellations+is_repeated_guest + adults + babies + deposit_type + booking_changes + days_in_waiting_list + customer_type, data = trainSet, Hess = TRUE)
summary(m)
#m<- polr(total_of_special_requests ~.-adr, data = trainSet, Hess = TRUE)
#p value
(ctable <- coef(summary(m)))
p=pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
(ctable <- cbind(ctable, "p value" = p))
#prediction
pred=predict(m,trainSet)
#confusion matrix and error for training data
(tab=table(pred,trainSet$total_of_special_requests))
1-sum(diag(tab))/sum(tab)
#confusion matrix and error for test data
pred1=predict(m,testSet)
(tab1=table(pred1,testSet$total_of_special_requests))
1-sum(diag(tab1))/sum(tab1)


##################### Model to predict booking cancellations #############################
# 1) using random forest package
str(trainSet)

model1=randomForest(is_canceled~., data=trainSet)
model1
#prediction and confusion matrix for training data
modpredTrain=predict(model1,trainSet)
confusionMatrix(modpredTrain,trainSet$is_canceled)
#prediction and confusion matrix for testing data
modpredTest=predict(model1,testSet)
confusionMatrix(modpredTest,testSet$is_canceled)
#Error rate
plot(model1)
#number of nodes for the trees
hist(treesize(model1),main = "No of nodes for the trees",col = "green")
#variable importance
varImpPlot(model1)
varUsed(model1)
