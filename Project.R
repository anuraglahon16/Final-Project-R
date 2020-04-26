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



################################## Model Building #######################################
install.packages("randomForest")
library(randomForest)
sapply(hotels, class)
str(hotels)
hotels$reservation_status_date=as.integer(hotels$reservation_status_date)
hotels$total_of_special_requests= as.factor(hotels$total_of_special_requests)
hotels$is_repeated_guest=as.factor(hotels$is_repeated_guest)

#Removing country and column
hotels=hotels[-24]
hotels=hotels[-14]
dim(hotels)

#splitting dataset into training and testing data
set.seed(0)
train <- sample(nrow(hotels), 0.8*nrow(hotels), replace = FALSE)
trainSet <- hotels[train,]
testSet <- hotels[-train,]
summary(trainSet)
summary(testSet)

#using random forest package
model=randomForest(total_of_special_requests ~ is_canceled + hotel + lead_time + stays_in_weekend_nights+stays_in_week_nights + market_segment + previous_cancellations+is_repeated_guest + adults + babies + deposit_type + booking_changes + days_in_waiting_list + customer_type, data = trainSet, importance = TRUE)
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
#plot
plot(model)
#tuning model
tuneRF(trainSet[,-27],trainSet[,27],stepFactor = 0.5,plot=TRUE,ntreeTry = 200,trace=TRUE,improve = 0.05)
#improved model
rf=randomForest(total_of_special_requests ~ is_canceled + hotel + lead_time + stays_in_weekend_nights+stays_in_week_nights + market_segment + previous_cancellations+is_repeated_guest + adults + babies + deposit_type + booking_changes + days_in_waiting_list + customer_type, data = trainSet,ntree=200,mtry=5,importance = TRUE,proximity=TRUE)
rf

#Ordinal Logistic Regression
install.packages("MASS")
library(MASS)
hotels$total_of_special_requests= as.ordered(hotels$total_of_special_requests)

#model1
m<- polr(total_of_special_requests ~ is_canceled + hotel + lead_time + stays_in_weekend_nights+stays_in_week_nights + market_segment + previous_cancellations+is_repeated_guest + adults + babies + deposit_type + booking_changes + days_in_waiting_list + customer_type, data = trainSet, Hess = TRUE)
summary(m)
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

#model2
m1<- polr(total_of_special_requests ~ is_canceled +hotel + lead_time + market_segment + is_repeated_guest + adults + babies + previous_cancellations + deposit_type + booking_changes + days_in_waiting_list + customer_type, data = trainSet, Hess = TRUE)
m1

