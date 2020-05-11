getwd()
setwd("C:/Users/anami/Desktop/MS BA/Stats for BA/Group Project/Final-Project-R")
hotels=read.csv("hotel_bookings.csv")


################################# MODEL BUILDING############################################
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



#################### Model to predict total requests from customers ##########################

###################################################1) using random forest package

install.packages("randomForest")
library(randomForest)
sapply(hotels, class)
str(hotels)
hotels$reservation_status_date=as.integer(hotels$reservation_status_date)
hotels$total_of_special_requests= as.factor(hotels$total_of_special_requests)
hotels$is_repeated_guest=as.factor(hotels$is_repeated_guest)
hotels$arrival_date_year=as.factor(hotels$arrival_date_year)
hotels$is_canceled=as.factor(hotels$is_canceled)

#Removing country column
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
tuneRF(trainSet[,-27],trainSet[,27],stepFactor = 0.5,plot=TRUE,ntreeTry = 300,trace=TRUE,improve = 0.05)

#improved model
rf=randomForest(total_of_special_requests ~., data = trainSet,ntree=300,mtry=5,importance = TRUE)
rf
p3=predict(rf,trainSet)
confusionMatrix(p3,trainSet$total_of_special_requests)




################################################# 2)using Ordinal Logistic Regression

install.packages("MASS")
library(MASS)
hotels$total_of_special_requests= as.ordered(hotels$total_of_special_requests)
str(hotels)
summary(hotels)

#model
m<- polr(total_of_special_requests ~ is_canceled + lead_time + stays_in_weekend_nights+stays_in_week_nights +is_repeated_guest + adults + babies + days_in_waiting_list +market_segment+ deposit_type+ customer_type, data = trainSet, Hess = TRUE)
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

# Check assumptions
install.packages("car")
library(car)
hotels$total_of_special_requests= as.numeric(hotels$total_of_special_requests)
vif(lm(total_of_special_requests ~ is_canceled + hotel + lead_time + stays_in_weekend_nights+stays_in_week_nights + previous_cancellations+is_repeated_guest + adults + babies + deposit_type + booking_changes + days_in_waiting_list + customer_type, data = trainSet))
install.packages("brant")
library(brant)
brant(m)
propOddsTest(m)
str(hotels)




###################################################3) Using logistic regression

hotels$total_of_special_requests=as.numeric(hotels$total_of_special_requests)
for (i in 1:118902) {
  if (hotels$total_of_special_requests[i]>1){
    hotels$total_of_special_requests[i]=(hotels$total_of_special_requests[i]= 1)
  }
}
hotels$total_of_special_requests= as.factor(hotels$total_of_special_requests)
hotels$total_of_special_requests
mod<- glm(total_of_special_requests ~ is_canceled + lead_time + stays_in_weekend_nights+stays_in_week_nights +is_repeated_guest + adults + babies + days_in_waiting_list +market_segment+ deposit_type+ customer_type, data = trainSet, family='binomial')
summary(mod)
exp(mod$coefficients)

#cross validation
install.packages("boot")
library(boot)
set.seed(0) 
cv_results=cv.glm(na.omit(trainSet), mod, K=10)
cv_results$delta

#Pridiction and confusion matrix for training data
prediction=predict(mod,trainSet, type='response')
prediction1= ifelse(prediction>0.5,1,0)
tab1= table(prediction1,trainSet$total_of_special_requests)
tab1
1-sum(diag(tab1))/sum(tab1)

#Pridiction and confusion matrix for test data
prediction2=predict(mod,testSet, type='response')
prediction3= ifelse(prediction2>0.5,1,0)
tab2= table(prediction3,testSet$total_of_special_requests)
tab2
1-sum(diag(tab2))/sum(tab2)





########################## Regularization for special Requests ################################

colnames(hotels)
str(hotels)
x_train1 = model.matrix(trainSet$total_of_special_requests~.-reservation_status -arrival_date_year -arrival_date_month, trainSet)[,-27]
y_train1 = trainSet$total_of_special_requests
x_test1=model.matrix(testSet$total_of_special_requests~.-reservation_status -arrival_date_year -arrival_date_month, testSet)[,-27]
y_test1=testSet$total_of_special_requests

################################################ Lasso Regression

cv.out <- cv.glmnet(x_train1,y_train1,alpha=1,family="binomial",type.measure = "mse" )
cv.out
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#get train dataset
lasso_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("No Request",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "Request"
lasso_predict <- ifelse(lasso_predict=="Request",1,0)
mean(lasso_predict==trainSet$total_of_special_requests)
tab= table(lasso_predict, trainSet$total_of_special_requests)
tab
1- sum(diag(tab))/sum(tab)


#get test data
lasso_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("No Request",nrow(testSet))
lasso_predict[lasso_prob>.5] <- "Request"
lasso_predict <- ifelse(lasso_predict=="Request",1,0)
mean(lasso_predict==testSet$total_of_special_requests)
# confusion matrix
tab1= table(lasso_predict, testSet$total_of_special_requests)
tab
1- sum(diag(tab1))/sum(tab1)



######################################################## Ridge regression

cv.out <- cv.glmnet(x_train1,y_train1,alpha=0,family="binomial",type.measure = "mse" )
cv.out
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)


#get train dataset
ridge_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("No Request",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "Request"
ridge_predict <- ifelse(ridge_predict=="Request",1,0)
mean(ridge_predict==trainSet$total_of_special_requests)
tab= table(ridge_predict, trainSet$total_of_special_requests)
tab
1- sum(diag(tab))/sum(tab)


#get test data
ridge_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("No Request",nrow(testSet))
ridge_predict[ridge_prob>.5] <- "Request"
ridge_predict <- ifelse(ridge_predict=="Request",1,0)
mean(ridge_predict==testSet$total_of_special_requests)
tab1= table(ridge_predict, testSet$total_of_special_requests)
tab1
1- sum(diag(tab1))/sum(tab1)

