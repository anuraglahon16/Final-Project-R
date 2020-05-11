getwd()
setwd('C:/Users/anura/Downloads')
hotels=read.csv("hotel_bookings.csv")


########################## MODEL BUILDING for booking cancellations #########################


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
head(hotels)

hotels$reservation_status_date <- NULL

dim(hotels)


#===================================splitting dataset into training and testing data
set.seed(0)
n=nrow(hotels)
shuffled=hotels[sample(n),]
trainSet=shuffled[1:round(0.8 * n),]
testSet = shuffled[(round(0.8 * n) + 1):n,]
summary(trainSet)
summary(testSet)

# 95512 rows
dim(trainSet)
#23878 rows
dim(testSet)

################################################### Using Logistic regression 

model1 <- glm(is_canceled ~ hotel + lead_time + arrival_date_month + children +
                market_segment + is_repeated_guest + adults + babies +
                previous_cancellations +
                deposit_type + booking_changes  +
                reserved_room_type + adr + days_in_waiting_list + customer_type +
                total_of_special_requests, 
              data = trainSet , family = "binomial")
summary(model1)

train_pred <-predict(model1, trainSet,type = 'response')
library(knitr)
library(ROCR)
install.packages("verification")
library(verification)
pred <- prediction(train_pred,trainSet$is_canceled)
perform <- performance(pred,"acc")
max <- which.max(slot(perform,"y.values")[[1]])
prob <- slot(perform,"x.values")[[1]][max]
prob

train_pred1 <- ifelse(train_pred >  prob, 1,0)
mean(trainSet$is_canceled == train_pred1)

tble <- table(Actual = trainSet$is_canceled,Predicted = train_pred1 );tble


test_pred <-predict(model1, testSet,type = 'response')

test_pred1 <- ifelse(test_pred > prob , 1,0)
#test accuracy 80.49%
mean(testSet$is_canceled == test_pred1)
tble1 <- table(Actual = testSet$is_canceled,Predicted = test_pred1 );tble1

TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble[1,])
P <- sum(tble[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble1))/sum(tble1)
roc.plot(
  testSet$is_canceled,
  test_pred,
  threshold = seq(0,max(test_pred),0.01)
)
#auc=83.52
pred1 <- prediction(test_pred,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
#step
stepHotel<-stepAIC(model1)
stepHotel$anova
#Checking Assumptions
install.packages("car")
library(car)

vif(model1)
AIC(model1)
BIC(model1)
plot(model1)

#residualPlots(model1)
durbinWatsonTest(model1)




##################################################### using random forest package

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


#model
model1=randomForest(is_canceled~.-reservation_status -arrival_date_year -arrival_date_month,data=trainSet)
model1

#prediction and confusion matrix for training data
modpredTrain=predict(model1,trainSet)
confusionMatrix(modpredTrain,trainSet$is_canceled)
library(caret)
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



#------------------------Running Different Models for selecting features---------------------------------------------

#For ridge and lasso regression, we will be using the `glmnet` library.  
#Remember, we need to tune our hyperparameter, $\lambda$ to find the 'best' ridge or lasso model to implement.  
library(glmnet)
x_train = model.matrix(trainSet$is_canceled~., trainSet)[,-1]
y_train = trainSet$is_canceled
x_test=model.matrix(testSet$is_canceled~., testSet)[,-1]
y_test=testSet$is_canceled
#Ridge regression
#Then we would want to build in a cross-validation process to choose our 'best' $\lambda$.  We can do this using `cv.glmnet,` 
cv_ridge = cv.glmnet(x_train, y_train, alpha = 0)
#ridge regression is performed by default  using `alpha = 0`
cv_ridge$lambda.min
#We see that the cross-validated model with a $\lambda = 0.048 provides the optimal model in terms of minimizing MSE
predict(cv_ridge, type="coefficients", s=0.04829162)
dim(hotels)
#min value of lambda
lambda_min <- cv_ridge$lambda.min
#best value of lambda
lambda_1se <- cv_ridge$lambda.1se
#regression coefficients
coef(cv_ridge,s=lambda_1se)

#Predicting on training data 100% with 30 features
#predict class, type="class"
ridge_prob <- predict(cv_ridge,newx = x_train,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==trainSet$is_canceled)

#Testing on test data 100 % with 30 features
ridge_prob <- predict(cv_ridge,newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(testSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==testSet$is_canceled)



#Lasso
cv_lasso = cv.glmnet(x_train, y_train, alpha = 1)
bestlam = cv_ridge$lambda.min
predict(cv_lasso, type="coefficients", s=bestlam)

#min value of lambda
lambda_min <- cv_lasso$lambda.min
#best value of lambda
lambda_1se <- cv_lasso$lambda.1se
#regression coefficients
coef(cv_lasso,s=lambda_1se)


#Predicting on training data 100% with 30 features
#predict class, type="class"
lasso_prob <- predict(cv_lasso,newx = x_train,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==trainSet$is_canceled)

#Testing on test data 63.08 %
lasso_prob <- predict(cv_lasso,newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(testSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==testSet$is_canceled)



#=========================================Regression for some variable with Logistic Regression 83.61 auc=============================================
x_train1 = model.matrix(trainSet$is_canceled~hotel + lead_time + arrival_date_month +arrival_date_year+ children +meal+
                          market_segment + is_repeated_guest + adults + babies +
                          previous_cancellations +
                          deposit_type + booking_changes  +
                          reserved_room_type + adr + days_in_waiting_list + customer_type +
                          total_of_special_requests, trainSet)[,-1]
y_train1 = trainSet$is_canceled
x_test1=model.matrix(testSet$is_canceled~hotel + lead_time + arrival_date_month +arrival_date_year+ children +meal+
                       market_segment + is_repeated_guest + adults + babies +
                       previous_cancellations +
                       deposit_type + booking_changes  +
                       reserved_room_type + adr + days_in_waiting_list + customer_type +
                       total_of_special_requests, testSet)[,-1]
y_test1=testSet$is_canceled


#=======================================Ridge Regression for cancelled hotel=======================================
#it will take time to run
#alpha0.fit <- cv.glmnet(x_train1,y_train1,type.measure = 'mse',alpha=0,family='gaussian')
#alpha0.predicted<-predict(alpha0.fit,s=alpha0.fit$lambda.1se,newx=x_test1)
#mean((y_test1-alpha0.predicted)^2)
#=======================================Lasso Regression for cancelled hotel=======================================
#alpha1.fit <- cv.glmnet(x_train1,y_train1,type.measure = 'mse',alpha=1,family='gaussian')#
#alpha1.predicted<-predict(alpha1.fit,s=alpha1.fit$lambda.1se,newx=x_test1)
#mean((y_test1-alpha1.predicted)^2)

#==============================================Regression of Lasso and Ridge to calculate accuracy=====================


#===========================Regression with Lasso and Ridge Regression with positive coefficient and without reservation status 76.87% ===========================================
x_train1 = model.matrix(trainSet$is_canceled~lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number+
                          arrival_date_day_of_month + stays_in_weekend_nights + stays_in_week_nights+
                          adults + children + babies + meal + distribution_channel+is_repeated_guest +
                          previous_cancellations + reserved_room_type + assigned_room_type+
                          deposit_type + customer_type + adr , trainSet)[,-1]
y_train1 = trainSet$is_canceled
x_test1=model.matrix(testSet$is_canceled~lead_time + arrival_date_year + arrival_date_month + arrival_date_week_number+
                       arrival_date_day_of_month + stays_in_weekend_nights + stays_in_week_nights+
                       adults + children + babies + meal + distribution_channel+is_repeated_guest +
                       previous_cancellations + reserved_room_type + assigned_room_type+
                       deposit_type + customer_type + adr , testSet)[,-1]
y_test1=testSet$is_canceled
#-------------------------Lasso Regression-----------------------
cv.out <- cv.glmnet(x_train1,y_train1,alpha=1,family="binomial",type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#Predict on training data set 76.71% accuracy
lasso_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==trainSet$is_canceled)

#Predicting of testing or new data set
#get test data
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(testSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==testSet$is_canceled)

tble1 <- table(Actual = testSet$is_canceled,Predicted = lasso_predict );tble1

TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble1[1,])
P <- sum(tble1[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)


library(knitr)
library(ROCR)
library(verification)
kable(df)
1 - sum(diag(tble1))/sum(tble1)
roc.plot(
  testSet$is_canceled,
  lasso_predict,
  threshold = seq(0,max(lasso_predict),0.01)
)

pred1 <- prediction(lasso_predict,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
#---------------------Ridge regression-----------------------------------------------
cv.out <- cv.glmnet(x_train1,y_train1,alpha=0,family="binomial",type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#Predicting on training data 75.81 %
#predict class, type="class"
ridge_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==trainSet$is_canceled)



#Predicting on testing data
#predict class, type="class"
ridge_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(testSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==testSet$is_canceled)

tble1 <- table(Actual = testSet$is_canceled,Predicted = ridge_predict );tble1

TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble1[1,])
P <- sum(tble1[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble1))/sum(tble1)
roc.plot(
  testSet$is_canceled,
  ridge_predict,
  threshold = seq(0,max(ridge_predict),0.01)
)

pred1 <- prediction(ridge_predict,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc


#============================================Regression to test Prediction 80.3% ==============================
x_train1 = model.matrix(trainSet$is_canceled~lead_time + country + deposit_type + adr + arrival_date_day_of_month +
                          total_of_special_requests  + stays_in_weekend_nights + previous_cancellations+
                          arrival_date_year+ booking_changes + required_car_parking_spaces +
                          market_segment, trainSet)[,-1]
y_train1 = trainSet$is_canceled
x_test1=model.matrix(testSet$is_canceled~lead_time + country + deposit_type + adr + arrival_date_day_of_month +
                       total_of_special_requests  + stays_in_weekend_nights + previous_cancellations+
                       arrival_date_year+ booking_changes + required_car_parking_spaces +
                       market_segment , testSet)[,-1]
y_test1=testSet$is_canceled
#-------------------------Lasso Regression-----------------------
cv.out <- cv.glmnet(x_train1,y_train1,alpha=1,family="binomial",type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#Predicting on training data 80.07%
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==trainSet$is_canceled)


#Predicting on testing data set
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(testSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==testSet$is_canceled)

#---------------------Ridge regression-----------------------------------------------
cv.out <- cv.glmnet(x_train1,y_train1,alpha=0,family="binomial",type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#Predicting on training data set 79.73%
#predict class, type="class"
ridge_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==trainSet$is_canceled)


#Predicting on testing data set
#predict class, type="class"
ridge_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(testSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==testSet$is_canceled)



#============================Regression to test Prediction same above but included stays_in_week_nights the best prediction 80.57%==============================
x_train1 = model.matrix(trainSet$is_canceled~lead_time + country + deposit_type + adr + arrival_date_day_of_month +
                          total_of_special_requests  + stays_in_weekend_nights +stays_in_week_nights + previous_cancellations+
                          arrival_date_year+ booking_changes + required_car_parking_spaces +
                          market_segment, trainSet)[,-1]
y_train1 = trainSet$is_canceled
x_test1=model.matrix(testSet$is_canceled~lead_time + country + deposit_type + adr + arrival_date_day_of_month +
                       total_of_special_requests  + stays_in_weekend_nights + stays_in_week_nights +previous_cancellations+
                       arrival_date_year+ booking_changes + required_car_parking_spaces +
                       market_segment , testSet)[,-1]
y_test1=testSet$is_canceled
#-------------------------Lasso Regression-----------------------
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

#Predicting on training data set 80.17 %
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==trainSet$is_canceled)

#Cross Validation on Training data


#Predicting on testing data set
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(testSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==testSet$is_canceled)
#confusion matrix
table(pred=lasso_predict,true=testSet$is_canceled)

tble1 <- table(Actual = testSet$is_canceled,Predicted = lasso_predict );tble1

TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble1[1,])
P <- sum(tble1[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble1))/sum(tble1)
roc.plot(
  testSet$is_canceled,
  lasso_predict,
  threshold = seq(0,max(lasso_predict),0.01)
)

pred1 <- prediction(lasso_predict,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
#---------------------Ridge regression-----------------------------------------------
cv.out <- cv.glmnet(x_train1,y_train1,alpha=0,family="binomial",type.measure = "mse" )
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#Predicting on training dataset 79.81%
ridge_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==trainSet$is_canceled)

#Cross Validation (K Fold) on Training data
# Prepare data set 


library(glmnet)

# Run cross-validation
mod_cv <- cv.glmnet(x=x_train1, y=y_train1, family='binomial')

mod_cv$lambda.1se

coef(mod_cv, mod_cv$lambda.1se)

#get test data
#predict class, type="class"
ridge_prob <- predict(cv.out,newx = x_test1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(testSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==testSet$is_canceled)


#Confusion Matrix

tble1 <- table(Actual = testSet$is_canceled,Predicted = ridge_predict );tble1

TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble1[1,])
P <- sum(tble1[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble1))/sum(tble1)
roc.plot(
  testSet$is_canceled,
  ridge_predict,
  threshold = seq(0,max(ridge_predict),0.01)
)

pred1 <- prediction(lasso_predict,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc

#==========================================================================================================================================





