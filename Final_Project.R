#Seeing the current directory
getwd()
#Setting the current directory
setwd('C:/Users/anura/Downloads')
#Required packages
library(funModeling)
library(tidyverse)
library(Hmisc)
library(DataExplorer)
#Load libraries
library(dplyr)
library(caret)
library(lattice)

library(magrittr)
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(gridExtra)) 
#reading data
hotels <- read.csv('hotel_bookings.csv')
#First 6 rows
head(hotels)
#Dimension of the hotel data set
dim(hotels)
#Summary of the hotel data set
summary(hotels)

H=hotels
dim(hotels)
#Dropping the agent column
#hotels <-hotels[ ,-c(24)]
is.na(hotels$agent)
#Missing value of hotel data set
plot_missing(hotels)
#There are 4 missing values in column children
#There are many missing values in categorical data we have to clean it
sum(is.na(hotels))
#Structure of the hotel data set
str(hotels)

# ---------------------Missing VALUE REPLACEMENT-----------------------------------

#Replacing  missing values in Children column from the corresponding Babies column
n <- length(hotels$children)
for (i in 1:n) {
  if (is.na(hotels$children[i]))
    hotels$children[i] <- hotels$babies[i]
}
#Get MOde
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Replacing undefined as SC .Both means no meal package
hotels <-fix(hotels)
hotels$meal <-replace(hotels$meal,hotels$meal=='Undefined','SC')
#We are dropping the levels as we have replaced Undefined with SC
hotels$meal <- factor(hotels$meal)
unique(hotels$meal)
hotels
#Replacing Undefinded with mode 
#Function to calculate mode
modeM <- getmode(hotels$market_segment)
modeM
#Replacing Undefined with mode
hotels$market_segment <- replace(hotels$market_segment,hotels$market_segment=='Undefined',modeM)
#We are droping the levels as we have replaced Undefined with mode
hotels$market_segment <- factor(hotels$market_segment)
#Checking unique value in market_segment
unique(hotels$market_segment)

#Replacing Undefinded with mode 
#Function to calculate mode
modeD <- getmode(hotels$distribution_channel)
modeD
#Replacing Undefined with mode
hotels$distribution_channel <- replace(hotels$distribution_channel,hotels$distribution_channel=='Undefined',modeD)
#We are droping the levels as we have replaced Undefined with mode
hotels$distribution_channel <- factor(hotels$distribution_channel)
#Checking unique value in market_segment
unique(hotels$distribution_channel)

#Correlation of numeric data
my_num_data <- hotels[, sapply(hotels, is.numeric)]
cor(my_num_data, use = "complete.obs", method = "pearson")


#checking missing values in quantitve columns
sapply(hotels, function(x) sum(is.na(x)))
#Missing value of quantitive hotel data set
plot_missing(hotels)
#describeBy function
library(psych)
psych::describe(hotels)
describeBy(hotels, hotels$hotel)

#--------------------checking our data after cleaning-----------------------------------------------

#Good way to see the dataframe dividing categorical and qualntitve features

library(skimr)

skim(hotels)




#--------------------------------------------------EDA---------------------------------------------------#

#Hotels
ggplot(hotels,aes(x=factor(hotel))) +
  geom_bar(col ="black",fill="#993333",alpha=0.5) +
  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_x_discrete("Hotel") +
  scale_y_continuous("Count")


# Number of arrival Date by Month
d <- hotels %>% 
  group_by(arrival_date_month) %>%
  count() %>%
  arrange(match(arrival_date_month,month.name))
d <- data.frame(ArrivalDateMonth = d$arrival_date_month,N =d$n);
d
# Graph of Number of arrival Date by Month
ggplot(hotels,aes(factor(arrival_date_month,levels=month.name))) +
  geom_bar(col ="black",fill="#993333",alpha=0.5) +
  theme(axis.text.x = element_text(face="bold", size=8, angle=30)) +
  scale_y_continuous("Count",limits = c(0,15000),breaks=seq(0,15000,by=1500)) +
  scale_x_discrete("Month")

#NUmber of city hotel and Resort Hotel cancelled or not cancelled 
ggplot(data = hotels,aes(factor(is_canceled)))+
  geom_bar( col='black', fill="#993333", alpha = 0.5) +
  facet_wrap(~hotel) +
  scale_x_discrete("Canceled",labels = c("No","Yes")) +
  scale_y_continuous("Count",limits = c(0,50000),breaks=seq(0,47222,by=5000))  +
  theme(axis.text.x = element_text(face="bold", size=10))

#Canceled and Lead time
ggplot(data = hotels, aes(x = factor(is_canceled), y = lead_time  )) + 
  geom_boxplot(col='black', fill="#993333", alpha = 0.5) +
  theme(axis.text.x = element_text(face="bold", size=10)) +
  scale_y_continuous("Lead Time",limits = c(0,800),breaks=seq(0,800,by=100)) +
  scale_x_discrete("Canceled",labels = c("No","Yes"))

#Year of arrival
barplot(table(hotels$arrival_date_year), col='orange')


#May and October have  the highest waiting times; these months represent the times right before and after peak reservation months (respectively)
ggplot(hotels, aes(x=arrival_date_month, y=days_in_waiting_list, group=1)) + stat_summary(fun="mean", geom="line", col="navy") + 
  ggtitle("Average Days on Waiting List by Arrival Month") + ylab("Waiting List: Average Days") + xlab("Month") +
  theme(axis.text.x=element_text(angle=40))


#The number of children included on reservations peaks in August, summer vacation season
hotels %>% group_by(arrival_date_month) %>% summarize(MEANADULTS=mean(adults)) -> ADULTS
hotels %>% group_by(arrival_date_month) %>% summarize(MEANCHILDREN=mean(children)) ->CHILDREN
ADULTS<-data.frame(ADULTS)
CHILDREN<-data.frame(CHILDREN)
ADULTSCHILDREN<-merge(ADULTS, CHILDREN, by="arrival_date_month")

ggplot(data=ADULTSCHILDREN) + 
  geom_line(aes(y=MEANADULTS, x=arrival_date_month, group=1, col="Adults")) + 
  geom_line(aes(y=MEANCHILDREN, x=arrival_date_month, group=1, col="Children")) +
  ggtitle("Average Adults and Children Reserved for by Arrival Month") + xlab("Month") + ylab("Average Days") +
  theme(axis.text.x=element_text(angle=40))

#Online market segment's cancellation is more
hotels%>% group_by(hotels$market_segment)  %>% summarise(length(is_canceled))

#City Hotel Cancellation is more
hotels %>% group_by(hotels$hotel)  %>% summarise(length(is_canceled))

#Couples booking cancellation is more
hotels %>% group_by(hotels$adults)  %>% summarise(length(is_canceled))

#'A' type room cancellation is higher
hotels %>% group_by(hotels$reserved_room_type)  %>% summarise(length(is_canceled))

##---------Few time Series EDA-------------
#Reservation status date by year
ts<- hotels%>%group_by(reservation_status_date)%>%summarise(n=n())
ts$reservation_status_date <- as.Date(ts$reservation_status_date)
ggplot(ts, aes(reservation_status_date, n)) + geom_line()

#Time Series Analysis
ggplot(ts, aes(reservation_status_date, n)) + geom_line()

ts <- ts %>% filter(!is.na(n))
ts
# Frequency is set with 365 because it's daily
components <- stl(ts(ts$n, frequency=365), 'periodic')
# seasonal, trend, remainder
plot(components)

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(hotels, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

#----------------------------------- Questions to Answer -------------------------------------------

#Q1.Are the Guest with children need parking space then guest with no children?(Multivariate)

hotel_stays <- hotels %>%
  filter(is_canceled == 0) %>%
  mutate(
    children = case_when(
      children + babies > 0 ~ "children",
      TRUE ~ "none"
    ),
    required_car_parking_spaces = case_when(
      required_car_parking_spaces > 0 ~ "parking",
      TRUE ~ "none"
    )
  )#%>%
  #select(-is_canceled, -reservation_status, -babies)



hotel_stays

hotel_stays %>%
  count(hotel, required_car_parking_spaces, children) %>%
  group_by(hotel, children) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(required_car_parking_spaces, proportion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2) +
  labs(
    x = NULL,
    y = "Proportion of hotel stays",
    fill = NULL
  )

# 2. WHich customer type pay more in  the hotels (this is for not cancelled one)?
#library(GGally)
#hotel_stays %>%
  #select(
    #hotel, adr,
    #customer_type,
    #total_of_special_requests
#  ) %>%
 # ggpairs(mapping = aes(color = customer_type))

# 3.Which country the guest come from (for non cancelled)? Need to do some work not able to draw the map by plotly
country <- hotels %>%
                filter(is_canceled==0)%>%
                count(country)
                
country


#-------------------------------------Plotly Interactive Graphs---------------------------
# Load the plotly package
library(plotly)

# Store the scatterplot of total_of_special_request vs.previous_bookings_not_canceled in 2016
scatter <- hotels %>%
  filter(arrival_date_year == '2016') %>%
  ggplot(aes(x = total_of_special_requests, y = previous_bookings_not_canceled)) +
  geom_point(alpha = 0.3)

# Convert the scatterplot to a plotly graphic
ggplotly(scatter)

#Histogram of lead time
# Create a histogram with bins of width 10 between 0 and 100
hotels %>%
  plot_ly(x = ~lead_time) %>%
  add_histogram(xbins = list(start=0,end=100, size=10),color=I('#111e6c'))

#Bar Plot for meal
# Create a frequency for meal
meal_table <- hotels %>%
  count(meal)

# Reorder the bars for meal by n
meal_table %>%
  mutate(meal = fct_reorder(meal, n, .desc = TRUE)) %>%
  plot_ly(x = ~meal, y = ~n) %>% 
  add_bars()  
meal_table

# Create a frequency for country
country_table <- hotels %>%
  count(country)
#Bar Plot for country
country_table %>%
  mutate(country = fct_reorder(country, n, .desc = TRUE)) %>%
  plot_ly(x = ~country, y = ~n) %>% 
  add_bars()  
country_table

# Create a scatter plot of days in waiting against lead time
hotels %>%
  plot_ly(x = ~days_in_waiting_list, y = ~lead_time) %>%
  add_markers(marker= list(opacity=0.2))


# Filter out the Reservation status and depodit type
Dep_Reserv<- hotels%>%
  filter(arrival_date_year == 2016 | arrival_date_year==2017 | arrival_date_year==2015)

#Create a stacked bar chart of Reservation type by Deposit type
Dep_Reserv %>%
  count(deposit_type, reservation_status) %>%
  plot_ly(x = ~deposit_type, y = ~n, color = ~reservation_status) %>%
  add_bars() %>%
  layout(barmode = "stack")

# Made a table
WaitingReserved<- hotels%>%
  filter(arrival_date_year == 2016 | arrival_date_year==2017 | arrival_date_year==2015)

# Create boxplots of days_in_waiting_list by reserved_room_type for above data
WaitingReserved %>%
  plot_ly(x = ~days_in_waiting_list, y =  ~reserved_room_type,hover_info='text',text=~paste("Days in waiting:",days_in_waiting_list,"Reserved room type:",reserved_room_type)) %>%
  add_boxplot()

# Use color to add is_cancelled as a third variable
hotels%>%
  plot_ly(x = ~days_in_waiting_list, y = ~previous_bookings_not_canceled, color = ~hotel) %>%
  add_markers(colors = "Dark2",marker= list(opacity=0.2))

# Polish the scatterplot by transforming the x-axis and labeling both axes


hotels%>%
  plot_ly(x = ~total_of_special_requests, y = ~adr) %>%
  add_lines() %>%
  layout(paper_bgcolor = "#ebebeb", xaxis = list(showgrid = FALSE))




#-------------------------------------Rough checking Initially ------------------------
library(ResourceSelection)
set.seed(1234)

model <- glm(is_canceled ~ hotel + lead_time + arrival_date_month + children +
                   market_segment + is_repeated_guest + adults + babies +
                   previous_cancellations +
                   deposit_type + booking_changes  +
                   reserved_room_type + adr + days_in_waiting_list + customer_type +
                   total_of_special_requests, 
                 data = hotels , family = "binomial")
summary(model)
anova(model)
hoslem.test(model$y, model$fitted)
# Compute the confidence intervals for model
#confint(model)




#---------------------------------------------Splitting the training and testing dataset ------------------------------------------------
head(hotels)

hotels$reservation_status_date <- NULL

dim(hotels)
skim(hotels)

#splitting dataset into training and testing data
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

#----------------------------------------------------Logistic Model 1 ------------------------------------------------------------------------

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
library(car)

vif(model1)
AIC(model1)
BIC(model1)
#plot(model1)

#residualPlots(model1)
durbinWatsonTest(model1)


#================================================Logistic Model 2==============================================================

model2 <- glm(is_canceled ~ hotel + lead_time + arrival_date_month +arrival_date_year+ children +meal+
                market_segment + is_repeated_guest + adults + babies +
                previous_cancellations +
                deposit_type + booking_changes  +
                reserved_room_type + adr + days_in_waiting_list + customer_type +
                total_of_special_requests, 
              data = trainSet , family = "binomial")
summary(model2)

train_pred2 <-predict(model2, trainSet,type = 'response')

pred1 <- prediction(train_pred2,trainSet$is_canceled)
perform2 <- performance(pred1,"acc")
max1 <- which.max(slot(perform2,"y.values")[[1]])
prob1 <- slot(perform2,"x.values")[[1]][max1]
prob1

train_pred2 <- ifelse(train_pred2 >  prob1, 1,0)
mean(trainSet$is_canceled == train_pred2)

tble2 <- table(Actual = trainSet$is_canceled,Predicted = train_pred2);tble2


test_pred2 <-predict(model2, testSet,type = 'response')

test_pred3 <- ifelse(test_pred2 > prob1 , 1,0)
#test accuracy 80.43%
mean(testSet$is_canceled == test_pred3)
tble11 <- table(Actual = testSet$is_canceled,Predicted = test_pred3 );tble11

TN <- tble11[1,1]
FN <- tble11[2,1]
FP <- tble11[1,2]
TP <- tble11[2,2]
N <- sum(tble2[1,])
P <- sum(tble2[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble11))/sum(tble11)
roc.plot(
  testSet$is_canceled,
  test_pred,
  threshold = seq(0,max(test_pred),0.01)
)
#auc=0.83
pred2 <- prediction(test_pred2,testSet$is_canceled)
auc <- performance(pred2,"auc")
auc <- unlist(slot(auc,"y.values"))
auc
#=============================================Logistic Regression Model 3===================================================
model3 <- glm(is_canceled ~ lead_time + arrival_date_month +arrival_date_year+children +meal+
                market_segment + is_repeated_guest + adults + babies + distribution_channel+
                previous_cancellations +
                deposit_type  +
                reserved_room_type + adr + days_in_waiting_list + customer_type, 
              data = trainSet , family = "binomial")
summary(model3)

train_pred2 <-predict(model3, trainSet,type = 'response')

pred1 <- prediction(train_pred2,trainSet$is_canceled)
perform2 <- performance(pred1,"acc")
max1 <- which.max(slot(perform2,"y.values")[[1]])
prob1 <- slot(perform2,"x.values")[[1]][max1]
prob1

train_pred2 <- ifelse(train_pred2 >  prob1, 1,0)
mean(trainSet$is_canceled == train_pred2)

tble2 <- table(Actual = trainSet$is_canceled,Predicted = train_pred2);tble2


test_pred2 <-predict(model2, testSet,type = 'response')

test_pred3 <- ifelse(test_pred2 > prob1 , 1,0)
#test accuracy 80.30
mean(testSet$is_canceled == test_pred3)
tble11 <- table(Actual = testSet$is_canceled,Predicted = test_pred3 );tble11

TN <- tble11[1,1]
FN <- tble11[2,1]
FP <- tble11[1,2]
TP <- tble11[2,2]
N <- sum(tble2[1,])
P <- sum(tble2[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)
1 - sum(diag(tble11))/sum(tble11)
roc.plot(
  testSet$is_canceled,
  test_pred,
  threshold = seq(0,max(test_pred),0.01)
)
#auc=0.8361
pred2 <- prediction(test_pred2,testSet$is_canceled)
auc <- performance(pred2,"auc")
auc <- unlist(slot(auc,"y.values"))
auc








#----------Inspecting our Hotels Dataset ----------------------------

str(hotels)
view(hotels)
glimpse(hotels)



#------------------------Running Different Models for selecting features---------------------------------------------
#drop company
#hotels=hotels[-24]
#drop country
hotels=hotels[-14]

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
#predict class, type=”class”
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
#predict class, type=”class”
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






#elastic net
cv_en = cv.glmnet(x, y, alpha = 0.5)
bestlam = cv_en$lambda.min
predict(cv_en, type="coefficients", s=bestlam)



#=======================================Ridge Regression for cancelled hotel=======================================
alpha0.fit <- cv.glmnet(x_train,y_train,type.measure = 'mse',alpha=0,family='gaussian')
alpha0.predicted<-predict(alpha0.fit,s=alpha0.fit$lambda.1se,newx=x_test)
mean((y_test-alpha0.predicted)^2)
#=======================================Lasso Regression for cancelled hotel=======================================
alpha1.fit <- cv.glmnet(x_train,y_train,type.measure = 'mse',alpha=1,family='gaussian')
alpha1.predicted<-predict(alpha1.fit,s=alpha1.fit$lambda.1se,newx=x_test)
mean((y_test-alpha1.predicted)^2)
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
alpha0.fit <- cv.glmnet(x_train1,y_train1,type.measure = 'mse',alpha=0,family='gaussian')
alpha0.predicted<-predict(alpha0.fit,s=alpha0.fit$lambda.1se,newx=x_test1)
mean((y_test1-alpha0.predicted)^2)
#=======================================Lasso Regression for cancelled hotel=======================================
alpha1.fit <- cv.glmnet(x_train1,y_train1,type.measure = 'mse',alpha=1,family='gaussian')
alpha1.predicted<-predict(alpha1.fit,s=alpha1.fit$lambda.1se,newx=x_test1)
mean((y_test1-alpha1.predicted)^2)

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
#predict class, type=”class”
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
#predict class, type=”class”
ridge_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==trainSet$is_canceled)



#Predicting on testing data
#predict class, type=”class”
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
#predict class, type=”class”
lasso_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==trainSet$is_canceled)


#Predicting on testing data set
#predict class, type=”class”
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
#predict class, type=”class”
ridge_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
ridge_predict <- rep("non_cancelled",nrow(trainSet))
ridge_predict[ridge_prob>.5] <- "canceled"
ridge_predict <- ifelse(ridge_predict=="canceled",1,0)

mean(ridge_predict==trainSet$is_canceled)


#Predicting on testing data set
#predict class, type=”class”
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
#plot result
plot(cv.out)

#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

#Predicting on training data set 80.17 %
#predict class, type=”class”
lasso_prob <- predict(cv.out,newx = x_train1,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("non_cancelled",nrow(trainSet))
lasso_predict[lasso_prob>.5] <- "canceled"
lasso_predict <- ifelse(lasso_predict=="canceled",1,0)

mean(lasso_predict==trainSet$is_canceled)

#Cross Validation on Training data


#Predicting on testing data set
#predict class, type=”class”
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
#predict class, type=”class”
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

pred1 <- prediction(lasso_predict,testSet$is_canceled)
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc

#==========================================================================================================================================
