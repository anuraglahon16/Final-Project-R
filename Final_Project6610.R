#Seeing the current directory
getwd()
#Setting the current directory
setwd('C:/Users/anura/Downloads')
#Required packages
library(funModeling)
library(tidyverse)
library(Hmisc)
library(DataExplorer)
#reading data
hotels <- read.csv('hotel_bookings.csv')
#First 6 rows
head(hotels)
#Dimension of the hotel data set
dim(hotels)
#Summary of the hotel data set
summary(hotels)
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
#Replacing Undefinded with mode but we can use backward and forward fill as this is a time series data
#Function to calculate mode
modeM <- getmode(hotels$market_segment)
modeM
#Replacing Undefined with mode
hotels$market_segment <- replace(hotels$market_segment,hotels$market_segment=='Undefined',modeM)
#We are droping the levels as we have replaced Undefined with mode
hotels$market_segment <- factor(hotels$market_segment)
#Checking unique value in market_segment
unique(hotels$market_segment)

#Replacing Undefinded with mode but we can use backward and forward fill as this is a time series data
#Function to calculate mode
modeD <- getmode(hotels$distribution_channel)
modeD
#Replacing Undefined with mode
hotels$distribution_channel <- replace(hotels$distribution_channel,hotels$distribution_channel=='Undefined',modeD)
#We are droping the levels as we have replaced Undefined with mode
hotels$distribution_channel <- factor(hotels$distribution_channel)
#Checking unique value in market_segment
unique(hotels$distribution_channel)






#checking missing values in quantitve columns
sapply(hotels, function(x) sum(is.na(x)))
#Missing value of quantitive hotel data set
plot_missing(hotels)
#describeBy function
library(psych)
psych::describe(hotels)
describeBy(hotels, hotels$hotel)


#---------------------DUMMY VARIABLES-----------------------------


# Create the dummy variables for hotel
hotel_code <- dummy.code(hotels$hotel)
hotel_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(hotel_code, hotels)
extended_hotels

# Create the dummy variables for customer_type
customer_code <- dummy.code(hotels$customer_type)
customer_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(customer_code, extended_hotels)
extended_hotels

# Create the dummy variables for deposit_type
deposit_code <- dummy.code(hotels$deposit_type)
deposit_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(deposit_code, extended_hotels)
extended_hotels

# Create the dummy variables for reservation_status
reservation_status_code <- dummy.code(hotels$reservation_status)
reservation_status_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(reservation_status_code, extended_hotels)
extended_hotels

unique(hotels$arrival_date_month)

# Create the dummy variables for arrival_date_month
arrivaldaymonth_status_code <- dummy.code(hotels$arrival_date_month)
arrivaldaymonth_status_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(arrivaldaymonth_status_code, extended_hotels)
extended_hotels

# Create the dummy variables for meal
meal_code <- dummy.code(hotels$meal)
meal_code
summary(hotels$meal)
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(meal_code, hotels)
extended_hotels

# Create the dummy variables for distribution_channel
distribution_code <- dummy.code(hotels$distribution_channel)
distribution_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(distribution_code, hotels)
extended_hotels

# Create the dummy variables for distribution_channel
reserved_room_code <- dummy.code(hotels$reserved_room_type)
reserved_room_code
# Merge the dataset in an extended dataframe
extended_hotels <- cbind(reserved_room_code, hotels)
extended_hotels





#-----------Droping Columns for those where we create Dummy Variable ----------


# Drop the columns of the dataframe
extended_hotels$hotel <- NULL
extended_hotels$arrival_date_month <- NULL
extended_hotels$reservation_status <- NULL
extended_hotels$deposit_type <- NULL
extended_hotels$customer_type <- NULL
extended_hotels$meal<-NULL
extended_hotels$market_segment <- NULL
#We are deleting the company column as it have more than 1 lakhs missing value
extended_hotels$company <-NULL
extended_hotels$distribution_channel <- NULL


#----------Inspecting our Hotels Dataset where we create Dummy variable-----------------

str(extended_hotels)
view(extended_hotels)
glimpse(extended_hotels)
