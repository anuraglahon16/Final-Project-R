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
sum(is.na(hotels))
#Structure of the hotel data set
str(hotels)
#Replacing  missing values in Children column from the corresponding Babies column
n <- length(hotels$children)
for (i in 1:n) {
  if (is.na(hotels$children[i]))
    hotels$children[i] <- hotels$babies[i]
}
#checking missing values in quantitve columns
sapply(hotels, function(x) sum(is.na(x)))
#Missing value of quantitive hotel data set
plot_missing(hotels)
#describeBy function
library(psych)
psych::describe(hotels)
describeBy(hotels, hotels$hotel)

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




# Drop the columns of the dataframe
extended_hotels$hotel <- NULL
extended_hotels$arrival_date_month <- NULL
extended_hotels$reservation_status <- NULL
extended_hotels$deposit_type <- NULL
extended_hotels$customer_type <- NULL
#We are deleting the company column as it have more than 1 lakhs missing value
extended_hotels$company <-NULL


str(extended_hotels)
view(extended_hotels)
glimpse(extended_hotels)



