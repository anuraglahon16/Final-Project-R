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
ts$reservation_status_date <- as.Date(timeS$reservation_status_date)
ggplot(ts, aes(reservation_status_date, n)) + geom_line()

#Time Series Analysis
ggplot(timeS, aes(reservation_status_date, n)) + geom_line()

ts <- timeS %>% filter(!is.na(n))
ts
# Frequency is set with 365 because it's daily
components <- stl(ts(ts$n, frequency=365), 'periodic')
# seasonal, trend, remainder
plot(components)

#It is a repeat may delete whichever is better
hotels%>%group_by(arrival_date_month)%>%summarise(Count = n())%>%arrange(-Count)%>%ggplot(aes(x = arrival_date_month, y = Count)) +
  geom_bar(stat = 'identity',fill = "dodgerblue") + coord_flip() + geom_text(aes(x =arrival_date_month, y=0.02, label= Count),
                                                                             hjust=-1, vjust=0, size=4, 
                                                                             colour="black", fontface="bold",
                                                                             angle=360)
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

# Create a frequency for meal
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
hotels %>%
  plot_ly(x = ~Global_Sales, y = ~Critic_Score) %>%
  add_markers(marker = list(opacity = 0.5)) %>%
  layout(xaxis = list(title = "Global sales (millions of units)", type = "log"),
         yaxis = list(title = "Critic score"))






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
#---------------------DUMMY VARIABLES -----------------------------


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
extended_hotels$reserved_room_type <-NULL
#We are deleting the company column as it have more than 1 lakhs missing value
extended_hotels$company <-NULL
extended_hotels$distribution_channel <- NULL


#----------Inspecting our Hotels Dataset where we create Dummy variable-----------------

str(extended_hotels)
view(extended_hotels)
glimpse(extended_hotels)


#------Will also use this later , PCA to Reduce Dimension----------------------------
