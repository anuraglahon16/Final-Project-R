# Final Project R
![](https://github.com/anuraglahon16/Final-Project-R/blob/master/images/download.png) ![](https://github.com/anuraglahon16/Final-Project-R/blob/master/images/resize-1589237735131618673GreenModernCampusBuildingSchoolBrochure.jpg)
 # 1.	Project Overview:
 
In this project, we will try to predict the possibility of a booking cancellations for a hotel based on different factors and also try to predict if there is a likelihood of getting a special requests from customers based on different factors. The data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, the number of adults, children, and/or babies, and the number of available parking spaces, among other things. From this, we can understand the customer’s behavior and it might help us to take better decisions.  

The process of our analysis will be: Understanding the Datasets, Data preparation and wrangling, Analyzing, and visualizing the data, Model building, comparing the model, and finally selecting the best model. 

Our goal is to predict what type of customers need special request and to predict the possibility of booking cancellations by analyzing all the factors that can influence booking cancellations. We also performed time series analysis to forecast the number of bookings using Holtwinters and ARIMA model. 

# 2.Data Source :

Data was collected from the Kaggle website. The data is originally from the article Hotel Booking Demand Datasets, written by Nuno Antonio, Ana Almeida, and Luis Nunes for Data in Brief, Volume 22, February 2019. This data set contains booking information for a city hotel and a resort hotel, and includes information such as when the booking was made, length of stay, the number of adults, children, and/or babies, and the number of available parking spaces, among other things
[Hotel booking data set](https://github.com/anuraglahon16/Final-Project-R/blob/master/hotel_bookings.csv)
 
# 3.	Business Understanding:

My goal for this project is predicting which kind of customers need special request and predicting the possibility of a booking for a hotel by knowing different features. This will help the hotel booking company to make better decisions.

# 4.	Data Understanding:

R library used: fun Modeling, tidyverse, Hmisc, DataExplorer, dplyr, caret, lattice, magrittr, ggplot2, scales, gridExtra, psych, plotly and many more.

The data set contains 119390 rows and 32 columns.

Missing Data 

• There were only 4 missing values for children column in our dataset. We replaced those 4 missing rows in Children column with the corresponding Babies column.There were 489 null values in the country column. We removed those rows to get the final dataset. 


# 5. Data preparation / Wrangling:

 For the meal column we replaced undefined rows with SC as both means no meal package.
  • There were 8 undefined rows in the market segment column we replaced those Undefined columns with mode value of the market segment.   • There were 2 undefined rows in the distribution channel column, we replaced those Undefined rows with mode value of the distribution channel. 
  
# 6. MODEL BUILDING :

MODEL BUILDING - FOR BOOKING CANCELLATIONS:
[R file for booking cancellations](https://github.com/anuraglahon16/Final-Project-R/blob/master/Model%20Building%20for%20Booking%20cancellations.R)  

MODEL BUILDING - FOR SPECIAL REQUESTS :
[R file for special requests](https://github.com/anuraglahon16/Final-Project-R/blob/master/Model%20Building%20for%20Special%20request.R)

# 7. TIME SERIES ANALYSIS FOR BOOKINGS: 

TIME SERIES ANALYSIS USING FORECASTS MODEL and MODEL USING HOLTWINTERS
[Time Series R file](https://github.com/anuraglahon16/Final-Project-R/blob/master/TimeSeriesAnalysis.R)

# 8. CONCLUSIONS :

• Booking cancellation model will help to Identify the likelihood of bookings being cancelled and makes it possible for hotel managers to take measures to avoid these potential cancellations, such as offering services, discounts, or other perks.  

• The prediction model enables hotel managers to mitigate revenue loss derived from booking cancellations and the risks associated with overbooking (reallocation costs, cash, or service compensations). 

• Special request model will contribute to reduce uncertainty in the inventory allocation and pricing decision process by predicting the likelihood of getting a request from customers. 

• The repeated guests are only 3%, which points a change in marketing and hospitality. 

 


 






