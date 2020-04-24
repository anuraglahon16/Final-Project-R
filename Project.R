getwd()
setwd("C:/Users/anami/Desktop/MS BA/Stats for BA/Group Project")
hotel=read.csv("hotel_bookings.csv")

 
#Exploratory Data Analysis
install.packages("funModeling") 
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("DataExplorer")
library(funModeling)
library(tidyverse)
library(Hmisc)
library(DataExplorer)

plot_str(hotel)         #show data structure
introduce(hotel)        #show basic description
plot_intro(hotel)       #plot basic description
glimpse(hotel)          #show data overview
plot_missing(hotel)     #show missing data


#Univariate stats
describe(hotel)
summary(hotel)
plot_num(hotel)         #plot histogram of all numeric data
plot_histogram(hotel)
plot_bar(hotel)         #barplot
plot_density(hotel)     #show density plot of all data
plot_correlation(hotel)
barplot(table(hotel$arrival_date_year))
barplot(table(hotel$arrival_date_month))
plot_boxplot(hotel)

