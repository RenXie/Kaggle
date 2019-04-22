############ LIBRARIES #####

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(e1071)
library(gbm)

#### Load  Data ######

sales_data = read.csv("../Predict sales/Input/sales_train_v2.csv")
item_data = read.csv("../Predict sales/Input/items.csv")
test_data = read.csv("../Predict sales/Input/test.csv")

#### Data Preparation  ##### 

# get the item category details in the sales data
sales_data = merge(sales_data, item_data[,c("item_id", "item_category_id")], by = "item_id", all.x = T)
sales_data$date = as.Date(sales_data$date, "%d.%m.%Y")

sales_data$year = year(sales_data$date)
sales_data$year =  as.factor(sales_data$year)

sales_data$month = month(sales_data$date)
sales_data$month = as.factor(sales_data$month)

sales_data$day = day(sales_data$date)
sales_data$day = as.factor(sales_data$day)

sales_data$weekdays =  weekdays(sales_data$date)
sales_data$weekdays = as.factor(sales_data$weekdays)

sales_data$item_category_id =  as.factor(sales_data$item_category_id)