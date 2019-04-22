############ LIBRARIES #####

library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)


### EDA ####
# sales shop wise
sales_shopwise = sales_data %>%
  select(shop_id, item_cnt_day) %>%
  group_by(shop_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_shopwise, 
       mapping = aes(x = reorder(shop_id, item_cnt_day), 
                     y = item_cnt_day, 
                     fill = factor(shop_id))) +
  geom_histogram(stat = "identity") +
  # coord_flip() +
  xlab("Shop ID") + ylab("Sales Count")+
  ggtitle(label = "Shop wise sales")


# sales item category wise
sales_categorywise = sales_data %>%
  select(item_category_id, item_cnt_day) %>%
  group_by(item_category_id) %>%
  summarise(item_cnt_day =  sum(item_cnt_day, na.rm = T))

ggplot(data =  sales_categorywise, 
       mapping = aes(x = reorder(item_category_id,item_cnt_day), 
                     y = item_cnt_day,
                     fill = factor(item_category_id))) +
  geom_histogram(stat = "identity") +
  # coord_flip() +
  xlab("Item Category") + ylab("Sales Count") +
  ggtitle("Sale Item Category wise")

# most items in shop

items_in_shop = sales_data %>%
  select(shop_id, item_id) %>%
  group_by(shop_id) %>%
  summarise(item_id = n_distinct(item_id))

ggplot(data = items_in_shop,
       mapping = aes(x = reorder(shop_id,item_id),
                     y = item_id,
                     fill = factor(shop_id)))+
  geom_histogram(stat = "identity") +
  xlab(" Shop ID")+ ylab(" Items in shop")+
  ggtitle("Most Items in Shops") +
  coord_flip()

# which catefgory of item is available most 
items_in_category = sales_data %>%
  select(item_category_id, item_id) %>%
  group_by(item_category_id) %>%
  summarise(item_id =  n_distinct(item_id))

ggplot(data = items_in_category,
       mapping = aes(x = reorder(item_category_id,item_id),
                     y = item_id,
                     fill = factor(item_category_id)))+
  geom_histogram(stat = "identity") +
  xlab(" Category ID")+ ylab(" Items in Category")+
  ggtitle("Most Items per Category") +
  coord_flip()


# which item is most popular and most sold in the each shop 

popularity  =  sales_data %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  # filter(sold_item_count == max(sold_item_count)) %>%
  arrange(desc(sold_item_count))

popular_items_in_shop  =  sales_data %>%
  group_by(shop_id, item_id) %>%
  summarise(sold_item_count = sum(item_cnt_day)) %>%
  filter(sold_item_count == max(sold_item_count)) %>%
  arrange(desc(sold_item_count))

ggplot(data = popular_items_in_shop,
       mapping = aes(x = reorder(shop_id, sold_item_count),
                     y = sold_item_count,
                     fill = factor(item_id))) +
  geom_histogram(stat = "identity") +
  xlab("") + ylab("Sales Count") +
  ggtitle("Most Popular Item per shop") +
  coord_flip()


# which shop has most category of items 
shop_with_most_category = sales_data %>%
  select(shop_id, item_category_id) %>%
  group_by(shop_id) %>%
  summarise(category_count =  n_distinct(item_category_id)) %>%
  arrange(desc(category_count))

ggplot(data = shop_with_most_category,
       mapping = aes(x = reorder(shop_id, category_count),
                     y = category_count,
                     fill = factor(shop_id))) +
  geom_histogram(stat = "identity") +
  xlab("Shop ID") + ylab("Item Category Count") +
  ggtitle("Most Item category per shop") +
  coord_flip()

# which item category is most popular and most sold in each shop
popular_category =  sales_data %>%
  group_by(shop_id, item_category_id) %>%
  summarise(category_count = sum(item_cnt_day)) %>%
  filter(category_count == max(category_count)) %>%
  arrange(desc(category_count))

ggplot(data =  popular_category, 
       mapping = aes(x = reorder(shop_id, category_count),
                     y =  category_count,
                     fill = factor(item_category_id))) +
  geom_histogram(stat ="identity") +
  xlab("Shop ID")+ ylab("Category per shop") + ggtitle("Most popular item category per shop")+
  coord_flip()


# which item category is highest sales grossing in all shops
most_grossing_category = sales_data %>%
  group_by(item_category_id) %>%
  summarise(total_gross = sum(item_cnt_day * item_price)) %>%
  arrange(desc(total_gross))

ggplot(most_grossing_category, 
       aes(x = reorder(item_category_id, total_gross),
           y = total_gross,
           fill = factor(item_category_id))) +
  geom_histogram(stat = "identity") +
  xlab("Category ID") + ylab("Total Gross")+
  ggtitle("Total Gross per Item category") +
  coord_flip()



# item categories available in each shop 
item_category_in_shops = sales_data %>%
  group_by(shop_id) %>%
  summarise(item_category =  paste(sort(unique(item_category_id)), collapse = ", ")) 

head(item_category_in_shops)

# which item gets sold  the most under which category 

most_sold_item_per_category = sales_data %>%
  group_by(item_category_id, item_id) %>%
  summarise(total_sales = sum(item_price * item_cnt_day)) %>%
  filter(total_sales == max(total_sales)) %>%
  arrange(desc(total_sales))


ggplot(most_sold_item_per_category,
       aes(x = reorder(item_category_id, total_sales), 
           y = total_sales,
           fill = factor(item_id))) +
  geom_histogram(stat = "identity") +
  labs(title = "Items sold per category",x = "Category ID", y = "Sales", fill = "Item ID") +
  coord_flip()




# day and month wise total sales 
month_daywise_total_sales =  sales_data %>%
  group_by(month, day) %>%
  summarise(total_sales =  sum(item_price * item_cnt_day))

ggplot(month_daywise_total_sales, 
       aes(x = day, y = total_sales, group =  month, color =  factor(month))) +
  geom_line() + geom_point() +
  labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Months") 


ggplot(month_daywise_total_sales, 
       aes(x = day, 
           y = total_sales, 
           fill =  factor(day))) +
  geom_histogram(stat = "identity") +
  labs(title = "Total Sales month-day wise", x = "Days", y = "Total sales", fill = "Days") +
  facet_wrap(~month, ncol = 2)


# year wise total sales
yearly_sales = sales_data %>%
  group_by(year) %>%
  summarise(yearly_sale = sum(item_price * item_cnt_day))

ggplot(yearly_sales, aes(x =  year, y = yearly_sale, fill =  factor(year)))+
  geom_histogram(stat = "identity")+
  labs(title = "Yearly Sales", x = "Year", y = "Total Sale", fill = "Year")

# year and month wise total sales 
ym_sales = sales_data %>%
  group_by(year, month) %>%
  summarise(ym_sale = sum(item_price*item_cnt_day)) %>%
  arrange(year)

ggplot(ym_sales, aes(x =  month, y = ym_sale, fill =  factor(year)))+
  geom_histogram(stat = "identity") +
  labs(title = "Yearly-Monthly Sales", x = "Months", y =  "Total sales", fill = "Year")

ggplot(ym_sales, aes(x =  month, y = ym_sale, fill =  factor(year)))+
  geom_histogram(stat = "identity", position = "dodge") +
  labs(title = "Yearly-Monthly sales", x = "Months", y =  "Total sales", fill = "Year")


# percent of items sold each month


# number of items sold each day 
daily_sale = sales_data %>%
  group_by(date) %>%
  summarise(items_sold =  sum(item_cnt_day))

ggplot(daily_sale, aes(x =  date, y = items_sold, color =  items_sold)) +
  geom_line() + geom_point()+
  labs(title = "Daily Item sold", x =  "Date", y = "Items sold")


# items sold on weekdays 
weekdays_item_sold = sales_data %>%
  group_by(weekdays) %>%
  summarise(item_sold = sum(item_cnt_day)) %>%
  arrange(desc(item_sold))

ggplot(weekdays_item_sold, aes(x =reorder(weekdays, item_sold), y =  item_sold, fill = factor(weekdays)))+
  geom_bar(stat = "identity") +
  labs(title = "Items sold on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
  coord_flip()

# sale revenue on weekdays
weekdays_sales = sales_data %>%
  group_by(weekdays) %>%
  summarise(total_sale = sum(item_cnt_day * item_price)) %>%
  arrange(desc(total_sale))

ggplot(weekdays_sales, aes(x =reorder(weekdays, total_sale), y =  total_sale, fill = factor(weekdays)))+
  geom_bar(stat = "identity") +
  labs(title = "Sales on weekdays", x = "Week Days", y =  "Items sold", fill = "Week Days") +
  coord_flip()

## End of EDA ####