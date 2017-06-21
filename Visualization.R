setwd('D:/My documents/Study/0.1/Instacart/data')
library('dplyr')
library('magrittr')
library('ggplot2')
library('plotly')
library('data.table')

aisles <- fread('aisles.csv')
products <- fread('products.csv')
orders <- fread('orders.csv')
departments <- fread('departments.csv')
orders_products_train <- fread('order_products__train.csv')
orders_products_prior <- fread('order_products__prior.csv')

orders
orders_products_train[,'add_to_cart_order']
orders_products_prior[,'add_to_cart_order']
# the prior set contain all the data about reorder while the train set is only 1/3

#1. Orders distribution over time:

orders$order_hour_of_day <- as.factor(orders$order_hour_of_day)
orders$order_dow <- as.factor(orders$order_dow)


plot_ly(orders, x = ~order_hour_of_day ,type = 'histogram') %>%
  layout(xaxis = list(title = 'Order hour'), barmode = 'stack')
# most of the orders are between 9-16h, approximately
plot_ly(orders, x = ~order_dow ,type = 'histogram') %>%
  layout(xaxis = list(title = 'Order day of week'))
# Day 0 and 1 seem to prevail
numbers_orders <- orders%>%
                    group_by(order_dow, order_hour_of_day )%>%
                    summarise(numbers_orders=n())
plot_ly(numbers_orders, x = ~order_dow, y = ~order_hour_of_day, z = ~numbers_orders,
        type = "heatmap",
        colorscale = "Greys") %>%
  layout(xaxis = list(title = 'Order day of week'),
         yaxis = list(title = 'Order hour'))

# of course, day 0 and 1 between 10 and 15h seems to have the most orders

# 2. Prior order distribution 
plot_ly(orders, x = as.factor(orders$days_since_prior_order) ,type = 'histogram') %>%
  layout(xaxis = list(title = 'Days since prior order'))
# After one week and one month?

# 3. Dig deeper into each user
# Order per user
users_orders <- orders %>% 
                  group_by(user_id) %>%
                  summarise(numbers_order = n(), 
                            days_spo_avg = mean(days_since_prior_order, na.rm =T))

plot_ly(users_orders, x = as.factor(users_orders$numbers_order) ,type = 'histogram') %>%
  layout(xaxis = list(title = 'Number of order per user'))
# half of them have 10 or fewer orders:
median(users_orders$numbers_order)

# 4. Items expoloration:
# 4a Bestsellers:
top_products <- orders_products_prior %>% 
                    group_by(product_id) %>%
                    summarise(numbers_orders = n()) %>%
                    arrange(desc(numbers_orders)) %>%
                    left_join(y = products, by = 'product_id')
top_products[,(2:3)] %>% head(20)
# Top 10 are all vege and fruits
# reorder or not
temp <- orders_products_prior %>% 
          group_by(reordered) %>%
          summarise(numbers_orders = n())
temp[,1] =c("First time ordered", "Reordered")

plot_ly(temp, labels = ~reordered, values = ~numbers_orders, type = 'pie') %>%
  layout(title = "Reordered proportion",  showlegend = T)
# nearly 60 percent of the orders are the reorderred ones

# 4b The first ordered products:
orders_products_prior %>% 
  filter(add_to_cart_order == 1) %>%
  group_by(product_id) %>%
  summarise(times = n()) %>%
  left_join(products, by = 'product_id') %>%
  arrange(desc(times)) %>%
  head(10)

# most likely to be re-orderred
top_1st <- orders_products_prior %>%
                  group_by(product_id , add_to_cart_order) %>%
                  summarise(times = n()) %>%
                  mutate(pct = times / sum(times)) %>%
                  filter(add_to_cart_order == 1, times > median(times)) %>%
                  left_join(products[,c(1:2)], by = 'product_id' ) %>%
                  arrange(desc(pct))

top_reorder <- orders_products_prior %>%
                  group_by(product_id , add_to_cart_order) %>%
                  summarise(times = n(), times_reorder = sum(reordered)) %>%
                  mutate(pct = times / sum(times), pct_reorder = times_reorder / sum(times)) %>%
                  left_join(products[,c(1:2)], by = 'product_id' ) %>%
                  arrange(desc(pct_reorder))
# Drinks at the top list of being remembered first even if they are re-ordered

# 5 Products order by departments and ailses:
temp <- orders_products_prior %>% 
          left_join(products, by = 'product_id')
temp2 <- temp %>% 
          group_by(department_id, aisle_id) %>%
          summarise(times = n())%>%
          left_join(departments, by = 'department_id')%>%
          left_join(aisles, by = 'a_id')
temp3 <- temp %>% 
          group_by(department_id) %>%
          summarise(times = n()) %>%
          left_join(departments, by = 'department_id')

plot_ly(temp3, labels = ~department, values = ~times, type = 'pie')%>%
  layout(title = "Orders by departments",  showlegend = F)
# the produce department have the most orders

        
