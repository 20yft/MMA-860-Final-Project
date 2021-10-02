library(tidyverse)
library(readxl)
library(haven)
library(tidyr)
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(dummies)
library(mice)
library(dplyr)
library(carData)
library(car)
library(lubridate)
library(writexl)
library(lattice)
library(caret)
par(mfrow=c(2,2))

product <- read_excel("/Users/zhanganqi/Desktop/860/final project/Data/Product.xlsx", sheet=2)
store <- read_excel("/Users/zhanganqi/Desktop/860/final project/Data/Store.xlsx", sheet=2)
transaction <- read_excel("/Users/zhanganqi/Desktop/860/final project/Data/Transaction Log.xlsx", sheet=2)


################################################# 1. Significant cleaning of data ################################################

#define a function to return string without leading or trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#remove space for the text column in transaction and product table
transaction$SKU_NME <- trim(transaction$SKU_NME)
product$ITEM_NAME <- trim(product$ITEM_NAME)

#Clean the product table:
#select necessary columns from product table
product2 <- subset(product, select = -c(DEPT_NBR, DEPT_NAME, SUB_DEPT_NBR, ITEM_NBR))
#Drop duplicate item names from product table
pro_dedup <- distinct(product2, ITEM_NAME, BRAND_NBR, .keep_all= TRUE)

#Clean the transaction table:
#select necessary columns from transaction table 
transaction2 <- subset(transaction, select = -c(REGISTER_NBR, LINE_NBR, CLASS_NBR, UPC, TXN_NBR, ITEM_NBR))
#Noticed in the transaction table, 11 observation has less than $1 in the Selling price 
#column which doesn't make sense, so we reomve them
transaction2_clean <- filter(transaction2, transaction2$LNE_ORIG_AMT > 1 | transaction2$LNE_ORIG_AMT <0)

#Clean the store table:
#select necessary columns from store table
store2 <- subset(store, select = -c(BANNER_NBR, BANNER_NAME, ACTIVE_STATUS, STORE_TYPE_CODE, ADDRESS))


################################################# 2. Significant merging/joining data ################################################

#Join the product and transaction table
pro_tran <- merge(x=pro_dedup,y=transaction2_clean, by.x ='ITEM_NAME', by.y = "SKU_NME")
#The missing value in the Price Markdown, push_notification and promotions_emailed columns means
#there is no discount on this item thus we can fill the missing value by 0
pro_tran[is.na(pro_tran)] <- 0
pro_tran$year <- year(pro_tran$DAY_DT)
pro_tran$week <- week(pro_tran$DAY_DT)
pro_tran$discount_percent<- round(1-(pro_tran$LNE_NET_SALES_AMT / pro_tran$LNE_ORIG_AMT),2)

#Join the store and transaction table
sto_tran <- merge(x=store2, y=transaction2_clean, by.all='STORE NBR')


################################################ 3. Regression model ###############################################

#Build Regression models to predict unit of sales for SANTANA brand
#since we don't know which item was returned from our raw data thus we exclude the returns 
santana <- filter(pro_tran, pro_tran$BRAND_NAME == 'SANTANA' & TXN_TYPE != 'Return')

#Due to the data limitation, there is no guaranteed sales per day for brand SANTANA,we have to predict the weekly sales 
#Calcualte the weekly sales in last two year (Y)
santana_weekly <- group_by(santana, year, week, TXN_TYPE) %>% summarise(Unit_Sales = n()) 
#merge Y and X
santana_model_data <- merge(x=santana_weekly, y=santana, by.all=c('week', 'year'))
#We consider the average selling price, avg. discount percentage, total number of push_notifications and total number of promotions emailed
#in the weekly level in the prediction model 
santana_model_data2 <- group_by(santana_model_data, year, week) %>% summarise(Unit_Sales= n(), 
                                                                              Avg_sales_amount = mean(LNE_NET_SALES_AMT),
                                                                              Avg_discount = mean(discount_percent),
                                                                            Total_Push_notification = sum(Push_notification),
                                                                              Total_emails =sum(Number_of_Promotions_Emailed))

#Split train and test data, going to train on 1.5 years, test on the 0.5 year
train<- santana_model_data2[0:57,]
test <- santana_model_data2[58:74,]

#Create lm on train data
santana_reg<- lm(Unit_Sales ~ ., train)
summary(santana_reg)
plot(santana_reg)

santana_reg2 <- lm(Unit_Sales ~ . -year -week -Avg_sales_amount, train)
summary(santana_reg2)
plot(santana_reg2)
#Predict values
pred <-predict(santana_reg2, test)
#Calculate statistics
data.frame(R2 = R2(pred, test$Unit_Sales),RMSE = RMSE(pred, test$Unit_Sales),MAE = MAE(pred, test$Unit_Sales))


########################################## 4. Insight tables used for visulazaiton ######################################

#Total sales per city in last two years
by_city <-group_by(sto_tran, CITY, TXN_TYPE) %>% summarise(count = n()) 
by_city2 <- spread(by_city, TXN_TYPE, count)
by_city2[is.na(by_city2)] <- 0
by_city2$net_sale <- by_city2$Sale - by_city2$Return

#Total sales per mall type in last two years
by_mall <- group_by(sto_tran, STORE_TYPE, TXN_TYPE) %>% summarise(count = n()) 
by_mall2 <- spread(by_mall, TXN_TYPE, count)
by_mall2$net_sale <- by_mall2$Sale - by_mall2$Return

#Total sales per brand in last two years in last two years
by_brand<-group_by(pro_tran, BRAND_NAME, TXN_TYPE) %>% summarise(count = n()) 
by_brand2 <- spread(by_brand, TXN_TYPE, count)
by_brand2$net_sale <- by_brand2$Sale - by_brand2$Return



























