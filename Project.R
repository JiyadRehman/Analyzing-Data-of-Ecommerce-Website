#Amazon Project

library(readr)
Amazon <- read_csv("D:/MASON/1 Semester/AIT-580/Assignment/Data Analytics Project/Working/Data/Amazon2.csv")

#For analysis we will prepare the data now

Temp <- Amazon

#Library to work with dates
install.packages("lubridate")
library(lubridate)

#Using the above library to add a column for month

Temp$PurchaseMonth <- month(Temp$`purchase-date`)
View(Temp)

#Histogram for Months
hist(Temp$PurchaseMonth, col = "Green", xlab = 'Month', ylab = 'Frequency',
      main = 'Histogram'
     )

#Histogram for Item Price
hist(Temp$`item-price`, col = "Blue", xlab = 'Price', ylab = 'Frequency', breaks = 100,
     main = 'Histogram'
)

#Creating Hour Column
timestamp <- strptime(Temp$`purchase-time`, format="%H:%M:%S")
hours <-  as.numeric(format(timestamp, format="%H"))
Temp$Hour <- hours

remove(timestamp,hours)

#Summary
summary(Temp$`item-price`)
summary(Temp$`item-promotion-discount`)
summary(Temp$quantity)
table(Temp$asin)


#Histogram for the hours of the day
hist(Temp$Hour, col = "Red", xlab = 'Time in Hours', ylab = 'Frequency', breaks = 4,
     main = 'Histogram for Hours of the day'
)

#Boxplot for the products and their order time
boxplot(Temp$Hour~Temp$asin, xlab = "Product", ylab = "Time in Hours")

boxplot(Temp$Hour~Temp$PurchaseMonth, xlab = "Month", ylab = "Time in Hours",
        col = "Orange", border = "Red"
        )


#Scatter Plot of Promotion discount against Quantity Purchased
plot(Temp$`item-promotion-discount`, Temp$quantity, xlab = 'Discount', 
     ylab = 'Quantity Purchased', main = 'Quantity against Discount', col = "Green"
     )

scatter.smooth(x=Temp$`item-promotion-discount`, y=Temp$quantity,xlab = 'Discount', 
           ylab = 'Quantity Purchased', main = 'Quantity against Discount', col = "Green"
              )

#Correlation function
cor.test(Temp$`item-promotion-discount`, Temp$quantity)
t.test(Temp$`item-promotion-discount`, Temp$quantity)




model <- lm(Temp$`item-promotion-discount`~ Temp$quantity,
            data = Temp)
model

#*********************
#Data Frame for Maps

Mapdf <- Temp

Mapdf$Long[Mapdf$`ship-state` == Location$StateAbb] <- Location$Long
colnames(Location)[1] <- "ship-state"
View(Mapdf)
View(Location)
NewMap <- merge(Mapdf,Location, by = "ship-state")

View(NewMap)

NewMap<-NewMap[!(NewMap$Long<(-130)),]

#*****************************************
#Plotting on Map
library(ggplot2)
library(ggmap)
library(maps)
#data("us.cities")
usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_jitter(aes(x=NewMap$Long, y=NewMap$Lat), 
                                 col = "Red"
, position = position_jitter(w = 1, h = 1)) + geom_count() + ggtitle("Map of US")

remove(Mapdf)




