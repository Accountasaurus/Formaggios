## Clean up the working Environment =======================================================
rm(list = ls())
gc()
library(ggplot2)
library(reshape2)
library(lubridate)


delivery <- read.csv("delivery.csv")
delivery$Date <- as.character(delivery$Date)
delivery$Date <- as.Date(delivery$Date, "%m/%d/%Y")
delivery[,3] <- weekdays(as.Date(delivery[,1]))
meandl <- tapply(delivery$Qty, delivery$V3, mean)
meandl <- c(meandl[2], meandl[6], meandl[7], meandl[5], meandl[1], meandl[3], meandl[4])

barplot(meandl, ylim = c(0, (max(meandl)+5)), col = rainbow(7),
        xlab = "Days of the Week", ylab = "Average Deliveries", main = "Q1 average deliveries")

delivery[,4] <- week(delivery$Date)
delbyweek <- tapply(delivery$Qty, delivery$V4, sum)

barplot(delbyweek[1:12], ylim = c(0, (max(delbyweek)+5)), col = rainbow(7),
        xlab = "Weeks since Jan 1, 2015", ylab = "Deliveries", main = "Q1 Deliveries by Week")


lddelivery <- read.csv("long.csv")
lddelivery$Date <- as.character(lddelivery$Date)
lddelivery$Date <- as.Date(lddelivery$Date, "%m/%d/%Y")
lddelivery[,3] <- weekdays(as.Date(lddelivery[,1]))
ldmeandl <- tapply(lddelivery$Qty, lddelivery$V3, mean)
ldmeandl <- c(ldmeandl[2], ldmeandl[6], ldmeandl[7], ldmeandl[5], ldmeandl[1], ldmeandl[3], ldmeandl[4])

barplot(ldmeandl, ylim = c(0, (max(ldmeandl)+5)), col = rainbow(7),
        xlab = "Days of the Week", ylab = "Average  Long Distance Deliveries", main = "Q1 Average Long Distance Deliveries")


lddelivery[,4] <- week(lddelivery$Date)
lddelbyweek <- tapply(lddelivery$Qty, lddelivery$V4, sum)

barplot(lddelbyweek[1:12], ylim = c(0, (max(lddelbyweek)+5)), col = rainbow(7),
        xlab = "Weeks since Jan 1, 2015", ylab = "Long Distance Deliveries", main = "Q1 Long Distance Deliveries by Week")

