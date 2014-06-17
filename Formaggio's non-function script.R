rev_data <- read.csv("trending.csv") ## Loads the CSV file from Chris Paul - Post Trimmed

## The following section returns the means of each day since the start of business. 

dailymeans <- c(mean(rev_data[,"Monday"], na.rm = TRUE),mean(rev_data[,"Tuesday"], na.rm = TRUE),mean(rev_data[,"Wednesday"], na.rm = TRUE),mean(rev_data[,"Thursday"], na.rm = TRUE),mean(rev_data[,"Friday"], na.rm = TRUE),mean(rev_data[,"Saturday"], na.rm = TRUE),mean(rev_data[,"Sunday"], na.rm = TRUE))
names(dailymeans) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
dailymeans

## The following section returns the last 4 weeks of daily means. 

rows <- nrow(rev_data)
rowsl3 <- rows - 3
week_4 <- rev_data[rows:rowsl3,]
histmeans <- c(mean(week_4[,"Monday"], na.rm = TRUE),mean(week_4[,"Tuesday"], na.rm = TRUE),mean(week_4[,"Wednesday"], na.rm = TRUE),mean(week_4[,"Thursday"], na.rm = TRUE),mean(week_4[,"Friday"], na.rm = TRUE),mean(week_4[,"Saturday"], na.rm = TRUE),mean(week_4[,"Sunday"], na.rm = TRUE))
names(histmeans) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
histmeans

## The following section will show the increase in means over this time last year

rows52 <- nrow(rev_data)-52
rows55 <- rows52 - 3
week_52 <- rev_data[rows52:rows55,]
histmeans52 <- c(mean(week_52[,"Monday"], na.rm = TRUE),mean(week_52[,"Tuesday"], na.rm = TRUE),mean(week_52[,"Wednesday"], na.rm = TRUE),mean(week_52[,"Thursday"], na.rm = TRUE),mean(week_52[,"Friday"], na.rm = TRUE),mean(week_52[,"Saturday"], na.rm = TRUE),mean(week_52[,"Sunday"], na.rm = TRUE))
names(histmeans52) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
histmeans - histmeans52

## The following sections will return the average weekly sales from all time.

rev_data[,8] <- rowSums(rev_data)
colnames(rev_data)[8] <- "Weekly_Sums"
weeklymean <- mean(rev_data[,8])
weeklymean

## The following code returns the last 4 weeks of sales

rows <- nrow(rev_data)
last4 <- c(rev_data[rows,8],rev_data[rows-1,8], rev_data[rows-2,8],rev_data[rows-3,8])
names(last4) <- c("Last Week" , "2 Weeks Ago", "3 Weeks Ago", "4 Weeks Ago")
last4

## The following section will return the last 4 weeks increase in revenue over the prior year.

rows <- nrow(rev_data)
last4inc <- c(rev_data[rows-52,8],rev_data[rows-53,8], rev_data[rows-54,8],rev_data[rows-55,8])
names(last4inc) <- c("Last Week" , "2 Weeks Ago", "3 Weeks Ago", "4 Weeks Ago")
last4 - last4inc

## This section plots a histogram of the last year of weekly totals

hist(rev_data[rows:(rows-52),8])



## This section prints out the values of each variable created above.

print ("The average daily sale from the start of business is: ") 
dailymeans
print ("The averages, by day, of the last 4 weeks of sales is:")
histmeans
print("The increase of daily sales over this time last year is:")
histmeans - histmeans52
print("The weekly average since the start of business is:")
weeklymean
print("The last 4 weeks of sales are:")
last4
print("The increase of the last 4 weeks over this time last year is")
last4 - last4inc












