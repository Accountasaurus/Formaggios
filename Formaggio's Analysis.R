dailymeans <- function(csvname){
        
        ##csvname is the name of the weekly breakdown spreadsheet.
        
        ##This function will pull the updated daily means from the 
        ##spreadsheet that Chris Paul sends us. Make sure to format it correctly
        ##and to make the "-" into real 0s
        
        rev_data <- read.csv(csvname)
        dailymeans <- c(mean(rev_data[,"Monday"], na.rm = TRUE),mean(rev_data[,"Tuesday"], na.rm = TRUE),mean(rev_data[,"Wednesday"], na.rm = TRUE),mean(rev_data[,"Thursday"], na.rm = TRUE),mean(rev_data[,"Friday"], na.rm = TRUE),mean(rev_data[,"Saturday"], na.rm = TRUE),mean(rev_data[,"Sunday"], na.rm = TRUE))
        names(dailymeans) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        dailymeans
        
}
        
means_4_wk_hist <- function (csvname){
        
        ##Gives us the daily means for the last 4 weeks.
        
        rev_data <- read.csv(csvname)
        rows <- nrow(rev_data)
        rowsl3 <- rows - 3
        week_4 <- rev_data[rows:rowsl3,]
        histmeans <- c(mean(week_4[,"Monday"], na.rm = TRUE),mean(week_4[,"Tuesday"], na.rm = TRUE),mean(week_4[,"Wednesday"], na.rm = TRUE),mean(week_4[,"Thursday"], na.rm = TRUE),mean(week_4[,"Friday"], na.rm = TRUE),mean(week_4[,"Saturday"], na.rm = TRUE),mean(week_4[,"Sunday"], na.rm = TRUE))
        names(histmeans) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        histmeans
}

mean_increase <- function(csvname){
        
        ##provides the mean increase over this time last year
        
        rev_data <- read.csv(csvname)
        rows52 <- nrow(rev_data)-52
        rows55 <- rows52 - 3
        week_52 <- rev_data[rows52:rows55,]
        histmeans52 <- c(mean(week_52[,"Monday"], na.rm = TRUE),mean(week_52[,"Tuesday"], na.rm = TRUE),mean(week_52[,"Wednesday"], na.rm = TRUE),mean(week_52[,"Thursday"], na.rm = TRUE),mean(week_52[,"Friday"], na.rm = TRUE),mean(week_52[,"Saturday"], na.rm = TRUE),mean(week_52[,"Sunday"], na.rm = TRUE))
        names(histmeans52) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        means_4_wk_hist(csvname) - histmeans52
        
        
}

weekly_average <- function(csvname){
        
        ##Will return the average weekly sales of all time.
        
        rev_data <- read.csv(csvname)
        rev_data[,8] <- rowSums(rev_data)
        weeklymean <- mean(rev_data[,8])
        weeklymean
}

last_4_weeks <- function(csvname){
        
        ##will return the last 4 weaks of revenue
        
        rev_data <- read.csv(csvname)
        rev_data[,8] <- rowSums(rev_data)
        rows <- nrow(rev_data)
        last4 <- c(rev_data[rows,8],rev_data[rows-1,8], rev_data[rows-2,8],rev_data[rows-3,8])
        names(last4) <- c("Last Week" , "2 Weeks Ago", "3 Weeks Ago", "4 Weeks Ago")
        last4
}

last4increase <- function(csvname){
        rev_data <- read.csv(csvname)
        rev_data[,8] <- rowSums(rev_data)
        rows <- nrow(rev_data)
        last4inc <- c(rev_data[rows-52,8],rev_data[rows-53,8], rev_data[rows-54,8],rev_data[rows-55,8])
        names(last4inc) <- c("Last Week" , "2 Weeks Ago", "3 Weeks Ago", "4 Weeks Ago")
        last4inc <- last4 - last4inc
        last4inc
}



dailyall <- dailymeans("trending.csv") ## Prints out the daily means since the start of business.
last4wkday <- means_4_wk_hist("trending.csv") ## Prints out the means of the last 4 weeks of business.
increasewkday <- mean_increase("trending.csv") ## Prints out the increase in daily means over this time last year.
weeklymean <- weekly_average("trending.csv") ## Prints the weekly mean from start of business.
last4 <- last_4_weeks("trending.csv") ## Prints last 4 weeks of business
last4inc <- last4increase("trending.csv") ## Prints last 4 weeks increase over this time last year.

dailyall
last4wkday
increasewkday
weeklymean
last4
last4inc