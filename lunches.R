## Clean up the working Environment =======================================================
rm(list = ls())
gc()
library(ggplot2)
library(reshape2)

## Import the Evening file and prepare for analysis ======================================
lunches <- read.csv("lunches.csv")
lunches$End.Date <- as.character(lunches$End.Date)
lunches$End.Date <- as.Date(lunches$End.Date, "%m/%d/%Y")


## Pulling the last 16 weeks revenues for each day individually===========================
monday <- lunches$Monday[(length(lunches$Monday)-15):length(lunches$Monday)]
tuesday <- lunches$Tuesday[(length(lunches$Tuesday)-15):length(lunches$Tuesday)]
wednesday <- lunches$Wednesday[(length(lunches$Wednesday)-15):length(lunches$Wednesday)]
thursday <- lunches$Thursday[(length(lunches$Thursday)-15):length(lunches$Thursday)]
friday <- lunches$Friday[(length(lunches$Friday)-15):length(lunches$Friday)]
saturday <- lunches$Saturday[(length(lunches$Saturday)-15):length(lunches$Saturday)]
sunday <- lunches$Sunday[(length(lunches$Sunday)-15):length(lunches$Sunday)]
week <- lunches$Week.Total[(length(lunches$Week.Total)-15):length(lunches$Week.Total)]


## Calculating lower confidence limit of each day of the week ============================
mon_lcl <- mean(monday)-qt(0.975, df=15)*sd(monday)/sqrt(16)
tue_lcl <- mean(tuesday)-qt(0.975, df=15)*sd(tuesday)/sqrt(16)
wed_lcl <- mean(wednesday)-qt(0.975, df=15)*sd(wednesday)/sqrt(16)
thu_lcl <- mean(thursday)-qt(0.975, df=15)*sd(thursday)/sqrt(16)
fri_lcl <- mean(friday)-qt(0.975, df=15)*sd(friday)/sqrt(16)
sat_lcl <- mean(saturday)-qt(0.975, df=15)*sd(saturday)/sqrt(16)
sun_lcl <- mean(sunday)-qt(0.975, df=15)*sd(sunday)/sqrt(16)
lcl_week <- mean(week)-qt(0.975, df=15)*sd(week)/sqrt(16)
print (lcl_week)

## Check of weekly LCL ===================================================================
week_chk <- sum(week > lcl_week)/16
print (week_chk)

## Create a line chart of the last 16 weeks of revenue===================================
last16 <- lunches[(length(lunches$Monday)-15):length(lunches$Monday),]
ggplot(data = last16, aes(x = End.Date, y = Week.Total)) + geom_line()

## Created a barplot of the averages of each day and week==================================
daily_means <- c(mean(monday), mean(tuesday), mean(wednesday), mean(thursday), mean(friday), mean(saturday), mean(sunday))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
names(daily_means) <- weekdays
barplot(daily_means, ylim = c(0, (max(daily_means)+50)), col = rainbow(7),
        xlab = "Days of the Week", ylab = "Average Revenue", main = "Average Daily Revenue, Prior 16 Weeks")
barplot(week, ylim = c(0, (max(week)+500)), col = rainbow(25), xlab = "Last 16 Weeks",
        ylab = "Revenues", main = "Last 16 Weeks of Revenue")


## Creating the yearly revenue estimate based on the LCL of each week=====================
yearly_lcl_estimate <- lcl_week * 52
print(yearly_lcl_estimate)

## Melting and graphing data by day ======================================================
length <- length(lunches$End.Date)
last16_melted <- melt(last16)
last16_melted <- last16_melted[1:112,]
last16_melted[,3] <- lunches$End.Date[(length(lunches$End.Date)-15):length(lunches$End.Date)]
ggplot(data = last16_melted, aes(x = V3, y = value, col = variable)) + geom_point() + facet_wrap(~ variable) + geom_smooth(method="lm")




