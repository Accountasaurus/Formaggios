## Clean up the working Environment =======================================================
rm(list = ls())
gc()

## Import the Evening file and prepare for analysis ======================================
evenings <- read.csv("evenings.csv")
evenings$End.Date <- as.character(evenings$End.Date)
evenings$End.Date <- as.Date(evenings$End.Date, "%m/%d/%Y")


## Pulling the last 16 weeks revenues for each day individually===========================
monday <- evenings$Monday[(length(evenings$Monday)-15):length(evenings$Monday)]
tuesday <- evenings$Tuesday[(length(evenings$Tuesday)-15):length(evenings$Tuesday)]
wednesday <- evenings$Wednesday[(length(evenings$Wednesday)-15):length(evenings$Wednesday)]
thursday <- evenings$Thursday[(length(evenings$Thursday)-15):length(evenings$Thursday)]
friday <- evenings$Friday[(length(evenings$Friday)-15):length(evenings$Friday)]
saturday <- evenings$Saturday[(length(evenings$Saturday)-15):length(evenings$Saturday)]
sunday <- evenings$Sunday[(length(evenings$Sunday)-15):length(evenings$Sunday)]


## Calculating lower confidence limit of each day of the week ============================
mon_lcl <- mean(monday)-qt(0.95, df=15)*sd(monday)/sqrt(16)
tue_lcl <- mean(tuesday)-qt(0.95, df=15)*sd(tuesday)/sqrt(16)
wed_lcl <- mean(wednesday)-qt(0.95, df=15)*sd(wednesday)/sqrt(16)
thu_lcl <- mean(thursday)-qt(0.95, df=15)*sd(thursday)/sqrt(16)
fri_lcl <- mean(friday)-qt(0.95, df=15)*sd(friday)/sqrt(16)
sat_lcl <- mean(saturday)-qt(0.95, df=15)*sd(saturday)/sqrt(16)
sun_lcl <- mean(sunday)-qt(0.95, df=15)*sd(sunday)/sqrt(16)

## Print out the revenue that can be counted on with 95% confidence=======================
print(mon_lcl + tue_lcl + wed_lcl + thu_lcl + fri_lcl + sat_lcl + sun_lcl)




















