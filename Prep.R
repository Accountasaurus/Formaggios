## Clean up the working Environment =======================================================
rm(list = ls())
gc()
library(ggplot2)
library(reshape2)
library("plyr", lib.loc="~/R/win-library/3.1")
library("WriteXLS", lib.loc="~/R/win-library/3.1")

## Import the sales item file and prepare for analysis
prep <- read.csv("Items Sold - Nov '14 - Feb '15.csv")
prep[,1] <- as.character(prep[,1])
prep[,1] <- as.Date(prep[,1], "%m/%d/%Y")
prep[,18] <- weekdays(as.Date(prep[,1]))
prep[,1] <- NULL
prep.melted <- melt(prep)
subsets <- split(prep, prep[,17], drop=TRUE)
friday <- subsets[[1]]
monday <- subsets[[2]]
saturday <- subsets[[3]]
sunday <- subsets[[4]]
thursday <- subsets[[5]]
tuesday <- subsets[[6]]
wednesday <- subsets[[7]]


## Analysis of friday
n <- 21
friday[,17] <- NULL
mean<-colMeans (friday, na.rm = FALSE, dims = 1)
sd<- apply(friday, 2, sd)
confidence <- qt(.95, df=20)* (sd/sqrt(21))
friday[22,] <- mean + confidence
write.csv(friday, "friday.csv")

## Analysis of Monday
n <- 21
monday[,17] <- NULL
mean<-colMeans (monday, na.rm = FALSE, dims = 1)
sd<- apply(monday, 2, sd)
confidence <- qt(.95, df=20)* (sd/sqrt(21))
monday[22,] <- mean + confidence

## Analysis of Tuesday
n <- 17
tuesday[,17] <- NULL
mean<-colMeans (tuesday, na.rm = FALSE, dims = 1)
sd<- apply(tuesday, 2, sd)
confidence <- qt(.95, df=16)* (sd/sqrt(17))
tuesday[18,] <- mean + confidence

## Analysis of wednesday
n <- 17
wednesday[,17] <- NULL
mean<-colMeans (wednesday, na.rm = FALSE, dims = 1)
sd<- apply(wednesday, 2, sd)
confidence <- qt(.95, df=16)* (sd/sqrt(17))
wednesday[18,] <- mean + confidence

##Analysis of Thursday
n <- 17
thursday[,17] <- NULL
mean<-colMeans (thursday, na.rm = FALSE, dims = 1)
sd<- apply(thursday, 2, sd)
confidence <- qt(.95, df=16)* (sd/sqrt(17))
thursday[18,] <- mean + confidence

##Analysis of Saturday
n <- 22
saturday[,17] <- NULL
mean<-colMeans (saturday, na.rm = FALSE, dims = 1)
sd<- apply(saturday, 2, sd)
confidence <- qt(.95, df=21)* (sd/sqrt(22))
saturday[23,] <- mean + confidence
write.csv(saturday, "saturday.csv")

##Analysis of Sunday
n <- 22
sunday[,17] <- NULL
mean<-colMeans (sunday, na.rm = FALSE, dims = 1)
sd<- apply(sunday, 2, sd)
confidence <- qt(.95, df=21)* (sd/sqrt(22))
sunday[23,] <- mean + confidence
write.csv(sunday, "sunday.csv")