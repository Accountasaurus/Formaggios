## Clean up the working Environment =======================================================
rm(list = ls())
gc()
library(ggplot2)
library(reshape2)
library("plyr", lib.loc="~/R/win-library/3.1")

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


## Analysis of Friday
n <- 17
friday[,17] <- NULL
fridaymean<-colMeans (friday, na.rm = FALSE, dims = 1)
fridaysd<- apply(friday, 2, sd)
confidence <- qt(.95, df=16)* (fridaysd/sqrt(17))
friday[18,] <- fridaymean + confidence




