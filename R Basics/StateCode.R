
# Review of R basics
#Subsetting
statesInfo <- read.csv("stateData.csv")
stateSubset <- subset(statesInfo, state.region == 1)
#bracket method
stateBracketSubset <- statesInfo[statesInfo$state.region == 1, ]
stateSubset
stateBracketSubset

MurderOverTen <- subset(statesInfo, murder > 10)
MurderOverTen

IlliteracyOver2 <- subset(statesInfo, illiteracy > 2)
IlliteracyOver2

install.packages('knitr', dependencies = T) 
library(knitr)

#Import reddit survey dataset

setwd("/Users/Admin/Documents/UdacityData")
reddit <- read.csv("reddit.csv")
str(reddit)

levels(reddit$age.range)
library(ggplot2)
qplot(reddit$age.range)

#ordered factor = age and income
qplot(data=reddit, x=income.range)
#order varables