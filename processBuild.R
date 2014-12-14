## Loading and preprocessing the data

options(scipen = 1, digits = 4)

rm(list=ls())
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(tidyr)
library(dplyr)

if (!file.exists("activity.csv")){
  print("Unzipping Data")
  unzip("activity.zip", exdir=".")
}

dat <- read.csv("activity.csv")
dat$date <- ymd(dat$date)



## What is mean total number of steps taken per day?

totalSteps <- as.numeric(tapply(dat$steps,dat$date,sum,na.rm=F))
day <- unique(dat$date)
byDay <- data.frame(day,totalSteps)
rm(list=c("day","totalSteps"))

hist(byDay$totalSteps[!is.na(byDay$totalSteps)], 
     breaks=10,
     xlab="Steps Per day",
     ylab="Frequency",
     main="Distribution of Total Steps Per Day",
     col="red")

# ggplot(byDay, aes(x=day,y=totalSteps)) + 
#     geom_bar(stat="identity") + 
#     xlab("Date") + 
# 	  ylab("Total Steps") + 
# 	  ggtitle("Total Steps By Day") +
#   	scale_x_datetime(labels = date_format("%b %d"), breaks = date_breaks("day")) +
#   	theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

meanByDay <- mean(byDay$totalSteps,na.rm=T)
medianByDay <- median(byDay$totalSteps,na.rm=T)


## What is the average daily activity pattern?

byInterval <- ddply(dat,~interval,summarize,meanSteps=mean(steps,na.rm=T)) %>%
  mutate(hhmm = formatC(interval,width=4,flag="0")) %>%
  separate(hhmm,into=c("hr","min"),sep=2)
byInterval <- mutate(byInterval,time=min(dat$date)+minutes(min)+hours(hr))
# print(head(byInterval))

ggplot(byInterval, aes(x=time,y=meanSteps)) +
  geom_line() +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour"))

# find maximum line
maxPoint <- byInterval[byInterval$meanSteps==max(byInterval$meanSteps),]
maxTime <- strftime(maxPoint$time, format="%I:%M %p",tz="UTC")




## Imputing missing values

# number of na's in all lines
colNAs <- colSums(is.na(dat))
totalNA <- sum(is.na(dat))

# create new data set for fixe data
datFix <- dat

## replace NAs with mean of all days for that time interval
naRows <- which(is.na(dat$steps)) # which rows have NA
naIntervals <- dat$interval[naRows] # what are the intervals for these NA rows
# select mean values by matching missing interval with those in byInterval mean table 
datFix$steps[naRows] <- byInterval$meanSteps[match(naIntervals,byInterval$interval)] 

# make new daily sums
totalSteps <- as.numeric(tapply(datFix$steps,datFix$date,sum,na.rm=T))
day <- unique(dat$date)
byDayFix <- data.frame(day,totalSteps)
rm(list=c("day","totalSteps"))

hist(byDayFix$totalSteps[!is.na(byDayFix$totalSteps)], 
     breaks=10,
     xlab="Steps Per day",
     ylab="Frequency",
     main="Distribution of Total Steps Per Day With Fixed Data",
     col="blue")

# ggplot(byDayFix, aes(x=day,y=totalSteps)) + 
#     geom_bar(stat="identity") + 
#     xlab("Date") + 
#     ylab("Total Steps") + 
# 	  ggtitle("Total Steps By Day") +
#   	scale_x_datetime(labels = date_format("%b %d"), breaks = date_breaks("day")) +
#   	theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

meanByDay <- mean(byDay$totalSteps,na.rm=T)
medianByDay <- median(byDay$totalSteps,na.rm=T)


## Are there differences in activity patterns between weekdays and weekends?

weekends <- which(weekdays(datFix$date)=="Saturday"|weekdays(datFix$date)=="Sunday")
datFix$day <- weekdays(datFix$date)
datFix$weekday <- "weekday"
datFix$weekday[weekends] <- "weekend"

weekendSummary <- ddply(datFix,interval~weekday,summarize,meanSteps=mean(steps,na.rm=T)) %>%
  mutate(hhmm = formatC(interval,width=4,flag="0")) %>%
  separate(hhmm,into=c("hr","min"),sep=2) %>%
  mutate(time=min(dat$date)+minutes(min)+hours(hr))

ggplot(weekendSummary, aes(time, meanSteps)) +
  geom_line() + 
  facet_grid(. ~ weekday) +
  xlab("Time") + 
  ylab("Total Steps") + 
  ggtitle("Total Steps By Day") +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour"))