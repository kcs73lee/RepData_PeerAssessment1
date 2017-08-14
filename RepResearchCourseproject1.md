---
title: "RepResearchCourseProject1"
author: "K C Saelee"
date: "August 9, 2017"
output: html_document

---

What is  the mean total number of steps taken per day?

Load and read dataset
```{r, echo=TRUE}
##unzipped file in working directory. Made wd the folder. 
act<- read.csv("activity.csv")
head(act)
stepsbyday<-aggregate(steps~date, act, sum)
```
Create histogram of steps taken per day
```{r, echo=TRUE}
hist(stepsbyday$steps)
```
Calculate mean and median steps taken per day
```{r, echo=TRUE}
meansteps<-mean(stepsbyday$steps)
mediansteps<-median(stepsbyday$steps)
```


What is the average daily activity pattern?
```{r, echo=TRUE}
steppattern<-aggregate(steps~interval, act, mean)
plot2<-plot(steppattern$interval, steppattern$steps, type ="l", main="Average daily activity pattern")
print(plot2)
```
Which 5 minute interval has max number of steps?
```{r, echo=TRUE}
which.max(steppattern$steps)
```
Imputting missing values
```{r, echo=TRUE}
##calculate total missing values
sum(is.na(act$steps))
```
##create new data set with missing values filled in
```{r, echo=TRUE}
act2<-act
missval<-is.na(act2$steps)
actinterval<-tapply(act2$steps, act2$interval, mean, na.rm=TRUE) ##used mean to fill
act2$steps[missval]<-actinterval[as.character(act2$interval[missval])]
head(act2)
```
##make histogram and compute mean and median
```{r, eacho=TRUE}
stepsbyday2<-aggregate(steps~date, act2, sum)
plot3<-hist(stepsbyday2$steps)
print(plot3)
```
```{r,echo=TRUE}
meansteps2<-mean(stepsbyday2$steps)
mediansteps2<-median(stepsbyday2$steps)
## > meansteps2
##[1] 10766.19
##> mediansteps2
##[1] 10766.19
## mean and median steps are lower than previous pre-imputed dataset
```
Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
weekdayact<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekendact<-c("Saturday","Sunday")
act2$week<-ifelse(is.element(weekdays(as.Date(act2$date))
                     ,weekdayact), "Weekdayact", "Weekendact")
act3<-aggregate(steps~interval+week,act2, mean)
head(act3)
```
Plot
```{r, echo=TRUE}
library(ggplot2)
weekplot<-ggplot(act3, aes(interval, steps,fill=week))+
  geom_line()+
  facet_wrap(~week,nrow=2)+ ggtitle("Average daily steps by week type")
print(weekplot)
```
