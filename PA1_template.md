---
title: "PA1"
author: "Ryan Emerson"
date: "Sunday, April 12, 2015"
output: html_document
---

# PA1 - Reproducible Research


```r
library(dplyr)
activity <- read.csv("C:\\Users\\Ryan\\Documents\\Coursera - Data Science\\Reproducible Research\\repdata_data_activity\\activity.csv")
```

## Total Steps

1. Calculate the total number of steps taken each day.

```r
stepsperday <- activity %>%
               group_by(date) %>%
               summarize(stepsperday = sum(steps))
```


2. Make a histogram of the total number of steps taken each day.

```r
hist(stepsperday$stepsperday, main = "Histogram of Steps per Day",
                              xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
paste("The mean number of steps per day is", mean(stepsperday$stepsperday, na.rm=TRUE))
```

```
## [1] "The mean number of steps per day is 10766.1886792453"
```

```r
paste("The median number of steps per day is", median(stepsperday$stepsperday, na.rm = TRUE))
```

```
## [1] "The median number of steps per day is 10765"
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
int <- activity %>%
       group_by(interval) %>%
       summarize(stepsperint = mean(steps, na.rm=TRUE))
plot(int$interval, int$stepsperint, type="l",
     main="Average Steps per Interval",
     xlab="Interval (5 mins)",
     ylab="Average Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
    
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxint <- filter(int,stepsperint == max(stepsperint))
maxint <- select(maxint, interval)
paste("The interval with the maximum number of steps is", maxint)
```

```
## [1] "The interval with the maximum number of steps is 835"
```

# Imputing Missing Values

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I would perform mean imputation within intervals to account for the missing data.

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newactivity <- activity
newactivity$steps[is.na(newactivity$steps)] <-  mean(newactivity$steps, na.rm=TRUE)
```

4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
    

```r
newstepsperday <- newactivity %>%
                  group_by(date) %>%
                  summarize(stepsperday = sum(steps))
hist(newstepsperday$stepsperday, main = "Histogram of Steps per Day",
                                 xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
paste("The mean number of steps per day is", round(mean(newstepsperday$stepsperday, na.rm=TRUE),2))
```

```
## [1] "The mean number of steps per day is 10766.19"
```

```r
paste("The median number of steps per day is", median(newstepsperday$stepsperday, na.rm = TRUE))
```

```
## [1] "The median number of steps per day is 10766.1886792453"
```

```r
mean(stepsperday$stepsperday, na.rm=TRUE) - mean(newstepsperday$stepsperday)
```

```
## [1] 0
```

```r
median(stepsperday$stepsperday, na.rm=TRUE) - median(newstepsperday$stepsperday)
```

```
## [1] -1.188679
```

Yes, these values differ from the estimates from the first part of the assignment.

Imputing missing data on the estimates of the total daily number of steps does not change the average number of steps per day. However, it does increase the median number of steps per day by 1.189 steps per day.


## Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
newactivity$date <- as.Date(newactivity$date)
newactivity$weekday <- weekdays(newactivity$date)
newactivity$daytype <- as.factor(ifelse(newactivity$weekday == "Saturday" |
                                        newactivity$weekday == "Sunday",
                                        "weekend", "weekday"))
```

2.    Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
weekdays <- newactivity %>%
            filter(daytype == "weekday") %>%
            group_by(interval) %>%
            summarize(stepsperint = mean(steps))
weekends <- newactivity %>%
            filter(daytype == "weekend") %>%
            group_by(interval) %>%
            summarize(stepsperint = mean(steps))
            
par <- par(mfrow = c(2, 1))               
plot(weekdays$interval, weekdays$stepsperint, type="l", 
     main = "Timeline of Steps per Interval (Weekdays)",
     xlab = "Interval", ylab="Steps")
plot(weekends$interval, weekends$stepsperint, type="l",  
     main = "Timeline of Steps per Interval (Weekends)",
     xlab = "Interval", ylab="Steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
