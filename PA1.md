# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

1 Load the data (i.e. read.csv())


```r
activityData <- read.csv(file="C:/Users/Olga/Desktop/activity.csv");
```

2 Process/transform the data (if necessary) into a format suitable for your analysis



```r
lapply(activityData, class)
```

```
## $steps
## [1] "integer"
## 
## $date
## [1] "factor"
## 
## $interval
## [1] "integer"
```

```r
activityData$date <- as.Date(activityData$date)

class(activityData$date)
```

```
## [1] "Date"
```

```r
dataFiltered <- activityData[!is.na(activityData$steps),]
dataRaw      <- activityData
```

## What is mean total number of steps taken per day?


1 Make a histogram of the total number of steps taken each day


```r
data <- dataFiltered 

hist(data$steps, col="red", main="Steps per day" , xlab="steps taken");
```

![plot of chunk unnamed-chunk-3](./PA1_files/figure-html/unnamed-chunk-3.png) 

2 Calculate and report the mean and median total number of steps taken per day


```r
c( mean(data$steps) , median(data$steps))
```

```
## [1] 37.38  0.00
```

## What is the average daily activity pattern?


1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
intervalData <- data.frame(interval = unique(activityData$interval));

for (i in 1:nrow(intervalData)) {
  subset <- data[data$interval == data[i,"interval"],];
  intervalData[i,"mean_all_days"] <- mean(subset$steps)
};

head(intervalData);
```

```
##   interval mean_all_days
## 1        0       1.71698
## 2        5       0.33962
## 3       10       0.13208
## 4       15       0.15094
## 5       20       0.07547
## 6       25       2.09434
```

```r
plot(intervalData$mean_all_days
  , type="l"
  , xlab="interval"
  , ylab="mean(steps per day");
```

![plot of chunk unnamed-chunk-5](./PA1_files/figure-html/unnamed-chunk-5.png) 

2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalData[which.max(intervalData$mean_all_days),]
```

```
##     interval mean_all_days
## 104      835         206.2
```

## Imputing missing values


1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(dataRaw[is.na(dataRaw$steps),])
```

```
## [1] 2304
```

2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
filler <- data.frame(dataRaw[0,]);
filler <- rbind(filler, dataRaw)

for (i in 1:nrow(intervalData) ) {
  interval <- intervalData[i,"interval"]
  meanVal <- intervalData[i,"mean_all_days"]
    
  rows <- which(is.na(filler$steps))
  filler[rows, "steps"] <- meanVal
}
```

3 Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data <- filler
```

4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(data$steps, col="red", main="Steps per day (filler)" , xlab="steps");
```

![plot of chunk unnamed-chunk-10](./PA1_files/figure-html/unnamed-chunk-10.png) 

## Are there differences in activity patterns between weekdays and weekends?


```r
c( mean(data$steps) , median(data$steps))
```

```
## [1] 32.71  0.00
```

1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
isWeekend <- function(x) {
  if( x == "Sunday" || x == "Saturday") "weekend" else "weekday";
}

data$day <- weekdays(data$date)
data$dayType <- lapply(data$day, isWeekend)

data$dayType <- sapply(data$dayType, as.factor)

print(lapply(data, class))
```

```
## $steps
## [1] "numeric"
## 
## $date
## [1] "Date"
## 
## $interval
## [1] "integer"
## 
## $day
## [1] "character"
## 
## $dayType
## [1] "factor"
```

2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library("lattice")

dayType <- data$dayType

xyplot( steps~interval | dayType, data=data, type="l", layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](./PA1_files/figure-html/unnamed-chunk-13.png) 
