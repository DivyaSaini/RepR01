Peer Assessment 1
=================


### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

### Load and process the data

```r
filename <- "./data/activity.csv"
activityData <- read.csv(filename, colClasses = c("integer", "Date", "factor"))
CompleteData <- na.omit(activityData)
rownames(CompleteData) <- 1:nrow(CompleteData)
dim(activityData)
```

```
## [1] 17568     3
```

```r
dim(CompleteData)
```

```
## [1] 15264     3
```

```r
library(ggplot2)
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
myplot1 <- ggplot(CompleteData, aes(date, steps))
myplot1 <- myplot1 + geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.7) 
myplot1 <- myplot1 + labs(title = "Histogram for total number of steps taken each day", x = "Date", y = "Total number of steps")
myplot1
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
totalSteps <- aggregate(CompleteData$steps, list(Date = CompleteData$date), FUN = "sum")$x
mean(totalSteps)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(totalSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps <- aggregate(CompleteData$steps, list(interval = as.numeric(as.character(CompleteData$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

myplot2 <- ggplot(avgSteps, aes(interval, meanOfSteps))
myplot2 <- myplot2 + geom_line(color = "steelblue", size = 0.8)
myplot2 <- myplot2 + labs(title = "Time-series plot of five minute interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
myplot2
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```

### Imput missing values
* The total number of rows with NAs:


```r
sum(is.na(activityData))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData <- activityData 
for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
        newData$steps[i] <- avgSteps[which(newData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(newData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
sum(is.na(newData))
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
myplot3 <- ggplot(newData, aes(date, steps)) 
myplot3 <- myplot3 + geom_bar(stat = "identity",
                                             colour = "cyan",
                                             fill = "cyan",
                                             width = 0.7) 
myplot3 <- myplot3 + labs(title = "Histogram for total number of steps taken each day (no missing data)", x = "Date", y = "Total number of steps")
myplot3
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalSteps <- aggregate(newData$steps, 
                           list(Date = newData$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalSteps)
newMedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```
On filling in the missing, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(newData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(newData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(newData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(newData$steps, 
                      list(interval = as.numeric(as.character(newData$interval)), 
                           weekdays = newData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"
library(lattice)
myplot4 <- xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
myplot4
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
