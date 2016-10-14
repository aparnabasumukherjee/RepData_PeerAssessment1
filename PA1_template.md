## Introduction


title: "activity"
author: "Aparna"
date: "September 30, 2016"
output: html_document
---

Reading CSV file and checking it

Loading and preprocessing the data

```{r}
activitydata <- read.table("D:/R learning/rep research/activity.csv", sep =  "," , header = TRUE) 
head(activitydata)
str(activitydata)

```
What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
```{r}
stepsByDay <- tapply(activitydata$steps, activitydata$date, sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day
```{r, echo=TRUE}
hist(stepsByDay)
```

3.Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(stepsByDay)
median(stepsByDay)
```

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
stepsInterval <- aggregate(steps ~ interval, data = activitydata, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l")
maxsteps <- max(stepsInterval$steps)

maxdata<- stepsInterval[stepsInterval$steps == maxsteps, ]
intervalmax <- maxdata[ ,1]
intervalmax
```

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
missdata<- activitydata[activitydata$steps == "NA", ]
nrow(missdata)
```
2. Filling in all of the missing values in the dataset by the mean for that 5-minute interval & create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
combo<- merge(x = activitydata, y = stepsInterval, by = "interval", all.x = TRUE)
combo$steps.x [is.na(combo$steps.x)] <- combo$steps.y[is.na(combo$steps.x)]
str(combo)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
stepsByDaynew <- tapply(combo$steps.x, combo$date, sum, na.rm=TRUE)
hist(stepsByDaynew)
mean(stepsByDaynew)
median(stepsByDaynew)
```
Mean and Median have gone up and are equal to each other.

Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
tt<- as.POSIXlt(as.Date(combo$date))
combo$dateType <-  ifelse(tt$wday %in% c(0,6), 'weekend', 'weekday')
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
```{r}
comboaverage <- aggregate(steps.x ~ interval + dateType, data=combo, mean)
library(lattice)
xyplot(steps.x ~ interval | factor(dateType), data = comboaverage, aspect = 1/2, 
    type = "l")
```---
title: "activity"
author: "Aparna"
date: "September 30, 2016"
output: html_document
---

Reading CSV file and checking it

Loading and preprocessing the data

```{r}
activitydata <- read.table("D:/R learning/rep research/activity.csv", sep =  "," , header = TRUE) 
head(activitydata)
str(activitydata)

```
What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
```{r}
stepsByDay <- tapply(activitydata$steps, activitydata$date, sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day
```{r, echo=TRUE}
hist(stepsByDay)
```

3.Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(stepsByDay)
median(stepsByDay)
```

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
stepsInterval <- aggregate(steps ~ interval, data = activitydata, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l")
maxsteps <- max(stepsInterval$steps)

maxdata<- stepsInterval[stepsInterval$steps == maxsteps, ]
intervalmax <- maxdata[ ,1]
intervalmax
```

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
missdata<- activitydata[activitydata$steps == "NA", ]
nrow(missdata)
```
2. Filling in all of the missing values in the dataset by the mean for that 5-minute interval & create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
combo<- merge(x = activitydata, y = stepsInterval, by = "interval", all.x = TRUE)
combo$steps.x [is.na(combo$steps.x)] <- combo$steps.y[is.na(combo$steps.x)]
str(combo)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
stepsByDaynew <- tapply(combo$steps.x, combo$date, sum, na.rm=TRUE)
hist(stepsByDaynew)
mean(stepsByDaynew)
median(stepsByDaynew)
```
Mean and Median have gone up and are equal to each other.

Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
tt<- as.POSIXlt(as.Date(combo$date))
combo$dateType <-  ifelse(tt$wday %in% c(0,6), 'weekend', 'weekday')
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
```{r}
comboaverage <- aggregate(steps.x ~ interval + dateType, data=combo, mean)
library(lattice)
xyplot(steps.x ~ interval | factor(dateType), data = comboaverage, aspect = 1/2, 
    type = "l")
```

---
title: "activity"
author: "Aparna"
date: "September 30, 2016"
output: html_document
---

Reading CSV file and checking it

Loading and preprocessing the data

```{r}
activitydata <- read.table("D:/R learning/rep research/activity.csv", sep =  "," , header = TRUE) 
head(activitydata)
str(activitydata)

```
What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
```{r}
stepsByDay <- tapply(activitydata$steps, activitydata$date, sum, na.rm=TRUE)
```

2. Make a histogram of the total number of steps taken each day
```{r, echo=TRUE}
hist(stepsByDay)
```

3.Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(stepsByDay)
median(stepsByDay)
```

What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
stepsInterval <- aggregate(steps ~ interval, data = activitydata, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l")
maxsteps <- max(stepsInterval$steps)

maxdata<- stepsInterval[stepsInterval$steps == maxsteps, ]
intervalmax <- maxdata[ ,1]
intervalmax
```

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
missdata<- activitydata[activitydata$steps == "NA", ]
nrow(missdata)
```
2. Filling in all of the missing values in the dataset by the mean for that 5-minute interval & create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
combo<- merge(x = activitydata, y = stepsInterval, by = "interval", all.x = TRUE)
combo$steps.x [is.na(combo$steps.x)] <- combo$steps.y[is.na(combo$steps.x)]
str(combo)
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
stepsByDaynew <- tapply(combo$steps.x, combo$date, sum, na.rm=TRUE)
hist(stepsByDaynew)
mean(stepsByDaynew)
median(stepsByDaynew)
```
Mean and Median have gone up and are equal to each other.

Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
tt<- as.POSIXlt(as.Date(combo$date))
combo$dateType <-  ifelse(tt$wday %in% c(0,6), 'weekend', 'weekday')
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
```{r}
comboaverage <- aggregate(steps.x ~ interval + dateType, data=combo, mean)
library(lattice)
xyplot(steps.x ~ interval | factor(dateType), data = comboaverage, aspect = 1/2, 
    type = "l")
```
