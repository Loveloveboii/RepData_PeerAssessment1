---
title: "PA1_template"
author: "Katarzyna Turkiewicz-Cisek"
date: "February 28, 2016"
output: html_document
---

```r
library('ggplot2')
library('plyr')
library('dplyr')
```
## Introduction 
In order to answer all questions we need to take the following steps:

1. Loading and preprocessing the data

Read data file

```r
data <- read.csv("activity.csv", ",", header=TRUE)
```

2. What is mean of the total number of steps taken per day?

Ignore all NAs

```r
d2 <- data[complete.cases(data$steps),]
```

Compute Total Steps per Day

```r
TotalStepsPerDay <- aggregate(x = list(Steps = d2$steps), by = list(Date=d2$date), FUN = sum)
```

Create histogram showing the total number of steps taken each day

```r
hist(TotalStepsPerDay$Steps,
  main="Histogram of Total Steps per Day", 
  xlab="Steps", 
  border="black", 
  col="green",
  las=1, 
  breaks=10)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" title="" alt="" width="672" />

Compute mean of the total number of steps taken per day

```r
MeanOfSteps <- mean(TotalStepsPerDay$Steps)
```
Mean: 10766.19

Compute median of the total number of steps taken per day

```r
MedianOfSteps <- median(TotalStepsPerDay$Steps)
```
Median: 10765

3. What is the average daily activity pattern?

Create the subset with mean for each interval

```r
IntervalSet <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
```

Create the timeseries plot showing 5 minutes intervals and the steps taken

```r
ggplot(IntervalSet, aes(x=interval, y=steps)) +
  geom_line(color = "black")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" width="672" />

Figure out the maximum steps, on average, across all the days

```r
IntervalSet[which.max(IntervalSet$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```
The interval with maximum steps is interval 835 with 206.1698 steps

4. Imputing missing values

Find out the number of NAs

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
Number of NAs: 2304

Fill in missing NAs

```r
modified_data <- data
NAs <- is.na(modified_data$steps)
avg_interval <- tapply(modified_data$steps, modified_data$interval, mean, na.rm = TRUE, simplify = TRUE)
modified_data$steps[NAs] <- avg_interval[as.character(modified_data$interval[NAs])]
```

Create aggregation similar to the 1st example in order to compare mean, median and create histogram with replaced missing values


```r
TotalStepsPerDayModified <- aggregate(x = list(Steps = modified_data$steps), by = list(Date=modified_data$date), FUN = sum)
```

Compute mean of steps with replaced NAs (similar to the one where we excluded NA values)


```r
MeanOfStepsModified <- mean(TotalStepsPerDayModified$Steps)
```

Compute median of steps with replaced NAs (same as the one where we excluded NA values)


```r
MedianOfStepsModified <- median(TotalStepsPerDayModified$Steps)
```

Both mean andmedian are now the same values (10766)

Create histogram showing the modified data (missing values replaced by averages)


```r
hist(TotalStepsPerDayModified$Steps,
     main="Histogram of Steps per Day with missing values replaced", 
     xlab="Steps", 
     border="black", 
     col="blue",
     las=1, 
     breaks=10)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-16-1.png" title="" alt="" width="672" />


5. Are there differences in activity patterns between weekdays and weekends?

Create a new column indicating if the day is a weekday or weekend


```r
modified_data <- mutate(modified_data, weektype = ifelse(weekdays(as.Date(modified_data$date)) == "Saturday" | weekdays(as.Date(modified_data$date)) == "Sunday", "weekend", "weekday"))
```

Compute mean of steps in the 5-minute interval


```r
interval_modified_data <- modified_data %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
```

Make the time series of the 5-minute interval for weekday and weekend

```r
ggplot(interval_modified_data, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-19-1.png" title="" alt="" width="672" />

Compare the averages
When we look at the plots we can observe that there is more activity during the early morning during the weekdays than during the weekend, but in general there is more activity during the weekend.
