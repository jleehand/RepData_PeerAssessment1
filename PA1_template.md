---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
df <- tibble::tibble(read.csv("activity.csv"))
df <- transmute(df, steps, ymd(df$date), interval)
df <- rename(df, date = `ymd(df$date)`)
```
## What is mean total number of steps taken per day?

```r
data <- df[!is.na(df$steps),]
data %>% group_by(date) -> result
result %>% summarise(mean_step = mean(steps), median_step = median(steps), total_step = sum(steps)) -> stat_results
print(stat_results$total_step)
```

```
##  [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426 15098
## [13] 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355  2492  6778
## [25] 10119 11458  5018  9819 15414 10600 10571 10439  8334 12883  3219 12608
## [37] 10765  7336    41  5441 14339 15110  8841  4472 12787 20427 21194 14478
## [49] 11834 11162 13646 10183  7047
```

```r
ggplot(data = stat_results, aes(x = total_step)) + geom_histogram(bins = 20) + theme_classic() +
labs(x = "Number of Steps", y = "Frequency", title = "Histogram of Total Number of Steps-per-day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Median Number of Steps

```r
print(stat_results$median_step)
```

```
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [39] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```

#### Mean Number of Steps

```r
print(stat_results$mean_step)
```

```
##  [1]  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 38.2465278
##  [7] 44.4826389 34.3750000 35.7777778 60.3541667 43.1458333 52.4236111
## [13] 35.2048611 52.3750000 46.7083333 34.9166667 41.0729167 36.0937500
## [19] 30.6284722 46.7361111 30.9652778 29.0104167  8.6527778 23.5347222
## [25] 35.1354167 39.7847222 17.4236111 34.0937500 53.5208333 36.8055556
## [31] 36.7048611 36.2465278 28.9375000 44.7326389 11.1770833 43.7777778
## [37] 37.3784722 25.4722222  0.1423611 18.8923611 49.7881944 52.4652778
## [43] 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 50.2708333
## [49] 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
```

## What is the average daily activity pattern?

```r
result %>% group_by(interval) %>% summarise(mean_steps = mean(steps)) -> interval_steps
plot(interval_steps$interval, interval_steps$mean_steps, type = "l", main = "Average of Number of Steps Taken Per 5-Minute Time Interval", xlab = "Five Minute Time Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Which 5-minute interval, on average, contains the maximum number of steps? 


```r
interval_steps$interval[which.max(interval_steps$mean_steps)]
```

```
## [1] 835
```

## Imputing missing values
#### How many total NA values?


```r
apply(is.na(df), MARGIN = 2, sum)
```

```
##    steps     date interval 
##     2304        0        0
```

#### Replace the NA values of steps with the mean of its interval


```r
df_impute <- data.table::copy(df)
na_indices <- is.na(df_impute$steps)
matched_indices <- match(df_impute$interval[na_indices], interval_steps$interval)
df_impute$steps[na_indices] <- interval_steps$mean_steps[matched_indices]

df_impute %>% group_by(date) -> result
result %>% summarise(mean_step = mean(steps), median_step = median(steps), total_step = sum(steps)) -> stat_results
ggplot(data = stat_results, aes(x = total_step)) + geom_histogram(bins = 20) + theme_classic() +labs(x = "Number of Steps", y = "Frequency", title = "Histogram of Total Number of Steps-per-day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### Median Number of Steps

The median changes dramatically for some dates in this imputation scheme, as the NA values are concentrated in particular days. This means replacement of NA values with the mean for the interval changes all the values for those days and changes the median. When dropping the NA values it also drops entire days out the original dataset. Given that the medians for the given data are all zero, these imputed values are probably not correct. 


```r
print(stat_results$median_step)
```

```
##  [1] 34.11321  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000 34.11321
##  [9]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [17]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [25]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000 34.11321
## [33]  0.00000  0.00000 34.11321  0.00000  0.00000  0.00000  0.00000 34.11321
## [41] 34.11321  0.00000  0.00000  0.00000 34.11321  0.00000  0.00000  0.00000
## [49]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
## [57]  0.00000  0.00000  0.00000  0.00000 34.11321
```

#### Mean Number of Steps

The changes to most means are small. Like the median above, the days that were dropped in the original analysis are now in the imputed dataset. These obviously cause some changes in the total number of steps and mostly add days that are "the average" of the original dateset. In the histogram this can be seen as an increase bins that were already high frequency. 


```r
print(stat_results$mean_step)
```

```
##  [1] 37.3825996  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667
##  [7] 38.2465278 37.3825996 44.4826389 34.3750000 35.7777778 60.3541667
## [13] 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667
## [19] 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167
## [25]  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500
## [31] 53.5208333 37.3825996 36.8055556 36.7048611 37.3825996 36.2465278
## [37] 28.9375000 44.7326389 11.1770833 37.3825996 37.3825996 43.7777778
## [43] 37.3784722 25.4722222 37.3825996  0.1423611 18.8923611 49.7881944
## [49] 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778
## [55] 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
## [61] 37.3825996
```


## Are there differences in activity patterns between weekdays and weekends?


```r
is_weekend <- weekdays(df_impute$date) %in% c("Saturday", "Sunday")
df_impute$day <- factor(as.integer(is_weekend), labels = c("weekday", "weekend"))

df_impute %>% group_by(day, interval) %>% summarise(mean_int = mean(steps)) -> result
```

```
## `summarise()` has grouped output by 'day'. You can override using the `.groups` argument.
```

```r
result %>% filter(day == "weekday") -> result_weekday
result %>% filter(day == "weekend") -> result_weekend

par(mfcol = c(2, 1))
plot(result_weekend$interval, result_weekend$mean_int, type = "l", main = "Weekend", xlab = "Interval", ylab = "Number of steps")
plot(result_weekend$interval, result_weekday$mean_int, type = "l", main = "Weekday", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
