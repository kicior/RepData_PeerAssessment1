# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(chron)

activity <- read_csv("activity.csv", col_types = list(col_integer(), col_date(), col_integer()))

activitybyday <- activity %>%
      group_by(date) %>%
      summarize(stepstotal = sum(steps))
activitybyinterval <- activity %>%
      group_by(interval) %>%
      summarize(stepsmean = mean(steps, na.rm = TRUE))
```

## What is mean total number of steps taken per day?

```r
activitybyday %>%
      ggplot(aes(x=stepstotal)) +
      geom_histogram(binwidth = 5000, aes(fill=..count..)) +
      labs(x = "Total number of steps per day", y = "Steps count")
```

![](PA1_template_files/figure-html/histogram-1.png) 

```r
summary(activitybyday)
```

```
##       date              stepstotal   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-31   Median :10765  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:13294  
##  Max.   :2012-11-30   Max.   :21194  
##                       NA's   :8
```

```r
activitybyday_mean <- summarize(activitybyday, mean = mean(stepstotal, na.rm = TRUE)) %>%
      as.matrix() %>%
      as.vector() %>%
      as.integer()
activitybyday_median <- summarize(activitybyday, median = median(stepstotal, na.rm = TRUE)) %>%
      as.matrix() %>%
      as.vector()
print(paste("Mean of total number of steps per day (integer): ", activitybyday_mean))
```

```
## [1] "Mean of total number of steps per day (integer):  10766"
```

```r
print(paste("Median of total number of steps per day (integer): ", activitybyday_median))
```

```
## [1] "Median of total number of steps per day (integer):  10765"
```

## What is the average daily activity pattern?

```r
activitybyinterval %>%
      ggplot(aes(x=interval, y=stepsmean)) +
      geom_line() +
      labs(x = "5-min intervals", y = "Mean steps count")
```

![](PA1_template_files/figure-html/activity-1.png) 

```r
summary(activitybyinterval)
```

```
##     interval        stepsmean      
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
activitybyinterval_max <- summarize(activitybyinterval, max = which.max(stepsmean)) %>%
      unlist() %>%
      as.vector() %>%
      activitybyinterval[., 1] %>%
      as.matrix() %>%
      as.vector()
print(paste("Interval ", activitybyinterval_max, " contains the maximum number of steps."))
```

```
## [1] "Interval  835  contains the maximum number of steps."
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset.

```r
print(paste("The original dataset contains ", sum(is.na(activity)), " NAs."))
```

```
## [1] "The original dataset contains  2304  NAs."
```
2. Devise a strategy for filling in all of the missing values in the dataset.
NAs will be replaced with the mean for the 5-minute interval.


```r
activitynona <- activity
activitybyintervalnonapre <- activitynona %>%
      group_by(interval) %>%
      summarize(stepsmean = mean(steps, na.rm = TRUE))
for (i in which(is.na(activitynona)))
      {
      meanline <- which(activitybyintervalnonapre$interval == unlist(activitynona[i, 3]))
      nonamean <- activitybyintervalnonapre[meanline, 2] %>% as.matrix() %>% as.vector()
      activitynona[i, 1] <- nonamean
      }

activitybydaynona <- activitynona %>%
      group_by(date) %>%
      summarize(stepstotal = sum(steps))
activitybydaynona %>%
      ggplot(aes(x=stepstotal)) +
      geom_histogram(binwidth = 5000, aes(fill=..count..)) +
      labs(x = "Total number of steps per day noNA", y = "Steps count")
```

![](PA1_template_files/figure-html/NAfilling-1.png) 

```r
summary(activitybydaynona)
```

```
##       date              stepstotal   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
activitybydaynona_mean <- summarize(activitybydaynona, mean = mean(stepstotal, na.rm = TRUE)) %>%
      as.matrix() %>%
      as.vector() %>%
      as.integer()
activitybydaynona_median <- summarize(activitybydaynona, median = median(stepstotal, na.rm = TRUE)) %>%
      as.matrix() %>%
      as.vector()
print(paste("Mean of total number of steps per day noNA (integer): ", activitybydaynona_mean))
```

```
## [1] "Mean of total number of steps per day noNA (integer):  10766"
```

```r
print(paste("Median of total number of steps per day noNA (integer): ", round(activitybydaynona_median, 0)))
```

```
## [1] "Median of total number of steps per day noNA (integer):  10766"
```
The median increased and the total steps count also increased.

## Are there differences in activity patterns between weekdays and weekends?


```r
activityweekend <- activitynona %>%
      mutate(dayweek = as.factor(ifelse(is.weekend(.$date), "weekend", "weekday"))) %>%
      filter(dayweek == "weekend") %>%
      group_by(interval) %>%
      summarize(stepsmean = mean(steps, na.rm = TRUE)) %>%
      mutate(dayweek = "weekend")
activityweekday <- activitynona %>%
      mutate(dayweek = as.factor(ifelse(is.weekend(.$date), "weekend", "weekday"))) %>%
      filter(dayweek == "weekday") %>%
      group_by(interval) %>%
      summarize(stepsmean = mean(steps, na.rm = TRUE)) %>%
      mutate(dayweek = "weekday")
activitydayweek <- full_join(activityweekday, activityweekend) %>%
      mutate(dayweek = as.factor(dayweek))
activitydayweek %>%
      ggplot(aes(x=interval, y=stepsmean)) +
      geom_line() +
      facet_grid(dayweek~.) +
      labs(x = "5-min intervals", y = "Mean steps count")
```

![](PA1_template_files/figure-html/week-1.png) 
