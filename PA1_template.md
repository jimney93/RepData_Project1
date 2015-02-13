# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(dplyr)
library(ggplot2)
df <- summarize(na.omit(data) %>% group_by(date), total.steps = sum(steps))
qplot(date, total.steps, data = df, stat = "identity", geom = "histogram") +
    geom_histogram(binwidth = 10) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5))
```

![](PA1_template_files/figure-html/total number of steps-1.png) 

```r
avg <- mean(df$total.steps, na.rm=T)
print(paste("the mean is ", avg))
```

```
## [1] "the mean is  10766.1886792453"
```

```r
med <- median(df$total.steps, na.rm=T)
print(paste("the median is ", med))
```

```
## [1] "the median is  10765"
```

## What is the average daily activity pattern?

```r
library(dplyr)
library(ggplot2)
df <- summarize(na.omit(data) %>% group_by(interval), avg.steps = mean(steps))

qplot(interval, avg.steps, data = df, geom = "line") +
    scale_x_continuous(breaks = seq(0, 3000, by = 100)) +
    scale_y_continuous(breaks = seq(0, 250, by = 25)) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
```

![](PA1_template_files/figure-html/average steps-1.png) 

```r
x <- df[which.max(df$avg.steps),]
sprintf("The 5 minute interval %.0f has the max average number of steps %f", x$interval, x$avg.steps)
```

```
## [1] "The 5 minute interval 835 has the max average number of steps 206.169811"
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?


