# Reproducible Research: Peer Assessment 1


```r
## Loading and preprocessing the data
setwd("/Users/nicholashassan/Reproducible Research")
original <- read.csv("activity.csv")
data <- read.csv("activity.csv")

for (i in 1:length(data$steps)) {
  if (is.na(data$steps[i])) {
    data$steps[i] = 0
  }
}

datesteps <- split(data, data$date)
totalsteps <- rep(NA, 61)
date <- rep(NA, 61)
for (i in 1:length(datesteps)) {
  date[i] = names(datesteps)[i]
  totalsteps[i] = (sum((datesteps[[i]][[1]])))
}


## What is mean total number of steps taken per day?
hist(totalsteps, main = "Frequency of total steps in one day", xlab = "Total steps in one day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)

```r
mean(totalsteps) #9354.23
```

```
## [1] 9354.23
```

```r
median(totalsteps) #10395
```

```
## [1] 10395
```

```r
## What is the average daily activity pattern?
intsteps <- split(data, data$interval)
totalsteps <- rep(NA, 288)
interval <- rep(NA, 288)
for (i in 1:length(intsteps)) {
  interval[i] = names(intsteps)[i]
  totalsteps[i] = (sum(intsteps[[i]][,1]))/length(intsteps[[i]][,1])
}

plot(interval, totalsteps, type = "l", main = "Mean number of steps per interval across 61 days", xlab = "Time Intervals (5 Minutes)", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)

```r
#Interval 104 has the most steps
which.max(totalsteps)
```

```
## [1] 104
```

```r
totalsteps[which.max(totalsteps)]
```

```
## [1] 179.1311
```

```r
## Imputing missing values
nomissing <- read.csv("activity.csv")

#Number of FALSE equals rows that have NA
table(complete.cases(nomissing))
```

```
## 
## FALSE  TRUE 
##  2304 15264
```

```r
combinedintstep <- data.frame(cbind(totalsteps, interval))
combinedintstep[,2] = as.numeric(levels(combinedintstep[,2]))[combinedintstep[,2]]
for (i in 1:length(nomissing[,1])) {
  if (is.na(nomissing[i,1])) {
    for (j in 1:length(combinedintstep[,2])) {
      if (nomissing[i,3] == combinedintstep[j,2]) {
        nomissing[i,1] = combinedintstep[j,1]
      }
    }
  }
}

datesteps <- split(nomissing, nomissing$date)
totalsteps <- rep(NA, 61)
date <- rep(NA, 61)
for (i in 1:length(datesteps)) {
  date[i] = names(datesteps)[i]
  totalsteps[i] = (sum((datesteps[[i]][1])))
}

hist(totalsteps, main = "Frequency of total steps in one day (NA Values Replaced)", xlab = "Total steps in one day")
mean(totalsteps) #Increases from 9354.23 to 13657.97
```

```
## [1] 13657.97
```

```r
median(totalsteps) #Increases from 10395 to 11458
```

```
## [1] 11458
```

```r
## Are there differences in activity patterns between weekdays and weekends?
nomissing[,2] = as.Date(nomissing[,2])
nomissing$dayofweek = weekdays(nomissing[,2])
nomissing$weekend = NA
for (i in 1:length(nomissing[,4])) {
  if (nomissing[i,4] %in% c("Saturday", "Sunday")) {
    nomissing[i,5] = "Weekend"
  }
  else {
    nomissing[i,5] = "Weekday"
  }
}

intsteps <- split(nomissing, nomissing$weekend)
totalsteps1 <- rep(NA, 288)
interval <- rep(NA, 288)
totalsteps2 <- rep(NA, 288)
intsplit1 <- split(intsteps$Weekday, intsteps$Weekday$interval)
intsplit2 <- split(intsteps$Weekend, intsteps$Weekend$interval)
for (i in 1:length(intsplit1)) {
  interval[i] = names(intsplit1)[i]
  totalsteps1[i] = (sum(intsplit1[[i]][,1]))/length(intsplit1[[i]][,1])
  totalsteps2[i] = (sum(intsplit2[[i]][,1]))/length(intsplit2[[i]][,1])
}

totalsteps1 <- data.frame(totalsteps1, "Weekday", interval)
totalsteps2 <- data.frame(totalsteps2, "Weekend", interval)
names(totalsteps1)[1] = "steps"
names(totalsteps2)[1] = "steps"
names(totalsteps1)[2] = "weekend"
names(totalsteps2)[2] = "weekend"
final <- rbind(totalsteps1, totalsteps2)

library(ggplot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)

```r
graph <- ggplot(data = final, aes(x = interval, y = steps, group = 1)) + geom_line(aes(group=1)) + facet_wrap(~weekend, nrow = 2)
graph + theme(strip.background = element_rect(colour = "black", fill = "beige")) + ylab("Number of steps") + xlab("Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)
