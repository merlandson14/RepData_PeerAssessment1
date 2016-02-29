# Movement Steps Assignment
M Erlandson  
February 27, 2016  

#Steps Analysis

##Introduction

The following program was designed to evaluate a random individual's steps that were recorded every 5 minutes from October 1 to November 31, 2012 (a total of 17,568 observations). This is an exploratory look at those findings.

The data comes in a zip file here: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

Downloaded on 2016-02-27

Inside is a single file, activity.csv, with normal comma separations and NAs for missing values.

Used R Studio version 0.99.473 and R version 3.2.2

###Loading and Initial Prepping of Data

Data was downloaded and unzipped into the current working directory. Once there, it was read into R. Date was changed from factor variable to Date format.


```r
moveData <- read.csv("activity.csv")
moveData$date <- as.Date(as.character(moveData$date), "%Y-%m-%d")
```

###First Questions - How many steps per day?

The first thing we want to do is analyze how many steps in total the individual took per day within these two months. We load in the dplyr and ggplot2 packages first.


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
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

Then, we take the aggregate sum of each day and  display it in a histogram, with a rainbow color scheme for day differentiations.


```r
totalStepsDaily <- aggregate(steps ~ date, moveData, sum)
pal <- colorRampPalette(c("red", "green", "blue", "purple"))
g1 <- ggplot(totalStepsDaily, aes(date, steps))
g1 + geom_bar(stat = "identity", fill = pal(53)) + labs(title = "Number of Steps per Day")
```

![](https://github.com/merlandson14/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/how many steps-1.png)

We also want to know the mean and median values over the total number of steps per day. 


```r
mean(totalStepsDaily$steps)
```

```
## [1] 10766.19
```

```r
median(totalStepsDaily$steps)
```

```
## [1] 10765
```

We can also find out the mean over all the 5-min intervals per day, saved as a data frame.


```r
meanStepsIntv <- aggregate(steps ~ date, moveData, mean)
```

And we can plot a histogram of the mean of each day.


```r
g2 <- ggplot(meanStepsIntv)
g2 + geom_bar(aes(date, steps), stat = "identity", fill = pal(53)) + labs(title = "Average Number of Steps per Day")
```

![](https://github.com/merlandson14/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/mean steps plot-1.png)

###Second Questions -- How many steps per 5 minutes?

Instead of daily, we can examine the step numbers for each 5-minute interval of the days in the two months. This makes some sense, especially if the individual had normal routines for sleeping, eating, exercise, and work. Some drawbacks, though, would be if the routines changed on off-work days or if the work was sporadic. We may examine that later.

Run an aggregate accounting over the 5-min intervals.


```r
meanSteps5min <- aggregate(steps ~ interval, moveData, mean)
```

And we plot the averages as a line graph.


```r
g3 <- ggplot(meanSteps5min, aes(interval, steps))
g3 + geom_line(color = "red") + labs(title = "Average Steps per 5-Minute Interval")
```

![](https://github.com/merlandson14/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/mean 5-min plot-1.png)

Based on this plot, the 5-min intervals that had the highest averages of steps is in the 800 interval range, but we can see exactly which one it is.


```r
meanSteps5min[which.max(meanSteps5min$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

###Third Questions -- What about missing values?

Missing values were included in the original data as NAs. This could have a negative impact on analyses, so we can attempt to guess values for these positions.

First, we can find the total number of NAs in the original dataset.


```r
nrow(moveData[is.na(moveData$steps),])
```

```
## [1] 2304
```

Second, we develop a plan for filling in the missing values. We can interpolate/extrapolate from the days around it or take the mean or median value for each 5-min interval over all the days. Again, there could be differences in weekdays, so maybe use the mean/median for the same days of the week as the missing values, but we might address that later. For now, let's use the integer value of the 5-min mean and create a filled data frame.


```r
moveFilled <- moveData                    # copy full dataset first
for(i in 1:nrow(moveData)){
    if(moveData$steps[i] %in% NA){
        moveFilled$steps[i] <- as.integer(meanSteps5min$steps[meanSteps5min$interval %in% moveData$interval[i]])
    }
}
```

Now we will show the same histogram again as before on total number of steps per day.


```r
totalFilledDaily <- aggregate(steps ~ date, moveFilled, sum)
g4 <- ggplot(totalFilledDaily, aes(date, steps))
g4 + geom_bar(stat = "identity", fill = pal(61)) + labs(title = "Number of Steps per Day (Values Filled)")
```

![](https://github.com/merlandson14/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/how many steps filled-1.png)

There are two days, Oct 2 and Nov 15, with very few values. They were not NAs, just small numbers of observed steps. We can speculate what might have happened on those days, but we do not know. For a more detailed analysis, we might want to exclude those days as outliers, but for this test, we will leave them in.

Next, we calculated the mean and median values the same as before.


```r
mean(totalFilledDaily$steps)
```

```
## [1] 10749.77
```

```r
median(totalFilledDaily$steps)
```

```
## [1] 10641
```

You'll notice that these values do differ from the previous set, but not by much on the mean. Since we used averages for filling, the overall average stays close to the same as it was. I was surprised that it brought the median down lower than it had been. The values that were missing must have been at lower step times.

###Fourth Questions -- What are the differences in weekdays and weekends?

As stated previously, there could be differences in this individual's daily routines from, say, a normal work week Monday through Friday versus what that person may have done as leisure activities on the weekends, Saturday and Sunday. 

We will use the filled-in data frame and add a factor variable, "weekday" and "weekend".


```r
moveFilled <- mutate(moveFilled, day = weekdays(moveFilled$date))
for(j in 1:nrow(moveFilled)){
    if(moveFilled$day[j] %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")){
        moveFilled$day[j] <- "weekday"
    } else {
        moveFilled$day[j] <- "weekend"
    }
}
```

Now we need to calculate the mean over each 5-minute interval, factored into either weekday or weekend.


```r
meanFilled5min <- aggregate(steps ~ day + interval, moveFilled, mean)
```

And we create a panel plot of line graphs over the two factors.


```r
g5 <- ggplot(meanFilled5min, aes(interval, steps))
g5 + geom_line(color = "red") + facet_grid(. ~ day) + labs(title = "Average Steps per 5-Minute Interval for Weekdays and Weekends")
```

![](https://github.com/merlandson14/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/mean 5-min/day plot-1.png)

It is obvious in both panels that this person is sleeping at the intervals 0 through about 500, so we will take those to be from midnight to waking.

We can see that there are some differences between weekdays and weekends, most especially in the mornings after waking (intervals at about 500 to 750), where on weekdays this person might be going to work and on weekends is sleeping in or relaxing at home. 

The huge jump in values for intervals 800 to 950 on weekdays may be an exercise routine that is either reduced or altered on the weekends.

There is also an increase in steps in the afternoons (intervals 1000 to 2000) on weekends, where this person might be traveling outside of the house for errands or fun activities, rather than staying still in a work environment.

In conclusion, we can see some interesting trends in steps if taken in 5-minute intervals and then divided into weekdays and weekends.
