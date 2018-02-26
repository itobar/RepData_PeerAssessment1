**1. Loading and preprocessing the data**

Code needed to load the data and process/transform the data into a
format suitable for analysis.

    destfile = "./data/Factivity.zip"
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(destfile)){download.file(fileURL, destfile, method="auto")}
    activityfile <- "./data/activity.csv"
    if(!file.exists(activityfile)){unzip(zipfile="./data/Factivity.zip",exdir="./data")}
    data <- read.csv(activityfile)
    rm(activityfile)
    data$date <- as.Date(data$date)

Mean total number of steps per day
----------------------------------

What is the mean total number of steps taken per day? For this part of
the assignment, ignore the missing values in the dataset.

**2. Make a histogram of the total number of steps taken each day**

    #Histogram in ggplot
    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.3.3

    total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
    qplot(total.steps, binwidth = 1000, xlab = "Total Number of Steps per Day", ylab = "Count")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

**3. Calculate and report the mean and median of the total number of
steps taken per day**

    #Calculate mean and median
    meansteps <- as.integer(mean(total.steps, na.rm = TRUE))
    mediansteps <- as.integer(median(total.steps, na.rm = TRUE))
    print(c("Mean Steps:", meansteps, "Median Steps:", mediansteps))

    ## [1] "Mean Steps:"   "9354"          "Median Steps:" "10395"

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot of the 5-minute interval (x-axis) and the
average number of steps taken, averaged across all days (y-axis).

    ##Group data by 5 minute interval and summarize the average number of steps in that interval
    averages <- aggregate(steps ~ interval, data, mean)

**4. Time series plot of the 5-minute interval and the average number of
steps taken, averaged across all days**

    #Average activity plot
    p2 <- ggplot(data = averages, aes(x = interval, y = steps)) +
      geom_line(color = "royalblue", size=1) +
      ggtitle("Average Steps per 5-minute Interval") +
      xlab("5-minute Interval") +
      ylab("Average Number of Steps") +
      theme(plot.title = element_text(hjust = 0.5))
    print(p2)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

**5. Which 5-minute interval across all days contains the maximum number
of steps?**

    maxInt <- averages[which.max(averages$steps),]
    print(maxInt)

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

**Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)**

    missingVals <- is.na(data$steps)
    table(missingVals)

    ## missingVals
    ## FALSE  TRUE 
    ## 15264  2304

Devise a strategy for filling in all of the missing values in the
dataset. Create a new dataset that is equal to the original dataset but
with the missing data filled in.

**6. The strategy is to replace missing values with the mean value of
their respective 5-minute interval.**

    fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps)) 
            filled <- c(steps) else filled <- (averages[averages$interval == interval, "steps"])
        return(filled)
    }
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

**7. Make a histogram of the total number of steps taken each day after
missing values are imputed**

    total.steps <- tapply(filled.data$steps, filled.data$date, FUN = sum)
    qplot(total.steps, binwidth = 1000, xlab = "Total Number of Steps per Day", ylab = "Count")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

**Calculate and report the mean and median of the total number of steps
taken per day of the new data set with imputed missing data**

    #Calculate mean and median
    new_mean <- as.integer(mean(total.steps))
    new_median <- as.integer(median(total.steps))
    print(c("Mean Steps:", new_mean, "Median steps:", new_median))

    ## [1] "Mean Steps:"   "10766"         "Median steps:" "10766"

The mean calculated after imputing missing data is now closer to the
median value. The NAs in the original data are set to zero by default.
When these values are replaced with the mean number of steps for their
respective interval, the large proportion of zero values observed in the
first histogram is removed.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Create a new factor variable in the dataset with two levels - "weekday"
and "weekend" indicating whether a given date is a weekday or weekend
day.

    weekday.or.weekend <- function(date) {
        Day <- weekdays(date)
        if (Day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
            return("Weekday") else if (Day %in% c("Saturday", "Sunday")) 
            return("Weekend") else stop("invalid date")
    }
    filled.data$date <- as.Date(filled.data$date)
    filled.data$Day <- sapply(filled.data$date, FUN = weekday.or.weekend)

**8. Panel plot comparing the average number of steps taken per 5-minute
interval across weekdays and weekends**

    averages <- aggregate(steps ~ interval + Day, data = filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line(stat = "identity", aes(colour = Day)) + facet_grid(Day ~ .) + 
        xlab("5-minute Interval") + ylab("Number of Steps") + ggtitle("Number of Steps per Interval by Day Type")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
