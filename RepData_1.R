

## Install and load packages if not present
if (!require("timeDate")) install.packages("timeDate")
if (!require("ggplot2")) install.packages("ggplot2")


# Loading and preprocessing data

if(!file.exists('activity.csv'))
    unzip('activity.zip')
activity <- read.csv("activity.csv")

# Transform date from factor into date variable
activity$date <- as.POSIXct(activity$date)

## What is mean total number of steps taken per day?


# We ignore NA values
daily_steps <- aggregate(activity$steps, by = list(activity$date), sum, na.rm=TRUE) 
names(daily_steps) <- c("Date", "steps")


# Histogram of daily steps

ggplot(daily_steps,aes(x = steps)) +
    ggtitle("Histogram of daily steps") +
    xlab("Steps") +
    ylab("Frequency") +
	geom_histogram()
	
	
#mean and median of total number of steps taken per day

mean(daily_steps$steps, na.rm="TRUE")
median(daily_steps$steps, na.rm="TRUE")


# Calculate the average daily activity pattern
avg_steps <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm=TRUE)
names(avg_steps) = c("interval", "mean_steps")


# Find maximum value
max_avg_steps <- avg_steps[which.max(avg_steps$mean_steps),]


# Plot the average daily activity pattern and the maximum value

ggplot(avg_steps, aes(x = interval, y = mean_steps)) + geom_line() + geom_point(x = max_avg_steps$interval, y=max_avg_steps$mean_steps, color='red',size=5)

$ The interval with the maximum number of steps
max_avg_steps$interval



##Imputing missing values

# Merge so that there is a new column that contains the average of the specific interval of each row
imputed_steps <- merge( x = activity, y = avg_steps, by = "interval", all.x = TRUE)

selected <- which(is.na(imputed_steps$steps))

imputed_steps[selected,"steps"] <- imputed_steps[selected,"mean_steps"]
imputed_steps <- imputed_steps[,1:3]

daily_steps2 <- aggregate(imputed_steps$steps, by = list(imputed_steps$date), sum, na.rm=TRUE)
names(daily_steps2) <- c("Date", "steps")


# Histogram of daily steps without NA
ggplot(daily_steps2,aes(x = steps)) +
    ggtitle("Histogram of daily steps without NA") +
    xlab("Steps") +
	ylab("Frequency") +
    geom_histogram()

	
## Are there differences in activity patterns between weekdays and weekends?

imputed_steps$weekday <- ifelse(isWeekday(imputed_steps$date, wday=1:5),"weekday","weekend")
# add new factor with 2 levels, weekday and weekend

imputed_steps$weekday <- as.factor(imputed_steps$weekday)

mean_weekday <- aggregate(imputed_steps$steps, by = list(imputed_steps$weekday, imputed_steps$interval), mean, na.rm = TRUE)
median_weekday <- aggregate(imputed_steps$steps, by = list(imputed_steps$weekday, imputed_steps$interval), median, na.rm = TRUE)



names(mean_weekday) = c("weekday", "interval","mean_steps")
ggplot(mean_weekday, aes(x = interval, y = mean_steps)) + ylab("Number of Steps") + geom_line() + facet_grid(weekday~.)



