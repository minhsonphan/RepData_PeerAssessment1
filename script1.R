## Loading and preprocessing the data
dfActivity <- read.csv("activity.csv", header = TRUE, sep= ",")


## What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day
dfActivityPerDay <- aggregate(steps ~ date, dfActivity, sum)

## Make a histogram of the total number of steps taken each day
hist(dfActivityPerDay$steps, main="Histogram of total number of steps per day", xlab="Total number of steps per day", ylab="Frequency", col="Red")        

## Calculate and report the mean and median of the total number of steps taken per day
summary(dfActivityPerDay$steps)


## What is the average daily activity pattern?
## Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
dfAverageActivityPerInterval <- aggregate(steps ~ interval, dfActivity, mean)
with(dfAverageActivityPerInterval,plot(x=interval, y=steps, type="l", xlab="5-minute interval", ylab="Average number of steps across all days"))

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
dfAverageActivityPerInterval$interval[which.max(dfAverageActivityPerInterval$steps)]


##Imputing missing values

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA)
table(is.na(dfActivity$steps))

## Devise a strategy for filling in all of the missing values in the dataset.
## We use the mean for the corresponding 5-minute interval
## Create a new dataset that is equal to the original dataset but with the missing data filled in.
dfActivityWithFilledSteps <- dfActivity 
dfActivityStepsNotFilled <- dfActivity[is.na(dfActivity$steps),]

for (i in rownames(dfActivityStepsNotFilled)){
        ## dfAverageActivityPerInterval[dfAverageActivityPerInterval$interval == dfActivityStepsNotFilled[i,]$interval,]$steps
        dfActivityWithFilledSteps[i,]$steps <- dfAverageActivityPerInterval[dfAverageActivityPerInterval$interval == dfActivityStepsNotFilled[i,]$interval,]$steps
}



## Make a histogram of the total number of steps taken each day
## Calculate and report the mean and median total number of steps taken per day. 
dfActivityPerDayWithFilledSteps <- aggregate(steps ~ date, dfActivityWithFilledSteps, sum)
hist(dfActivityPerDayWithFilledSteps$steps, main="Histogram of total number of steps per day (with missing value filled for steps)", xlab="Total number of steps per day", ylab="Frequency", col="Red")        



## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?
par(mfrow=c(1,2))
Hist1 <- hist(dfActivityPerDay$steps, main="Histogram of total number of steps per day", xlab="Total number of steps per day", ylab="Frequency", col="Red", ylim=c(0, 35))        
Hist2 <- hist(dfActivityPerDayWithFilledSteps$steps, main="Histogram of total number of steps per day (with missing value filled for steps)", xlab="Total number of steps per day", ylab="Frequency", col="Blue")        
Max1 <- Hist1$counts[which.max(Hist1$counts)]
Max2 <- Hist2$counts[which.max(Hist2$counts)]
Impact <- (Max2 - Max1)/Max1*100
paste(round(Impact, 2), "%", sep="")


## Are there differences in activity patterns between weekdays and weekends?
## Use the dataset with the filled-in missing values for this part.
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating 
dfActivityWithFilledSteps$weekday <- as.factor(ifelse(weekdays(as.Date(dfActivityWithFilledSteps$date)) %in% c("samedi", "dimanche"), "weekend", "weekday"))

## whether a given date is a weekday or weekend day
## Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
dfAverageActivityPerTypeOfDay <- aggregate(steps ~ weekday+interval, dfActivityWithFilledSteps, mean)
dfAverageActivityPerWeekday <- dfAverageActivityPerTypeOfDay[dfAverageActivityPerTypeOfDay$weekday == "weekday",]
dfAverageActivityPerWeekend <- dfAverageActivityPerTypeOfDay[dfAverageActivityPerTypeOfDay$weekday == "weekend",]
par(mfrow=c(1,2))
with(dfAverageActivityPerWeekday,plot(x=interval, y=steps, type="l", xlab="5-minute interval", ylab="Average number of steps across weekdays", ylim=c(0, 230)))
with(dfAverageActivityPerWeekend,plot(x=interval, y=steps, type="l", xlab="5-minute interval", ylab="Average number of steps across weekends", ylim=c(0, 230)))

