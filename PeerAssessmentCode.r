dataset <- read.csv("activity/activity.csv")

StepsByDay <- aggregate(steps ~ date, data=dataset, FUN=sum)

hist(StepsByDay$steps, main="Steps per day", xlab="Steps per day", ylab="Frequency")

StepsPerDayMean <- mean(StepsByDay$steps)
StepsPerDayMedian <- median(StepsByDay$steps)

StepsByInterval <- aggregate(steps ~ interval, data=dataset, FUN=mean)

plot(StepsByInterval$interval, StepsByInterval$steps, type="l")

MaxInterval <- StepsByInterval$interval[tail(order(StepsByInterval$steps),1)]

NumberMissing <- sum(is.na(dataset$steps))

dataset_filled <- dataset

num <- length(dataset_filled$steps)

for (i in 1:num) {
  if(is.na(dataset_filled$steps[i])) {
    dataset_filled$steps[i] <- 34
  }
}

StepsByDay_filled <- aggregate(steps ~ date, data=dataset_filled, FUN=sum)

hist(StepsByDay_filled$steps, 
     main="Summary of steps per day", 
     xlab="Steps per Day", ylab="Frequency")

StepsPerDayMean_filled <- mean(StepsByDay_filled$steps)
StepsPerDayMedian_filled <- median(StepsByDay_filled$steps)

weekdays <- weekdays(as.POSIXlt(dataset$date), abbreviate=TRUE)

is.weekday <- vector(length=num, mode="character")

for (j in 1:num) {
  if(weekdays[j]=="Sun" | weekdays[j]=="Sat") { is.weekday[j] <- "weekend" }
  else { is.weekday[j] <- "weekday" }
}
is.weekday <- as.factor(is.weekday)

dataset_day_type <- cbind(dataset_filled, is.weekday)
dataset_weekday <- dataset_day_type[dataset_day_type$is.weekday == "weekday", ]
dataset_weekend <- dataset_day_type[dataset_day_type$is.weekday == "weekend", ]

weekday_steps <- aggregate(steps ~ interval, data= dataset_weekday, FUN=mean)
weekend_steps <- aggregate(steps ~ interval, data= dataset_weekend, FUN=mean)

plot(weekday_steps$interval, weekday_steps$steps, type="l", xlab="Time interval", ylab="steps", main="Steps by interval - Weekday")

plot(weekend_steps$interval, weekend_steps$steps, type="l", xlab="Time interval", ylab="steps", main="Steps by interval - Weekend")
