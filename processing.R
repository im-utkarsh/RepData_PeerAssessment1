library(dplyr)
data <- read.csv('activity/activity.csv')
data$date <- as.Date(data$date,'%Y-%m-%d')

## Plot 1
steps_per_day <- group_by(data,date) %>%
    summarise(sum(steps, na.rm = TRUE)) %>%
    rename(steps = `sum(steps, na.rm = TRUE)`)
hist(steps_per_day$steps, xlab = 'Steps', breaks = 10, main = 'Steps per Day')
# mean and median
mean(steps_per_day$steps)
median(steps_per_day$steps)

## Plot 2
steps_per_timeInterval <- group_by(data,interval) %>%
    summarise(mean(steps,na.rm = TRUE)) %>%
    rename(steps = `mean(steps, na.rm = TRUE)`)
plot(steps_per_timeInterval$interval, steps_per_timeInterval$steps,type = 'l',
     xlab = 'Time', ylab = 'Steps', main = 'Steps taken per Time Interval')
# interval with max no of average steps
steps_per_timeInterval[which.max(steps_per_timeInterval$steps),][[1]]

## 3
missing_values <- sum(is.na(data))
idx <- which(is.na(data))

data_new <- data
for (i in idx) {
    data_new[i,1] <- filter(steps_per_timeInterval, interval == data_new[i,3])$steps
}

steps_per_day_new <- group_by(data_new,date) %>%
    summarise(sum(steps, na.rm = TRUE)) %>%
    rename(steps = `sum(steps, na.rm = TRUE)`)
hist(steps_per_day_new$steps, xlab = 'Steps', breaks = 10,
     main = 'Steps per Day after imputing missing values')
# mean and median
mean(steps_per_day_new$steps)
median(steps_per_day_new$steps)

# now both the mean and median are same with value higher than previous one.


## 4

library(lattice)
data_new <- mutate(data_new, day = as.factor
             (ifelse(weekdays(date) == 'Sunday', "Weekend","Weekday")))
steps_per_timeInterval_by_day <- group_by(data_new,interval,day) %>%
    summarise(mean(steps)) %>%
    rename(steps = `mean(steps)`)
xyplot(steps~interval | day, steps_per_timeInterval_by_day,
       type = 'l', layout = c(1,2))

