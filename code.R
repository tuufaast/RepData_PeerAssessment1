
aggregates = aggregate(steps ~ date, data, sum, na.action = na.omit)

hist(aggregates$steps)
mean(aggregates$steps)
median(aggregates$steps)


aggregates_5 = aggregate(steps ~ interval, data, mean, na.action = na.omit)
plot(aggregates_5, type="l")

aggregates_5$interval[which.max(aggregates_5$steps)]

index_nas=which(is.na(data$steps))
length(index_nas)

match_to_interval = match(data$interval[index_nas], aggregates_5$interval)

data2=data
data2$steps[index_nas]=aggregates_5$steps[match_to_interval]


aggregates2 = aggregate(steps ~ date, data2, sum, na.action = na.omit)



hist(aggregates2$steps)
mean(aggregates2$steps)
median(aggregates2$steps)

Sys.setlocale("LC_TIME", "C")

data2$weekday<-factor(weekdays(data2$date), levels = 
                           c("Monday", "Tuesday", "Wednesday", "Thursday", 
                             "Friday", "Saturday", "Sunday"))

levels(data2$weekday) = c(rep("weekday", 5), rep("weekend",2))

aggregates2_5 <- aggregate(steps ~ interval+weekday, data2, mean)
require(lattice)

xyplot(steps~interval|weekday,aggregates2_5, layout=c(1,2), type="l", ylab="Number of steps" )
