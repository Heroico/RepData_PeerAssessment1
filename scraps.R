data <- read.csv("data/activity.csv")

total <- aggregate(data$steps, by=list(Date=data$date), FUN=sum)
colnames(total)[2] <- "Total"

mean_steps <- aggregate(data$steps, by=list(Date=data$date), FUN=mean)
colnames(mean_steps)[2] <- "Mean"

daily_mean_steps <- aggregate(data$steps, by=list(Interval=data$interval), FUN=mean)
colnames(daily_mean_steps)[2] <- "Daily_Mean"

median_steps <- aggregate(data$steps, by=list(Date=data$date), FUN=median)
colnames(median_steps)[2] <- "Median"

daily_data <- merge(mean_steps, median_steps)
daily_data <- merge(daily_data, total)
daily_data

# a day's data
data[which(data$date=="2012-10-21"),]
length(data[which((data$date=="2012-10-21") & (data$steps == 0)), ]$steps)
