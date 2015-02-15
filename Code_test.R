data = read.csv("activity.csv", stringsAsFactors= FALSE)

head(data)
summary(data)
str(data)

range(data$interval)

library(dplyr)

steps_perday = tapply(data$steps, data$date, sum, na.rm=T)

range(steps_perday[])

hist(steps_perday, breaks=25, col="#2ca25f",
     xlab = "Number of Steps",
     main= "Histogram of the total number of steps taken each day")



##

head(steps_perday)

steps_perint = tapply(data$steps, data$interval, mean, na.rm=T)

plot(steps_perint ~ unique(data$interval), type="l")




data2 = data

for (i in 1:nrow(data2)){
  if (is.na(data$steps[i])){
    data2$steps[i] = steps_perday[data$date[i]]
    
  }
}
