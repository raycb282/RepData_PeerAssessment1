# setwd("~/RR1")
# unzip("repdata-data-activity.zip")
library(dplyr)
library(lattice)
activity <- read.csv("activity.csv",sep = ",", header=TRUE)
activity$date <- as.Date(activity$date)
totalsteps <- ddply(activity, "date", function(x) sum(x$steps))
hist(totalsteps$V1, breaks = 6,
        col = "lightblue",
        main = paste("Histogram of the total number of steps taken each day"),
        xlab = " Number of steps")
        abline(v=mean(totalsteps$V1, na.rm = TRUE), lty=1, col="green")
        abline(v=median(totalsteps$V1, na.rm = TRUE), lty=2, col="red")
        text(mean(totalsteps$V1, na.rm = TRUE),5,labels="mean", pos=4, col="green")
        text(median(totalsteps$V1, na.rm = TRUE),3,labels="median", pos=4, col="red")
print(paste("The mean is = ", mean(totalsteps$V1, na.rm = TRUE)))
print(paste("The median is = ", median(totalsteps$V1, na.rm = TRUE)))

