print(paste("Count of enties with missing values is",length(activity$steps[is.na(activity$steps)])))

fillNAsteps <- function(interval) {
        avgStepsInt[avgStepsInt$interval == interval, ]$steps
}

activityAll <- activity  
count = 0  
for (i in 1:nrow(activityAll)) {
        if (is.na(activityAll[i, ]$steps)) {
                activityAll[i, ]$steps <- fillNAsteps(activityAll[i, ]$interval)
                count = count + 1
        }
}
print(paste("Total of", count, "NA values update."))

totalsteps2 <- ddply(activityAll, "date", function(x) sum(x$steps))
hist(totalsteps2$V1, breaks = 6,
     col = "lightblue",
     main = paste("Histogram of the total number of steps taken each day"),
     xlab = " Number of steps")
abline(v=mean(totalsteps2$V1), lty=1, col="green")
abline(v=median(totalsteps2$V1), lty=2, col="red")
text(mean(totalsteps2$V1),5,labels="mean", pos=4, col="green")
text(median(totalsteps2$V1),3,labels="median", pos=4, col="red")
print(paste("The mean is = ", mean(totalsteps2$V1)))
print(paste("The median is = ", median(totalsteps2$V1)))