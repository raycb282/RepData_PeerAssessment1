avgStepsInt <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)  
plot(avgStepsInt, type="l",  
        main="5 minute Interval Time Series", 
        ylab="Average number of Steps", 
        xlab="Interval", col="blue") 
abline(v=avgStepsInt$interval[avgStepsInt$steps >= max(avgStepsInt$steps)], lty = 1, col = "red")
        text(x = avgStepsInt$interval[avgStepsInt$steps >= max(avgStepsInt$steps)],y = round(max(avgStepsInt$steps)),   
        labels=paste("Most steps = ",(round(max(avgStepsInt$steps)))), 
          pos = 4, col = "red")