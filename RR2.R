avgStepsInt <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)  
plot(avgStepsInt, type="l",  
        main="Average steps: 5 minute Interval Time Series", 
        ylab="Average number of Steps", 
        xlab="Interval", col="blue") 
abline(v=avgStepsInt$interval[avgStepsInt$steps >= max(avgStepsInt$steps)], lty = 1, col = "red")
        text(x = avgStepsInt$interval[avgStepsInt$steps >= max(avgStepsInt$steps)],y = round(max(avgStepsInt$steps)),   
        labels=paste("Most steps = ",(round(max(avgStepsInt$steps)))), 
        pos = 4, col = "red")
        
totStepsInt <- aggregate(steps ~ interval, data = activity, max, na.rm = TRUE)  
plot(totStepsInt, type="l",  
     main="Total steps: 5 minute Interval Time Series", 
     ylab="Total number of Steps", 
     xlab="Interval", col="blue") 
abline(v=totStepsInt$interval[totStepsInt$steps >= max(totStepsInt$steps)], lty = 1, col = "red")
text(x = avgStepsInt$interval[totStepsInt$steps >= max(totStepsInt$steps)],y = round(max(totStepsInt$steps)),   
     labels=paste("Total steps = ",(round(max(totStepsInt$steps)))), 
     pos = 4, col = "red")