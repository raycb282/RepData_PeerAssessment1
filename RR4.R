activityAll$day <- weekdays(activityAll$date)

for (i in 1:nrow(activityAll)) {                                       
        if (activityAll[i,]$day %in% c("Saturday","Sunday")) {            
                activityAll[i,]$day<-"weekend"                                
        }
        else{
                activityAll[i,]$day<-"weekday"                                
        }
}

totalsteps3 = aggregate(steps ~ interval + day, activityAll, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = totalsteps3, aspect = 1/2, type = "l")

