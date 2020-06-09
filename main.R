# this is the file where the code will be updated

#this code is used for reading the data from the csv file activiy.csv

data <- read.csv("activity.csv")

#we need to change the date format into date hence we write the code below

data$date<-as.Date(data$date)

#next we need to creaate a histogram of the total number of steps taken each day

sum_steps<-aggregate(data$steps,by=list(data$date),FUN=sum,na.rm=TRUE) 

hist(sum_steps$x, 
     breaks=seq(from=0, to=25000, by=2500),
     col="red", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")

#the next is we will calculate the mean and median steps taken each day

mean(sum_steps$x)

median(sum_steps$x)

#next we will create a time series plot of the average number of steps taken

avg_steps<-aggregate(data$steps,by=list(data$interval),FUN=mean,na.rm=TRUE)

colnames(avg_steps)<-c("interval","steps")

library(ggplot2)

ggplot(aes(x=interval,y=steps),data=avg_steps)+geom_line()

#now we calculate the five minute intervals that on average that every step contains

avg_steps[avg_steps$steps==max(avg_steps$steps),1]

#next we describe the steps for inputting the data
#first we calculate and report the number of missing values in the data
sum(is.na(data$steps))

#replace NA with the means of the steps

data$steps[is.na(data$steps)]<-mean(data$steps,na.rm=TRUE)

#this is the head of the new data set
head(data)

#now we create a histogram with the replaced NA values

sum_steps<-aggregate(data$steps,by=list(data$date),FUN=sum,na.rm=TRUE) 

hist(sum_steps$x, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Total number of steps taken each day\n(NA replaced by mean)")
# we now calcluate the mean and median values after replacing the na values
mean(sum_steps$x)

median(sum_steps$x)

#now we create Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

# Convert date into weekdays

data$days=tolower(weekdays(data$date))

#Now categorised days into weekend and weekdays

data$day_type<-ifelse(data$days=="saturday"|data$days=="sunday","weekend","weekday")

#Take mean steps taken on weekend or weekday in the intervals

avg_steps<-aggregate(data$steps,by=list(data$interval,data$day_type),FUN=mean,na.rm=TRUE)

colnames(avg_steps)<-c("interval","day_type","steps")

# Create panel plot between average steps and interval seperated by day type

ggplot(aes(x=interval,y=steps),data=avg_steps)+geom_line()+facet_wrap(~avg_steps$day_type)