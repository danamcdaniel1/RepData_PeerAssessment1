loaddata <- function(){
    read.csv("C:\\Users\\Dianshi\\Desktop\\Laptop Stuff\\Reproducible research\\Peer Assessment 1\\activity.csv",
             stringsAsFactors = F, header = T)
}

as.datetime <- function(date = "", time = ""){
    newtime <- sprintf("%02d:%02d", floor(time / 100), time %% 100)
    datetimestr <- sprintf("%s %s", date, newtime)
    datetime <- strptime(datetimestr, format = "%Y-%m-%d %H:%M")
    datetime
}

## load and process data
x <- loaddata() # raw data
y <- x
y$"datetime" <- as.datetime(x[,"date"], x[,"interval"])
y$"interval" <- as.POSIXct(sprintf("%02d:%02d", floor(y$"interval" / 100), y$"interval" %% 100), format = "%H:%M")
z            <- aggregate(y[,"steps"], by = (list(y[,"date"])), function(x) sum(x))  # total number of steps per day
names(z)     <- c("date", "steps taken")
z[,"date"]   <- as.POSIXct(z[,"date"])
a            <- summary(z[,2])
dailymean    <- a[4]
dailymedian  <- a[3]
b            <- aggregate(y[["steps"]], 
                          by = list(y$"interval"),
                          function(x) mean(x, na.rm = T)) 
names(b)     <- c("interval", "steps")

## What is mean total number of steps taken per day?
plot(z[,"date"], z[,"steps taken"], type = "l", xlab = "Date", ylab = "Steps taken", main = "Steps per day") 
hist(z[,2], main = "Steps taken each day",
     xlab = "Number of steps",
     ylab = "Number of days Oct. to Dec.",
     breaks = 9,
     ylim = c(0,20),
     col = "steelblue") 
rug(z[,2], col = 'turquoise4', lwd = 2)
abline(v = dailymean, col = "purple", lty = 2, lwd = 2.5)
abline(v = dailymedian, col = "violet", lty = 2, lwd = 2.5)
dailymean  # mean number of steps each day
dailymedian # median number of steps each day

## What is the average daily activity pattern?
plot(b$interval, b$steps, type = 'l', xlab = "time", ylab = "Steps", main = "Daily activity pattern")
b[b$steps == max(b[,2]),] ## time interval with the most steps

## Input Missing Values
c <- y
names(c) <- names(y)
numNA <- summary(c$steps)[7]
c[which(is.na(c$steps)), "steps"]<- b[b$interval == c[is.na(c$steps),"interval"], "steps"]
summary(c$steps)[6]
d <- aggregate(c[,"steps"], by = (list(c[,"date"])), function(x) sum(x))
names(d) <- c("interval", "steps")
hist(d[,2], main = "Steps taken each day", xlab = "Number of steps", 
     ylab = "Frequency", breaks = 10, col = 'steelblue')
newMean <- summary(d[,"steps"])[4]
newMedian <- summary(d[,"steps"])[3]
e <- aggregate(c$"steps", by = list(c$"interval"), function(x) mean(x, na.rm = T)) 
names(e) <- c("interval", "steps")
plot(e$interval, e$steps, type = 'l', xlab = "time", ylab = "steps", main = "Daily activity pattern")
lines(b$interval, b$steps, col = "red")
lines(e$interval, e$steps, col = "blue")

## weekend vs. weekday comparisons.

f <- c
f$day <- weekdays(as.Date(f$date))
fweekend <- f[f$day == "Saturday" | f$day == "Sunday",]
fweekday <- f[!(f$day == "Saturday" | f$day == "Sunday"),]
fweekendactivity <- aggregate(fweekend$"steps", by = list(fweekend$"interval"), function(x) mean(x, na.rm = T)) 
fweekdayactivity <- aggregate(fweekday$"steps", by = list(fweekday$"interval"), function(x) mean(x, na.rm = T)) 
names(fweekdayactivity) <- c("interval", "steps")
names(fweekendactivity) <- c("interval", "steps")
diffweekendsteps <- fweekendactivity$steps - fweekdayactivity$steps
lines(fweekendactivity$interval, diffweekendsteps, col = "orange")
colfunc <- colorRampPalette(c("blue", "red"))
dailydiff <- c(fweekendactivity$interval, diffweekendsteps)

setwd("C:\\Users\\Dianshi\\Desktop\\Laptop Stuff\\Reproducible research\\Peer Assessment 1")
png("ComparisonPlot.png", height = 480, width = 480)
par(mfrow = c(3,1))
plot(fweekdayactivity$interval, fweekdayactivity$steps, 
     type = 'n',
     xlab = 'Time',
     ylab = 'Steps',
     main = "Weekday activity",
     col  = 'black')
polygon(fweekdayactivity$interval, fweekdayactivity$steps, col = 'steelblue1')
plot(fweekendactivity$interval, fweekendactivity$steps,
     type = 'n',
     xlab = 'Time',
     ylab = 'Steps',
     main = "Weekend activity",
     col  = "purple")
polygon(fweekendactivity$interval, fweekendactivity$steps, col = 'orchid2')
plot(fweekdayactivity$interval, diffweekendsteps,
     type = 'n',
     xlab = 'Time', 
     ylab = 'Weekend - Weekday steps',
     main = 'Comparison of weekday vs. weekend activity',
     col = colfunc(length(diffweekendsteps)))
polygon(fweekdayactivity$interval, diffweekendsteps, col = 'blueviolet')
abline(0,0, col = 'black', lty = 2)
dev.off()

