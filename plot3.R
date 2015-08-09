getData <- function() {
	setClass('myDate')
	setAs("character", "myDate", function(from) as.Date(from, format="%d/%m/%Y") )
	allDat <- read.table("household_power_consumption.txt", header=TRUE, sep=";",
		colClasses=c('myDate', 'character', 'numeric', 'numeric', 'numeric',
			'numeric', 'numeric', 'numeric', 'numeric'), na.strings = "?")
	myDat <- allDat[(allDat$Date >= "2007-02-01") & (allDat$Date <= "2007-02-02"),]
	# we may want to order them by date if we are not already
	myDat <- myDat[with(myDat, order(myDat$Date, myDat$Time)), ]
	return(myDat);
}

plot3 <- function() {
	x <- getData()
	# use png device, a dev.copy makes legend labels to be truncated
	png(file = "plot3.png", width = 480, height = 480)
	# reset mfrow so that plots have the default
	par(mfrow=c(1,1))
	plot(x$Sub_metering_1~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"),
	ann=FALSE, type="n")
	lines(x$Sub_metering_1~as.POSIXct(paste(x$Date, x$Time)), col="black")
	lines(x$Sub_metering_2~as.POSIXct(paste(x$Date, x$Time)), col="red")
	lines(x$Sub_metering_3~as.POSIXct(paste(x$Date, x$Time)), col="blue")
	legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		lty=c(1, 1, 1), col=c("black", "red", "blue"))
	title(ylab='Energy sub metering')
	dev.off()
}

plot3()