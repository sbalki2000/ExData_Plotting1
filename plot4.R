getData <- function(x) {
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

plot2 <- function(x) {
	plot(x$Global_active_power~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"),
	type="n", ann=FALSE)
	title(ylab='Global Active Power (Kilowatts)')
	lines(x$Global_active_power~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"))
}

plot3 <- function(x) {
	
	plot(x$Sub_metering_1~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"),
	ann=FALSE, type="n")
	lines(x$Sub_metering_1~as.POSIXct(paste(x$Date, x$Time)), col="black")
	lines(x$Sub_metering_2~as.POSIXct(paste(x$Date, x$Time)), col="red")
	lines(x$Sub_metering_3~as.POSIXct(paste(x$Date, x$Time)), col="blue")
	legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		lty=c(1, 1, 1), col=c("black", "red", "blue"), bty="n")
	title(ylab='Energy sub metering')
}

plot5 <- function(x) {
	plot(x$Voltage~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"),
	type="n", ann=FALSE)
	title(ylab='Voltage', xlab='datetime')
	lines(x$Voltage~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"))
}

plot6 <- function(x) {
	plot(x$Global_reactive_power~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"),
	type="n", ann=FALSE)
	title(ylab='Global_reactive_power', xlab='datetime')
	lines(x$Global_reactive_power~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"))
}

plot4 <- function() {
	x <- getData();
	png("plot4.png", width = 480, height = 480)
	par(mfrow = c(2, 2))
	
	plot2(x)
	plot5(x)
	plot3(x)
	plot6(x)
	
# use dev.copy() to copy to a png file
	dev.off()
}

plot4()
