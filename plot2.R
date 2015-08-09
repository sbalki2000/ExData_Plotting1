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

plot2 <- function() {
	x <- getData()
	# use png device, a dev.copy makes legend labels to be truncated
	png(file = "plot2.png", width = 480, height = 480)
	# reset mfrow so that plots have the default
	par(mfrow=c(1,1))
	plot(x$Global_active_power~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"),
	type="n", ann=FALSE)
	title(ylab='Global Active Power (Kilowatts)')
	lines(x$Global_active_power~as.POSIXct(paste(x$Date, x$Time), format="%Y-%m-%d %H:%M:%S"))
	dev.off()
}

plot2()