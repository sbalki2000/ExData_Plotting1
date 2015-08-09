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

plot1 <- function() {
	# use png device, a dev.copy makes legend labels to be truncated
	png(file = "plot1.png", width = 480, height = 480)
	# reset mfrow so that plots have the default
	par(mfrow=c(1,1))
	x <- getData()
	hist(x$Global_active_power, col="red", ann=FALSE)
	title(main = "Global Active Power", xlab = "Global Active Power (kilowatts)",
	ylab = "Frequency")
	dev.off()
}

plot1()
