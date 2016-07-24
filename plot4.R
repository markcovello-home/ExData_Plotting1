## {project root}/plot4.R plot4 module for exploratory data analysis October 2015
## writes plot 4 for project 1 to output device plot4.png as speciifed in assignment
## This is similar to the first 3 plots; not parameters, just the call, 
## e.g., plot4()
##
library(dplyr)

plot4 <- function() {
  # first get the data
  unzip("exdata_data_household_power_consumption.zip")
  text_data <- "household_power_consumption.txt"
  thisds <- read.table(text_data,sep=";",as.is=TRUE,header=TRUE)
  thisds <- mutate(thisds,strdttime = paste(Date,Time))
  dttime <- strptime(thisds$strdttime,"%d/%m/%Y %H:%M:%S")
  thisds <- cbind(thisds,dttime)
  start <- strptime("01/02/2007 00:00:00","%d/%m/%Y %H:%M:%S")
  end <- strptime("02/02/2007 23:59:59","%d/%m/%Y %H:%M:%S")
  get.rows <- thisds$dttime >= start & thisds$dttime <= end
  appdata <- thisds[get.rows,]
  
  ## first get the points for each graph
  ## the funky weekday variable is teh x axis for all4
  bywkday <- mutate(appdata, wkday=weekdays(as.Date(dttime)))
  plot4x <- bywkday$dttime
  ## This is not working here worked with the others 
  plot.new()
  ##axis(side=1,bywkday$wkday)
  ## really funny; it seems to keep weekday on the horizontal without this axis statement at all
  ## what an amateur setup!
  ## now the y vars
  ## all but 2,1 are single variables
  ## I'll deal with that when we get there
  p411y <- bywkday$Global_active_power
  p412y <- bywkday$Voltage
  p422y <- bywkday$Global_reactive_power
  ## everything done will be going to the device so open it here:
  png(file="plot4.png")
  ## set up the canvas - got the margins by screwing wiuth them
  ## it was a good use of a weekend.
  par(mfrow=c(2,2),mar=c(4,4,2,2))
  par(xpd=FALSE)
  ## put 1,1 1,2 on the canvas
  plot(plot4x,p411y,type="l",xlab=" ", ylab="Global Active Power")
  plot(plot4x,p412y,type="l",xlab="datetime", ylab="Voltage")
  ## OK here's the b***ch
  plot421yk <- bywkday$Sub_metering_1
  plot421ye <- bywkday$Sub_metering_3
  plot421yd <- bywkday$Sub_metering_2
  plot(plot4x,plot421yk,type="n",xlab=" ",ylab="Energy sub metering")
  points(plot4x,plot421yk,type="l",col="black")
  points(plot4x,plot421ye,type="l",col="blue")
  points(plot4x,plot421yd,type="l",col="red")
  leg.txt <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  leg.col <- c("black", "red", "blue")
  legend("topright","(x,y)",legend=leg.txt,col=leg.col,text.col=leg.col,lty=c(1,1,1))
  ## sh!% of course the legend is hosed and there is no instruction on what to do about it
  
  ## come back to that, but lets finish the 4th for now:
  ## so I cme back an it turned out the png file gas enough resolution to handle it, so that's
  ## it.  Praise the Lord!
  plot(plot4x,p422y,type="l",xlab="datetime", ylab="Global_reactive_power")
  
  dev.off()
} ## plot4