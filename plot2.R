## {project root}/plot2.R plot2 module for exploratory data analysis October 2015
## writes plot 2 for project 1 to output device "idenitifed by outdevice"plot2.png" as specified in assignment
## pretty much the same story as plot1 &2; no 
library(dplyr)
plot2 <- function() {
  #1. read and filter data; see plot1.R for more extensive breakdown
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
  
  ## this  puts the weekday for the data on the x axis, so we need to plot by that
  bywkday <- mutate(appdata, wkday=weekdays(as.Date(dttime)))
  ## now assign teh variables from this for the scatter plot
  plot2y <- bywkday$Global_active_power
  plot2x <- bywkday$dttime
  
  png(file="plot2.png")
  plot.new()
  ##axis(side=1,bywkday$wkday)
  plot(plot2x,plot2y,type="l",xlab=" ",ylab="Global Active Power (kilowatts)")
  dev.off()
} ## plot2