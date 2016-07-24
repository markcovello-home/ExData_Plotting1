## {project root}/plot3.R plot3 module for exploratory data analysis October 2015
## writes plot 3 for project 1 to "plot3.png"
## this is the biggest pia of the bunch so far, but since it's included in plot4,
## I suppose that will soon claim the crown.  
## The data read was done in teh first two plots and should be second nature by now:
## as with first 2 no parameters input and output specified by assignment
library(dplyr)
plot3 <- function() {
  ## The data read was done in teh first two plots and should be second nature by now:
  unzip("exdata_data_household_power_consumption.zip")
  text_data <- "household_power_consumption.txt"
  thisds <- read.table(text_data,sep=";",as.is=TRUE,header=TRUE)
  #step 2 only data specified : 2007-02-01 and 2007-02-02
  #       convert strdttime to date and filter - need dplyr
  
  thisds <- mutate(thisds,strdttime = paste(Date,Time))
  dttime <- strptime(thisds$strdttime,"%d/%m/%Y %H:%M:%S")
  thisds <- cbind(thisds,dttime)
  start <- strptime("01/02/2007 00:00:00","%d/%m/%Y %H:%M:%S")
  end <- strptime("02/02/2007 23:59:59","%d/%m/%Y %H:%M:%S")
  get.rows <- thisds$dttime >= start & thisds$dttime <= end
  appdata <- thisds[get.rows,]
  ## do the same nonsense as plot 2 to keep weekday on the xaxis
  bywkday <- mutate(appdata, wkday=weekdays(as.Date(dttime)))
  ## now assign teh variables from this for the scatter plot
  plot3yblack <- bywkday$Sub_metering_1
  plot3yblue <- bywkday$Sub_metering_3
  plot3yred <- bywkday$Sub_metering_2
  plot3x <- bywkday$dttime
  
  png(file="plot3.png")
  plot.new()
  ##axis(side=1,bywkday$wkday)
  ## now for teh fun part, this gives what we want, but it's kind of cheating
  ## it would be better to let the data do this, but wth knows how to do that
  ## instead, manually set up the y axis 
  
  plot(plot3x,plot3yblack,type="n",xlab=" ",ylab="Energy sub metering")
  points(plot3x,plot3yblack,type="l",col="black")
  points(plot3x,plot3yblue,type="l",col="blue")
  points(plot3x,plot3yred,type="l",col="red")
  ## Now just add the legend
  leg.txt <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  leg.col <- c("black", "red", "blue")
  legend("topright","(x,y)",legend=leg.txt,col=leg.col,text.col=leg.col,lty=c(1,1,1))
  dev.off()
} ## plot3