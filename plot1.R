## {project root}/plot1.R plot1 module for exploratory data analysis October 2015
## writes plot 1 for project 1 to plot1.png as specified by assignment.  No parameters are necessary;
## e.g. plot1()
## uses Electric power consumption data as source
## call in dplyr
library(dplyr)

plot1 <- function() {
# step 1 read in data:
  unzip("exdata_data_household_power_consumption.zip")
  text_data <- "household_power_consumption.txt"
  thisds <- read.table(text_data,sep=";",as.is=TRUE,header=TRUE)
#step 2 only data specified : 2007-02-01 and 2007-02-02
#       convert strdttime to date and filter 
thisds <- mutate(thisds,strdttime = paste(Date,Time))
dttime <- strptime(thisds$strdttime,"%d/%m/%Y %H:%M:%S")
thisds <- cbind(thisds,dttime)
start <- strptime("01/02/2007 00:00:00","%d/%m/%Y %H:%M:%S")
end <- strptime("02/02/2007 23:59:59","%d/%m/%Y %H:%M:%S")
get.rows <- thisds$dttime >= start & thisds$dttime <= end
appdata <- thisds[get.rows,]
# and now appdata has teh data we need ready to graph

  png(file="plot1.png")
  hist(as.numeric(as.character(appdata$Global_active_power)),
       col="red",xlab="Global Active Power (kilowatts) ", main="Global Active Power")
   dev.off()
  } ## plot1