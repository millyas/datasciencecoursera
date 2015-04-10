url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(url,destfile="./a.zip")
library(data.table)
nim<-read.table(unzip("./a.zip","household_power_consumption.txt"),sep=";",header=TRUE)
nim$Date<-as.Date(nim$Date,"%d/%m/%Y")
nim$Time<-as.character(nim$Time)
nim$Time<-strptime(nim$Time,format="%H:%M:%S")
x<-as.Date(c("2007-02-01","2007-02-02"))
nim<-nim[(nim$Date==x[1])|(nim$Date==x[2]),]
d<-as.numeric(as.character(nim[[3]]))
png(filename="plot1.png",height=480,width=480)
hist(d,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
graphics.off()