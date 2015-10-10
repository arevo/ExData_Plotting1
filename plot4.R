plot4 <- function(){
  ## Refining the data set
  library(dplyr)
  pathtodataset <- "/Users/ashwinrevo/R/coursera/household_power_consumption.txt"
  electricpowerconsumptiondata <- read.table(pathtodataset, header=TRUE, sep=";")
  filterpowerconsumption <- filter(electricpowerconsumptiondata, powerDate=="1/2/2007")
  filterpowerconsumption <- rbind(filterpowerconsumption, filter(electricpowerconsumptiondata, powerDate=="2/2/2007"))
  filterpowerconsumption <- filterpowerconsumption[!(filterpowerconsumption$Global_active_power=="?"), ]
  filterpowerconsumption <- filterpowerconsumption[!(filterpowerconsumption$Voltage=="?"), ]
  x <- filterpowerconsumption$powerDate
  y <- filterpowerconsumption$Time
  z <- paste(x, y)
  filterpowerconsumption$Time = strptime(z, "%d/%m/%Y %H:%M:%S")
  filterpowerconsumption$powerDate = as.Date(filterpowerconsumption$powerDate, "%d/%m/%Y")
  filterpowerconsumption$Global_active_power = as.numeric(filterpowerconsumption$Global_active_power)
  d<-d[!(d$A=="B" & d$E==0),]
  ## Plot
  png("plot4.png", width = 480, height = 480)
  par(mfrow=c(2,2))
  plot(filterpowerconsumption$Time, filterpowerconsumption$Global_active_power/500, type = "l", xlab="", ylab="Global Active Power")
  plot(filterpowerconsumption$Time, filterpowerconsumption$Voltage, type = "l", xlab="datetime", ylab="Voltage")
  plot(filterpowerconsumption$Time, filterpowerconsumption$Sub_metering_1, col="black", type = "l", xlab="", ylab="Energy sub metering", ylim=c(0,30))
  lines(filterpowerconsumption$Time, filterpowerconsumption$Sub_metering_2, col="red", type = "l", xlab="", ylab="Energy sub metering")
  lines(filterpowerconsumption$Time, filterpowerconsumption$Sub_metering_3, col="blue", type = "l", xlab="", ylab="Energy sub metering")
  legend("topright", 
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col = c("black","red","blue"),
         lty=c(1,1,1)
  )
  plot(filterpowerconsumption$Time, filterpowerconsumption$Global_reactive_power, type = "l", xlab="datetime", ylab="Global_reactive_power")
  dev.off()
}