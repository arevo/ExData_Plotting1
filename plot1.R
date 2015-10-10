plot1 <- function(){
  ## Refining the data set
  library(dplyr)
  pathtodataset <- "/Users/ashwinrevo/R/coursera/household_power_consumption.txt"
  electricpowerconsumptiondata <- read.table(pathtodataset, header=TRUE, sep=";")
  filterpowerconsumption <- filter(electricpowerconsumptiondata, powerDate=="1/2/2007")
  filterpowerconsumption <- rbind(filterpowerconsumption, filter(electricpowerconsumptiondata, powerDate=="2/2/2007"))
  filterpowerconsumption <- filterpowerconsumption[!(filterpowerconsumption$Global_active_power=="?"), ]
  
  x <- filterpowerconsumption$powerDate
  y <- filterpowerconsumption$Time
  z <- paste(x, y)
  filterpowerconsumption$Time = strptime(z, "%d/%m/%Y %H:%M:%S")
  filterpowerconsumption$powerDate = as.Date(filterpowerconsumption$powerDate, "%d/%m/%Y")
  filterpowerconsumption$Global_active_power = as.numeric(filterpowerconsumption$Global_active_power)
  d<-d[!(d$A=="B" & d$E==0),]
  ## Plot
  png("plot1.png", width = 480, height = 480)
  hist((filterpowerconsumption$Global_active_power)/500,
       main="Global Active Power",
       xlab="Global Active Power (kilowatts)",
       col="red")
  dev.off()
}