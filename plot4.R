#plot4.R
#generates the second plot from the data set
plot4 <- function() {
  #downloading the target file
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip",method = "libcurl")
  
  #unzipping the target file to the current working directory
  unzip("household_power_consumption.zip", exdir=".")
  
  #read the file
  powerConsumption <- read.csv("./household_power_consumption.txt", header = T, sep = ";", na.strings = "?", 
                               nrows = 2075259, check.names = F, stringsAsFactors = F, comment.char = "", quote = '\"')
  #reformat the dates to day:month:year
  powerConsumption$Date <- as.Date(powerConsumption$Date, format = "%d/%m/%Y")
  
  #grab the data from 1 - 2 February 2007
  febData <- subset(powerConsumption, subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))
  
  #get rid of the main data set as we don't need it
  remove(powerConsumption)
  
  #now combine Date and Time to Date/Time classes in R
  concatTime <- paste(as.Date(febData$Date),febData$Time)
  febData$timestamp <- as.POSIXct(concatTime)
  
  #now generate Plot 4
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  
  with(febData, {
    plot(Global_active_power~timestamp, type="l",
         ylab = "Global Active Power (kilowatts)", xlab = "")
    plot(Voltage~timestamp, type="l",
         ylab = "Voltage (volts)", xlab = "")
    plot(Sub_metering_1~timestamp, type="l",
         ylab = "Voltage (volts)", xlab = "")
    lines(Sub_metering_2~timestamp, col='Red')
    lines(Sub_metering_3~timestamp, col='Blue')
    #do the legends
    legend("topright", col = c("black","red","blue"), lty=1, lwd=2, bty="n", cex = 0.5,
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    plot(Global_reactive_power~timestamp, type="l",
         ylab = "Global Reactive Power (kilowatts)", xlab = "")
    
  })
  
  #saving Plot 4
  dev.copy(png, file = "figure/plot4.png", height = 400, width = 480)
  dev.off()
}
