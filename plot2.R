#plot2.R
#generates the second plot from the data set
plot2 <- function() {
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
  
  plot(febData$timestamp, febData$Global_active_power, type = "l",
       ylab = "Global Active Power (kilowatts)", xlab = "")
  
  #saving Plot 2
  dev.copy(png, file = "figure/plot2.png", height = 400, width = 480)
  dev.off()
}



