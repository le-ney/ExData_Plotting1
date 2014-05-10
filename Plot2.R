#This function accepts a filename as argument. The file name can contain
#the full path in Unix format (including Mac OS X and Linux) 
#(./path../filename). The output of this function is plot_2.png file containing
#line chart for Global Active Power and Date & time. Please note the generated PNG file 
#can be found in your R current work directory
plot.2 <- function(fileName){
        dataSet <- read.table(fileName, header=TRUE, sep= ";", na.strings="?") #load data file to a table 
        dataSet$Date <- strptime(paste(dataSet[,1], dataSet[,2]),"%d/%m/%Y %H:%M:%S") #convert string format to date type 
        dataSet <- dataSet[,c(1,3)] #select only the required fields
        dataSet <- dataSet[(as.Date(dataSet$Date) >= as.Date("2007-02-01")) & 
                                   (as.Date(dataSet$Date) <= as.Date("2007-02-02")),] #filter for certain dates
        dataSet [,2] <- as.numeric(dataSet [,2]) #convert global active power field to numeric
        png(file="plot_2.png", height=480, width=480) #set and open PNG file
        with(dataSet, {
                plot(Date,Global_active_power, xlab="", ylab="Global Active Power (kilowatts)",pch=NA) 
                lines(Date, Global_active_power) # plot line chart
        })       
        dev.off() #close PNG file
}