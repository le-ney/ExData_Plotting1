#This function accepts a filename as argument. The file name can contain
#the full path in Unix format (including Mac OS X and Linux) 
#(./path../filename). The output of this function is plot_4.png file containing
#four charts. Please note the generated PNG file 
#can be found in your R current work directory
plot.4 <- function(fileName){
        dataSet <- read.table(fileName, header=TRUE, sep= ";", na.strings="?") #load data file to a table
        dataSet$Date <- strptime(paste(dataSet[,1], dataSet[,2]),"%d/%m/%Y %H:%M:%S") #convert string format to date type 
        dataSet <- dataSet[(as.Date(dataSet$Date) >= as.Date("2007-02-01")) & 
                                   (as.Date(dataSet$Date) <= as.Date("2007-02-02")),] #filter for certain dates
        dataSet [,2:8] <- lapply(dataSet [,2:8],as.numeric)
        png(file="plot_4.png", height=480, width=480) #set and open PNG file
        par(mfrow=c(2,2))
        with(dataSet, { #plot first graph
                plot(Date,Global_active_power, xlab="", ylab="Global Active Power",pch=NA) 
                lines(Date, Global_active_power)
                #plot second
                plot(Date,Voltage, xlab="datetime", ylab="Voltage",pch=NA) 
                lines(Date, Voltage)
                #plot third
                plot(Date,Sub_metering_1, xlab="", ylab="Energy sub metering",pch=NA) 
                lines(Date, Sub_metering_1)
                lines(Date, Sub_metering_2, col='red')
                lines(Date, Sub_metering_3, col='blue')
                lg <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
                cl <-  c('black', 'red', 'blue')
                legend("topright", lg, col = cl, lty = c(1,1,1), bty="n")
                #plot fourth
                plot(Date,Global_reactive_power, xlab="datetime",pch=NA) 
                lines(Date, Global_reactive_power)
        })
        dev.off() #close PNG file
}