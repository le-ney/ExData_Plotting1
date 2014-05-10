#This function accepts a filename as argument. The file name can contain
#the full path in Unix format (including Mac OS X and Linux) 
#(./path../filename). The output of this function is plot_1.png file containing
#histogram of Global Active Power. Please note the generated PNG file 
#can be found in your R current work directory
plot.1 <- function(fileName){
        dataSet <- read.table(fileName, header=TRUE, sep= ";", na.strings="?") #load data file to a table 
        dataSet <- dataSet[,c(1,3)] #select only the required fields
        dataSet [,1] <- as.Date(dataSet [,1],"%d/%m/%Y") #convert string format to date type 
        dataSet <- dataSet[(dataSet$Date == as.Date("2007-02-01")) | (dataSet$Date == as.Date("2007-02-02")),] #filter for certain dates
        dataSet [,2] <- as.numeric(dataSet [,2]) #convert global active power field to numeric
        png(file="plot_1.png", height=480, width=480) #set and open PNG file
        with(dataSet, hist(Global_active_power,col="red",main = "Global Active Power",
                           xlab = "Global Active Power (kilowatts)")) #plot histogram
        dev.off() #close PNG file
}