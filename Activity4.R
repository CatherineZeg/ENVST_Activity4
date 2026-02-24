#load in the libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#load in campus weather data set
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")

#load in meta data about campus weather data set
metaData <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv")


#in class work ----

#UTC - universal time, standard for recording data
#Since people interpret time in local time 
#want to make sure time zones match between data and user
#the longer the senor hasn't been told the time, it tend to slow down as it ages and wears
#not as common with modern sensors

#updating weather date time zones
weather$DateF <- mdy_hm(weather$Date)
weather$DateET <- mdy_hm(weather$Date, tz = "America/New_York")

#checking the errors with the time conversion
weatherCheck <- weather %>%
  filter(is.na(weather$DateET))

#create interval of time
weather$DateF[2] %--% weather$DateF[3]
#finds total time of the interval
int_length(weather$DateF[2] %--% weather$DateF[3])

#test example
test <- weather$DateF[1:10]
#original list
test
#drops the first element
test[-1]

#x is a date
#function to check that the time interval between elements are the same
timeCheck900 <- function(x) {
  intervals <- x[-length(x)] %--% x[-1] 
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}

#apply time check function to the DateF column
timeCheck900(weather$DateF)

#create list of soil files in soil fold
soilFiles <- list.files("/cloud/project/activity04/soil")

#set up variable to be used in for loop
soilList <- list()

#for loop iterating through each soil file
for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}

#combine the data sets through their columns
soilData <- do.call("rbind", soilList)

#Prompt 1
#calculate the moving average for temperature in weather station data frame
airMA <- numeric()

#for loop to find the moving average for each obs after the first 7
for(i in 8:length(weather$AirTemp)) {
  airMA[i] <- mean(weather$AirTemp[(i - 7) : i])
}

#add data to weather df
weather$airMA <- airMA

#Prompt 2 - fix
weather_sub <- weather %>%
  filter(weather$DateF >= "2022-12-11 15:30:00")
ggplot(weather,
       aes(x = DateF, y = SolRad)) +
  geom_line()
  
 
#hw ----
