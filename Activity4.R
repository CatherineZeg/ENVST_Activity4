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

#Prompt 2 
weather_sub <- weather %>%
  filter(between(weather$DateF, as.Date("2021-5-01 00:00:00"), as.Date("2021-6-01 00:00:00")))

ggplot(weather_sub,
       aes(x = DateF, y = SolRad)) +
  geom_line()
  
#compared to other months, the sensor seems to behave normally, so I think it hasn't experienced issues

#Prompt 3 - didn't get to this in class

#hw ----

#Question 1:
#create subset of weather for precipitation that occurs when the air temperature is above or equal to zero
# X or Y level observations are less than or equal to 2 degrees
clintonPrecipitation <- subset(weather, AirTemp >= 0 & (abs(XLevel) <= 2 & abs(YLevel) <= 2))

sum(is.na(clintonPrecipitation$Precip))

#Question 2: 
#check if Battery Voltage is below 8.5 Volts (8500 mV)
weather$VoltageFlag <- ifelse(weather$BatVolt < 8500, 
                             1, 
                             0) 

#Question 3: 
weather_no_na <- subset(weather, !is.na(weather$SolRad))
ggplot(weather,
       aes(x = DateF, y = AirTemp)) +
  geom_line()
#a function that checks for observations that are in unrealistic 
#data ranges in air temperature and solar radiation.
#checks if solar radiation is more than twice the radiation of the previous radiation
#checks if airtemp is more than 1 greater/less than previous airtemp record
unrealisticAirSolarRange <- function(x) {
  x[(!is.na(x$AirTemp) & (x$AirTemp > 30 | x$SolRad < -25)) |
    (!is.na(x$SolRad) & (x$SolRad > 950 | x$SolRad < 0)), ]
}
nrow(unrealisticAirSolarRange(weather))

#Question 4:
#subset of weather df for Jan - Mar of 2021.
winterAirTemp <- weather %>%
  filter(between(weather$DateF, as.Date("2021-1-01 00:00:00"), as.Date("2021-4-01 00:00:00")))

#ggplot of winter air temperatures in Jan - Mar of 2021.
ggplot(winterAirTemp,
       aes(x = DateF, y = AirTemp, col = AirTemp)) +
  geom_line() +
  labs(title = "Winter Air Temperature (Jan - Mar 2021)",
       y = "Air Temp (celcius)",
       x = "Date",
       color = "Temp") +
  scale_color_gradientn(colors = c("blue", "#FFFD87", "red")) +
  theme_minimal()

#Question 5:
#subset of weather df for Mar - April of 2021.
marAprAirTemp <- weather %>%
  filter(between(weather$DateF, as.Date("2021-3-01 00:00:00"), as.Date("2021-5-01 00:00:00")))

#for loop to set all AirTemps to NA if they are less than 35 degrees F that day or the day prior
for(x in 2:nrow(marAprAirTemp)) {
  #handles the edge case of the first observation of marAprAirTemp
  if (x == 2 & marAprAirTemp$AirTemp[x-1] < 1.7) {
    marAprAirTemp$Precip[x] <- NA
  }
  #sets AirTemp to NA if temperature is less than 35 degrees F (1.7 degrees C)
  if (marAprAirTemp$AirTemp[x] < 1.7 | marAprAirTemp$AirTemp[x-1] < 1.7) { 
    print(marAprAirTemp$AirTemp[x])
    marAprAirTemp$Precip[x] <- NA
  }
}

#total non NAs in the Precip row
sum(!is.na(marAprAirTemp$Precip)) 
#3858

#Question 6:
#Read in the soil temperature data using the for loop. 
#Alter your time interval function to include a user specified time interval 
#(in seconds) as an argument in the function. 
#Check for any clock/time issues associated with the soil data and 
#include the results in your output. 
#Include a brief description of any issues with the clock/data availability. 
#for (x in 1:nrow(weather)) {
#  soilData <- weather$so
#}


soilData$Date <- ymd_hm(soilData$Timestamp, tz = "America/New_York")

#time check function updated to check for all inconsistencies for user chosen second intervals
timeCheck <- function(x, time) {
  intervals <- x[-length(x)] %--% x[-1] 
  interval_times <- int_length(intervals)
  intervals[interval_times != time]
}

#checks for data not an hour apart
timeCheck(soilData$Date, 3600)
