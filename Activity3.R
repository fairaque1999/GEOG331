library(lubridate)
library(tidyverse)
library(patchwork)

# In this script, I followed the activity and also responded to the required questions with code
# and comments

#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

####QUESTION 1 - RESPONSE IN THE GOOGLE DOC#####
####QUESTION 2 - RESPONSE IN THE GOOGLE DOC#####

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("/Volumes/class/GEOG331_S22/students/ishraque/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("/Volumes/class/GEOG331_S22/students/ishraque/data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

####QUESTION 3 - RESPONSE IN THE GOOGLE DOC#####

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

library(lubridate)

#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("/Volumes/class/GEOG331_S22/students/ishraque/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("/Volumes/class/GEOG331_S22/students/ishraque/data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularly confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]


####QUESTION 4 - RESPONSE IN THE GOOGLE DOC#####

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

####QUESTION 5 - CODE BELOW, DISCUSSION IN THE GOOGLE DOC#####

# Creating a test using assert to verify that lightscale has the same length as 
# a column in datW
assert(length(datW$precipitation) == length(lightscale), "error: unequal length")
# let's try another column: the decimal days
assert(length(datW$DD) == length(lightscale), "error: unequal length")

# No error came out, so the vector lightscale has the same length as a column of
# datW

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

####QUESTION 6 - CODE BELOW, DISCUSSION IN THE GOOGLE DOC#####
datW$wind.speedQ1 <- ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA,
                           ifelse(datW$precipitation>5, NA, datW$wind.speed))

# Now, since we are removing the same high precipitation and lightning events from
# the wind speed measurements as we were in the air temperature measurements, then
# the locations of the NA values in both of these columns should be identical and
# can be a way to verify proper filter using assert

assert(identical(which(is.na(datW$wind.speedQ1)), which(is.na(datW$air.tempQ2))), "error: unequal length")
# no error comes out, so the filtering was proper

# plot for the windspeed (let us use ggplot instead)

wind_speed_plot <- ggplot(data = datW, aes(x = DD, y=wind.speedQ1)) +
  geom_line()+
  geom_point()+
  labs(x ="Day of the Year",
       y = "Wind Speed (m/s)") +
  theme_bw()

wind_speed_plot

####QUESTION 7 - CODE BELOW#####

# Having the 4 plots for soil temperature, soil moisture, precipitation, and air temperature
# will be really helpful here.

soil_temp_plot <- ggplot(data = datW, aes(x = DD, y=soil.temp)) +
  geom_line()+
  geom_point(size = 0.7)+
  labs(x ="Day of the Year",
       y = "Soil Temperature (degrees C)") +
  theme_bw()

air_temp_plot <- ggplot(data = datW, aes(x = DD, y=air.tempQ2)) +
  geom_line()+
  geom_point(size = 0.7)+
  labs(x ="Day of the Year",
       y = "Air Temperature (degrees C)") +
  theme_bw()

# Comparing these two plots, we see that up until the data collection stopped,
# the data in the soil temperature followed the data in the air temperature,
# which is what we would expect if the soil sensor was unplugged the same time it
# was taken out of the soil

soil_mois_plot <- ggplot(data = datW, aes(x = DD, y=soil.moisture)) +
  geom_line()+
  geom_point(size = 0.7)+
  labs(x ="Day of the Year",
       y = expression(paste("Soil Moisture"," (m"^"3 ","per m"^"3", ")"))) +
  theme_bw()

prcp_plot <- ggplot(data = datW, aes(x = DD, y=precipitation)) +
  geom_point(size = 0.7)+
  labs(x ="Day of the Year",
       y = expression(paste("Precipitation (mm)"))) +
  theme_bw()

# Comparing these two plots, we see that up until the data collection stopped,
# the data in the soil moisture followed the trend of soil mositure after a rain
# (which is a steep rise followed by a gradual decrease). Comparing the trends in
# the soil moisture with the precipitation we can see that the soil sensor
# was disconnected 50 or so days after a rainstorm, which means the gradual decrease
# of soil moisture up until the data stops are accurate.

####QUESTION 8 - CODE BELOW#####

# Let us find the average of the required measurements 


avg_air_temp <- round(mean(datW$air.tempQ2, na.rm = TRUE), digits = 1)
length(datW$air.tempQ2[!is.na(datW$air.tempQ2)])
# the length is 2105 ignoring NA values
avg_wind_speed <- round(mean(datW$wind.speedQ1, na.rm = TRUE), digits = 2)
length(datW$wind.speedQ1[!is.na(datW$wind.speedQ1)])
# the length is 2105 ignoring NA values
avg_soil_temp <- round(mean(datW$soil.temp, na.rm = TRUE), digits = 1)
length(datW$soil.temp[!is.na(datW$soil.temp)])
# the length is 1411 ignoring NA values
avg_soil_moisture <- round(mean(datW$soil.moisture, na.rm = TRUE), digits = 4)
length(datW$soil.moisture[!is.na(datW$soil.moisture)])
# the length is 1411 ignoring NA values
total_prcp <- round(sum(datW$precipitation, na.rm = TRUE), digits = 2)
length(datW$precipitation[!is.na(datW$precipitation)])

# the air temperature and wind speed measurements hvae non clustered NA measurements
# so their measurement range can be reliably said to be from 2018.6.12 to 2018.7.26

# from the soil data graph we know that starting from some date all the soil data 
# were NAs. There are 707 NAs and in total 2118 observations so the 1412th row 
# in the data sheet should be the first NA. So their measurement range can be 
# reliably said to be from 2018.6.12 to 2018.7.11

# Making the required dataframe
datW_table <- data.frame(avg_air_temp, avg_wind_speed, avg_soil_temp, avg_soil_moisture,
                           total_prcp)


####QUESTION 9 - CODE BELOW#####

# The required plots have already been set up with ggplot above, now we can use patchwork
# to neatly organize them

patchwork <- (air_temp_plot/soil_temp_plot) | (prcp_plot/soil_mois_plot)
patchwork




