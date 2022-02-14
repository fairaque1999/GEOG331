# Practicing for Activity 2 in GEOG 331
# FI 01/31/22

# vector of tree heights in meters

heights <- c(30, 41, 20, 22)
# converting the heights to cm
heights_cm <- heights*100
heights_cm

# look at the first tree height
heights[1]

# looks at the 2nd and 3rd heights
heights[2:3]


####MATRIX####
# create a matrix with two columns that are filled in by rows
mat <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow=TRUE)

# create a matrix where the values fill in by columns
mat.col <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow=FALSE)

# Looking at a specific value
mat.col[1,2]

# looking at all the values in a row or column
mat.col[1,] # first row
mat.col[,2] # second column

####DATAFRAMES####

library(tidyverse)
# importing the weather station csv file

if (.Platform$OS.type == "windows") {
  dat.w <- read.csv("Z:\\students\\ishraque\\data\\noaa_weather\\2011124.csv")#, stringsAsFactors = TRUE)
}else if (.Platform$OS.type == "unix") {
  dat.w <- read.csv("/Volumes/class/GEOG331_S22/students/ishraque/data/noaa_weather/2011124.csv", stringsAsFactors = TRUE)
}

# getting more information about the dataframe
str(dat.w)

####QUESTION 1#####
dim(dat.w)
# There are 157849 rowa and 9 columns

# specify a date format
dat.w$dateF <- as.Date(dat.w$DATE, "%Y-%m-%d")
# create a year column
dat.w$year <- as.numeric(format(dat.w$dateF, "%Y"))

####QUESTION 2#####
# numeric - the values are numbers or contain decimals
# integer - special case of numeric data. So it is numeric data with no decimals
# character - data type for storing texts are strings in R
# factor - factors are the data objects which are used to categorize the data and store it as levels. They can store both strings and integers. They are useful in the columns which have a limited number of unique values. 

# Find all the unique site names
site_names <- unique(dat.w$NAME)

# Finding the mean max temperature at Aberdeen as we remove the NA values
mean(dat.w$TMAX[dat.w$NAME == "ABERDEEN, WA US"], na.rm = TRUE)

# Calculating the average daily temoperature
dat.w$TAVE <- dat.w$TMIN + ((dat.w$TMAX - dat.w$TMIN)/2)

# Getting aggregate average temperature across all the sites (average of the above averages)
averageTemp <- aggregate(dat.w$TAVE, by = list(dat.w$NAME), FUN = "mean", na.rm = TRUE)
# Changing the column names of the averageTemp dataframe
colnames(averageTemp) <- c("NAME", "MAAT") # MAAT - mean annual air temp

# Converting the sitenames into the underlying factor integer values
dat.w$siteN <- as.numeric(dat.w$NAME)

# Plotting a histogram with base R
hist(dat.w$TAVE[dat.w$siteN == 1],
     freq=FALSE, 
     main = paste(levels(dat.w$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# Plotting the histogram with ggplot
hist <- ggplot(dat.w[dat.w$siteN == 1,], aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="grey50", color="black", binwidth=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(levels(dat.w$NAME)[1])) +
  theme_bw()

####QUESTION 2#####
# help(hist) and help(paste)
