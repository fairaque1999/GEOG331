options(warn=-1)
library(tidyverse)
# importing the weather station csv file
# change the filepath whichever way convenient. The following conditional mitigates differences
# in importing for different OS
if (.Platform$OS.type == "windows") {
  dat.w <- read.csv("Z:\\students\\ishraque\\data\\noaa_weather\\2011124.csv")
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
vec_num <- c(1, 1.2, 5, 5.6, 7.8)
class(vec_num)
# integer - special case of numeric data. So it is numeric data with no decimals
vec_int <- as.integer(c(1, 3, -6, 5, 70))
class(vec_int)
# character - data type for storing texts are strings in R
vec_char <- c("geog", "geol", "colgate", "env", "data")
class(vec_char)
# factor - factors are the data objects which are used to categorize the data and store it as levels. They can store both strings and integers. They are useful in the columns which have a limited number of unique values. 
vec_factor <- factor(c("test1", "test2", "test3", "test4", "test5"))
class(vec_factor)

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

####QUESTION 3#####
# help(hist) and help(paste)

# hist computes and plots the histogram of the given data values. Some of the the
# arguments used are elaborated below:
# - x : vector of data values
# - freq : if True plots the frequency, if False plots the relative density/freq
# - xlab : x axis label
# - col : color of the bars
# - border: border color of the bard

# paste converts ones or more R objects (the first argument) into character vectors
# paste can also be used to concatenate vectors after they are converted into characters

####QUESTION 4#####

# I decided to plot my histograms using ggplot and group them together using patchwork

library(patchwork)

# decided to plot sites 1, 2, 3, and 5

# Creating the dataframes for the histograms. Specific sites can be chosen here to 
# choose which histograms to plot
site1.df <- filter(dat.w, dat.w$siteN == 1)
site2.df <- filter(dat.w, dat.w$siteN == 2)
site3.df <- filter(dat.w, dat.w$siteN == 3)
site4.df <- filter(dat.w, dat.w$siteN == 5)


hist1 <- ggplot(site1.df, aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="grey50", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(unique(site1.df$NAME))) +
  geom_vline(xintercept=mean(site1.df$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(site1.df$TAVE, na.rm=TRUE) +
               sd(site1.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(site1.df$TAVE, na.rm=TRUE) -
               sd(site1.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()

hist2 <- ggplot(site2.df, aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="dodgerblue", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(unique(site2.df$NAME))) +
  geom_vline(xintercept=mean(site2.df$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(site2.df$TAVE, na.rm=TRUE) +
               sd(site2.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(site2.df$TAVE, na.rm=TRUE) -
               sd(site2.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()


hist3 <- ggplot(site3.df, aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="lightgreen", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(unique(site3.df$NAME))) +
  geom_vline(xintercept=mean(site3.df$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(site3.df$TAVE, na.rm=TRUE) +
               sd(site3.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(site3.df$TAVE, na.rm=TRUE) -
               sd(site3.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()

hist4 <- ggplot(site4.df, aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="orange1", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(unique(site4.df$NAME))) +
  geom_vline(xintercept=mean(site4.df$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(site4.df$TAVE, na.rm=TRUE) +
               sd(site4.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(site4.df$TAVE, na.rm=TRUE) -
               sd(site4.df$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()

# Plotting all the histograms in quadrature through patchwork
(hist1|hist2)/(hist3|hist4)

####QUESTION 5#####
# Sites 1 and 2 have normally distributed average temperatures, but sites 3, 4, and 5 all
# have a left skew and also seem to be bimodal

####QUESTION 6#####

# Extreme high temperatures start above the 95th percentile

hi_temp <- qnorm(0.95, mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE),
                 sd(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE))

# now the mean increases by 4 and the sd stays the same, which means we have a shifted distribution

# then using this new normal distribution, we can calculate the probability of temperatures greater than the current threshold (hi_temp) from occurring

hi_prob <- pnorm(hi_temp,
                 mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE) + 4,
                 sd(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE))

# There is a roughly 80% chance of crossing the high temp threshold

####QUESTION 7#####

# Creating a dataframe of exponential and gamma distribution

x<-seq(0, 200, 0.1) 
y_exp<-dexp(x, 1/5)
y_gamma <- dgamma(x, shape = 2, scale = 5/2)
ggdat<-data.frame(x,y_exp, y_gamma)

hist_prcp<- ggplot(filter(dat.w, dat.w$siteN == 1)) +
  geom_histogram( aes(x = PRCP, y = ..density..), fill="royalblue3", color="white", binwidth=5, boundary=5)+
  geom_line(data = ggdat, aes(x = x, y = y_exp),  linetype = 1, color = "red", size = 1) + 
  geom_line(data = ggdat, aes(x = x, y = y_gamma),  linetype = 2, color = "green", size = 1)+
  labs(y="Relative frequency",
       x = "Precipitation (in mm)",
       title=paste(levels(dat.w$NAME)[1])) +
  theme_bw()

hist_prcp

# Here we have plotted the daily precipitaion histogram for Aberdeen and overlaid two distirbution
# plots on top. The green graph shows the gamma distribution with n=2 and lambda = 1/5
# Whereas the red curve shows the exponential distribution with lambda = 1/5
# These two distributions seem to get the closest in modeling the histogram of the data

####QUESTION 8#####

# Aggregating across site and years using the sum function
annualPrecip <- aggregate(dat.w$PRCP, by = list(dat.w$NAME, dat.w$year), FUN = "sum", na.rm = TRUE)
colnames(annualPrecip) <- c("NAME", "year", "Annual_PRCP")

# select the site to plot 
site <- 5

# site specific dataframe to be used for plotting
site_prcp <- filter(annualPrecip, as.numeric(NAME) == site)

hist_annual_prcp<- ggplot(site_prcp) +
  geom_histogram( aes(x = Annual_PRCP, y = ..density..), fill="royalblue3", color="white", bins=15)+
  labs(y="Relative frequency",
       x = "Annual Precipitation (in mm)",
       title=paste(levels(site_prcp$NAME)[site])) +
  theme_bw()
hist_annual_prcp

# The data is normally distributed (roughly, but site 5 has some skew) across all sites

####QUESTION 9#####

# aggregating the annual precipitation dataframe by site and then taking the mean
meanPrecip <- aggregate(annualPrecip, by = list(annualPrecip$NAME), FUN = "mean", na.rm = TRUE)
# Cleaning up redundant columns
meanPrecip <- subset(meanPrecip, select = -c(NAME, year))
colnames(meanPrecip)[1] <- "NAME"

meanPrecip
averageTemp

# Comparing these values give us some insight about the relative climates of the 5 sites in the
# dataframe. Aberdeen has fairly high mean annual precipitation and moderate mean temperature. So
# could be temperate and wet. Livermore and Mormon flat has sparse precipitation and high average
# temperatures indicating that they are probably warm dry climates. Mandan Station has low 
# precipitation and low average temperature, which might be because it is cold and dry.
# Finally, Morrisville has low average temperature and high precipitation, so it is a wet cold 
# climate region.

