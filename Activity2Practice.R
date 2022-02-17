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
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE) - sd(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE) + sd(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
####QUESTION 3#####
# help(hist) and help(paste)
# paste converts ones or more R objects (the first argument) into character vectors
# level

####QUESTION 4#####
# Plotting the histogram with ggplot
library(patchwork)

hist1 <- ggplot(filter(dat.w, dat.w$siteN == 1), aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="grey50", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(levels(dat.w$NAME)[1])) +
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 1)$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 1)$TAVE, na.rm=TRUE) +
             sd(filter(dat.w, dat.w$siteN == 1)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 1)$TAVE, na.rm=TRUE) -
               sd(filter(dat.w, dat.w$siteN == 1)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()

hist2 <- ggplot(filter(dat.w, dat.w$siteN == 2), aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="dodgerblue", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(levels(dat.w$NAME)[2])) +
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 2)$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 2)$TAVE, na.rm=TRUE) +
               sd(filter(dat.w, dat.w$siteN == 2)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 2)$TAVE, na.rm=TRUE) -
               sd(filter(dat.w, dat.w$siteN == 2)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()

hist3 <- ggplot(filter(dat.w, dat.w$siteN == 5), aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="orange1", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(levels(dat.w$NAME)[5])) +
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 5)$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 5)$TAVE, na.rm=TRUE) +
               sd(filter(dat.w, dat.w$siteN == 5)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 5)$TAVE, na.rm=TRUE) -
               sd(filter(dat.w, dat.w$siteN == 5)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()


hist4 <- ggplot(filter(dat.w, dat.w$siteN == 3), aes(TAVE)) +
  geom_histogram( aes(y = ..density..), fill="lightgreen", color="white", binwidth=2, boundary=2)+
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(levels(dat.w$NAME)[3])) +
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 3)$TAVE, na.rm=TRUE), color="tomato3", size = 1)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 3)$TAVE, na.rm=TRUE) +
               sd(filter(dat.w, dat.w$siteN == 3)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  geom_vline(xintercept=mean(filter(dat.w, dat.w$siteN == 3)$TAVE, na.rm=TRUE) -
               sd(filter(dat.w, dat.w$siteN == 3)$TAVE, na.rm=TRUE), color="tomato3", size = 1, linetype = 3)+
  theme_bw()

(hist1|hist2)/(hist3|hist4)

## Drawing the probability distribution using ggplot instead of base R
## but the fitted normal distribution does not have the same maximum value as 
## the histogram

# hist_df <- filter(dat.w, dat.w$siteN == 1)
# fitted_hist <-  ggplot(hist_df, aes(TAVE)) +
#   geom_histogram( aes(y = ..density..), fill="grey50", color="white", binwidth=2, boundary=2)+
#   stat_function(fun = dnorm, args = list(mean = mean(hist_df$TAVE, na.rm = T),
#                                          sd =  sd(hist_df$TAVE, na.rm = T)))+
#   labs(y="Relative frequency",
#        x = "Average daily temperature (degrees C)",
#        title=paste(levels(dat.w$NAME)[1]))
# 
# fitted_hist
# Choose site number first

site <- 1

hist_df <- filter(dat.w, dat.w$siteN == site)
x.plot <- seq(min(hist_df$TAVE, na.rm = TRUE),max(hist_df$TAVE, na.rm = TRUE), length.out = 100)
y.plot <-  dnorm(x.plot,
                 mean(hist_df$TAVE, na.rm=TRUE),
                 sd(hist_df$TAVE, na.rm=TRUE))
hist.max <- max(hist(hist_df$TAVE, plot=FALSE)$density)
y.scaled <- (hist.max/max(y.plot))*y.plot

dist_df <- data.frame(x.plot, y.scaled)

fitted_hist <- ggplot(NULL) +
  geom_histogram(data = hist_df, aes(x = TAVE, y = ..density..), fill="grey50", color="white", binwidth=2, boundary=2) +
  geom_line(data = dist_df, aes(x = x.plot, y = y.scaled),  linetype = 2, color = "royalblue3", size = 1) +
  labs(y="Relative frequency",
       x = "Average daily temperature (degrees C)",
       title=paste(levels(dat.w$NAME)[site]))

fitted_hist

####QUESTION 5#####
# Sites 1 and 2 have normally distributed average temperatures, but sites 3, 4, and 5 seem to have either a skewed or a bi modal distribution

####QUESTION 6#####

# Extreme high temperatures start above the 95th percentile

hi_temp <- qnorm(0.95, mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE),
      sd(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE))

# now the mean increases by 4 and the sd stays the same, which means we have a shifted distribution

# then using this new normal distribution, we can calculate the probability of temperatures greater than teh current threshold from occuring

hi_prob <- pnorm(hi_temp,
                 mean(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE) + 4,
                 sd(dat.w$TAVE[dat.w$siteN == 1],na.rm=TRUE))

# There is a roughly 80% chance of crossing the high temp threshold

####QUESTION 7#####
x<-seq(0, 200, 0.1) 
y<-dexp(x, 1/5)
y_gamma <- dgamma(x, shape = 2, scale = 5/2)
ggdat<-data.frame(x,y)

hist_prcp<- ggplot(filter(dat.w, dat.w$siteN == 1)) +
  geom_histogram( aes(x = PRCP, y = ..density..), fill="royalblue3", color="white", binwidth=5, boundary=5)+
  geom_line(data = ggdat, aes(x = x, y = y),  linetype = 2, color = "red", size = 1) + 
  geom_line(data = ggdat, aes(x = x, y = y_gamma),  linetype = 3, color = "green", size = 1)+
  labs(y="Relative frequency",
       x = "Precipitation (in mm)",
       title=paste(levels(dat.w$NAME)[1])) +
  theme_bw()

hist_prcp

# exponential distribution seems to fit well and so does the gamma distribution, not much the beta distribution

####QUESTION 8#####

annualPrecip <- aggregate(dat.w$PRCP, by = list(dat.w$NAME, dat.w$year), FUN = "sum", na.rm = TRUE)
colnames(annualPrecip) <- c("NAME", "year", "Annual_PRCP")

# select the site to plot 
site <- 3

# sie specific dataframe
site_prcp <- filter(annualPrecip, as.numeric(NAME) == site)

hist_annual_prcp<- ggplot(site_prcp) +
  geom_histogram( aes(x = Annual_PRCP, y = ..density..), fill="royalblue3", color="white", bins=15)+
  labs(y="Relative frequency",
       x = "Annual Precipitation (in mm)",
       title=paste(levels(site_prcp$NAME)[site])) +
  theme_bw()
hist_annual_prcp

# The data is normally distributed

####QUESTION 9#####

meanPrecip <- aggregate(annualPrecip, by = list(annualPrecip$NAME), FUN = "mean", na.rm = TRUE)
meanPrecip <- subset(meanPrecip, select = -c(NAME, year))
colnames(meanPrecip)[1] <- "NAME"
