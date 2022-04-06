#####################QUESTION 1#####################
# Answered in the google doc

#load in lubridate
library(lubridate)
library(tidyverse)

#read in streamflow data
datH <- read.csv("/Volumes/GEOG331_S22/students/ishraque/data/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("/Volumes/GEOG331_S22/students/ishraque/data/streamflow/2049867.csv")                            
head(datP)

#only use most reliable measurements using the USGS data flag A
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))   

#####################QUESTION 2#####################
# Answered in the google doc

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#####################QUESTION 3#####################
dim(datH) # 454648 observations
dim(datD) # 393798 observations

dim(datP) # 16150 observations

# View(datP) View(datH) View(datD)

# Discussion in the google docs

#####################QUESTION 4#####################
# Answered in the google doc

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

# Decided not to use dev the pop-up plot is very distracting
#start new plot
# dev.new(width=14,height=8)

# #bigger margins
# par(mai=c(1,1,1,1))
# #make plot
# plot(aveF$doy,aveF$dailyAve, 
#      type="l", 
#      xlab="Year", 
#      ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
#      lwd=2)


# PLOT WITH SD POLYGON

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

# PLOT WITH TOP AND RIGHT BORDERS REMOVED
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

# ADDING LEGEND TO THE PLOT

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

# FIXING THE LEGEND SYMBOL

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#####################QUESTION 5#####################

# Adding a line for the 2017 observations

# isolating the 2017 observations
datD2017 <- datD[datD$year==2017,]
range(datD2017$discharge) # 1.22 - 160
months<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept",
           "Oct","Nov","Dec")

# making the plot
par(mai=c(1,1,1,1))

plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes

polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)

lines(datD2017$doy, datD2017$discharge, type = "l",
      lwd=1, col="red")

axis(1, seq(0,366, by=33), #tick intervals
     lab=months) #tick labels
axis(2, seq(0,180, by=20),
     seq(0,180, by=20),
     las = 2)#show ticks at 90 degree angle

legend("topright", c("mean","1 standard deviation", "2017 Streamflow"), #legend items
       lwd=c(2,NA,1),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

#####################QUESTION 6#####################

mean(datD2017$discharge)
sd(datD2017$discharge)
median(datD2017$discharge)
quantile(datD2017$discharge)

#####################QUESTION 7#####################

# Aggregate the precipitation dataset by day and year with the function length
datPagg <- aggregate(x = datP, by = list(datP$year, datP$doy), FUN=length)
# Filtering days that have 24 measurements
datP24 <- filter(x, doy == 24)

# Creating a dataframe with only the year and day columns of this filtered dataframe
# Also adding a decimal year column

datP24_new <- data.frame(year = datP24$Group.1,
                         doy = datP24$Group.2)
datP24_new$decYear <- ifelse(leap_year(datP24_new$year),datP24_new$year + 
                            (datP24_new$doy/366),
                            datP24_new$year + (datP24_new$doy/365))

# Creating the plot

par(mai=c(1,1,1,1))
plot(datD$decYear, 
     datD$discharge,
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     ylim=c(0,500))
    

points(datP24_new$decYear, 
       replicate(length(datP24_new$decYear), 0), 
       col = 'red', pch=15, cex=0.5)

legend("topright", 
       c("Discharge","Days with all prcp measures"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

# MAKING A HYDROGRAPH

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
