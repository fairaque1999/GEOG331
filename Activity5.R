#####################QUESTION 1#####################
# Answered in the google doc

#load in lubridate
library(lubridate)
library(tidyverse)
library(patchwork)
library(dplyr)

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
datP24 <- filter(datPagg, doy == 24)

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

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl


par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#####################QUESTION 8#####################

# Choosing another day in the winter to make a hydrograph

# Looking at our filtered precipitation dataframe from question 7, we select days
# 13 and 14 of the year 2012 (have all 24 hour prcp measurements) which are winter days

#subsest discharge and precipitation within range of interest
hydroD2 <- datD[datD$doy >= 13 & datD$doy < 15 & datD$year == 2012,]
hydroP2 <- datP[datP$doy >= 13 & datP$doy < 15 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#ceiling rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl2,hydroP2$pscale[i],hydroP2$pscale[i],yl2),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()
#####################QUESTION 9#####################

# Filtering and labeling the data for Seasons

datD2016 <- datD[datD$year == 2016,]
datD2016season <- datD2016 %>% mutate(Season = case_when(doy >= 1 & doy < 92 ~ "Winter",
                                                         doy >= 92 & doy < 183 ~ "Spring",
                                                         doy >= 183 & doy < 275 ~ "Summer",
                                                         doy >= 275 & doy < 367 ~ "Fall"))

datD2017 <- datD[datD$year == 2017,]
datD2017season <- datD2017 %>% mutate(Season = case_when(doy >= 1 & doy < 91 ~ "Winter",
                                                         doy >= 91 & doy < 182 ~ "Spring",
                                                         doy >= 182 & doy < 274 ~ "Summer",
                                                         doy >= 274 & doy < 366 ~ "Fall"))

# Plotting the Violin plots

violin2016 <- ggplot(data= datD2016season, aes(Season,discharge)) + 
              geom_violin()+
              labs(y=expression(paste("Discharge (ft"^"3 ","sec"^"-1",")")),
                   x = "Season",
                   title="Discharge in 2016")+
              theme_bw()

violin2017 <- ggplot(data= datD2017season, aes(Season,discharge)) + 
  geom_violin()+
  labs(y=expression(paste("Discharge (ft"^"3 ","sec"^"-1",")")),
       x = "Season",
       title="Discharge in 2017")+
  theme_bw()

violin2016|violin2017
              

