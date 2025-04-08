# getSondeTimeSeries
# download time series of radion sonde data from mesonet.agron.iastate.edu
# MAC 01/03/2019
# https://mesonet.agron.iastate.edu/archive/raob/

library(data.table)
library(tidyr)

# set End Date
endDate<-"08/03/2020"

# get KTUS
sondeData<-fread(paste0('https://mesonet.agron.iastate.edu/cgi-bin/request/raob.py?station=KTUS&sts=01/01/1957+00:00&ets=',endDate,'+12:00'))
# get KTWC
sondeData2<-fread(paste0('https://mesonet.agron.iastate.edu/cgi-bin/request/raob.py?station=KTWC&sts=01/01/1957+00:00&ets=',endDate,'+12:00'))

# combine date frames
sondeData<-rbind(sondeData,sondeData2)

# clean up data frame types
#date<-as.Date(sondeData$validUTC,  "%Y-%m-%d %H:%M:%S")
sondeData$dateTime<-as.POSIXct(sondeData$validUTC,  format="%Y-%m-%d %H:%M:%S")

sondeData <- transform(sondeData, 
                       tmpc = as.numeric(tmpc),
                       dwpc = as.numeric(dwpc),
                       drct = as.numeric(drct),
                       speed_kts = as.numeric(speed_kts),
                       bearing = as.numeric(bearing),
                       range_sm = as.numeric(range_sm))

# get date/time values
sondeData$hour<- as.integer(strftime(sondeData$dateTime, format="%H"))
sondeData$month<- as.integer(strftime(sondeData$dateTime, format="%m"))
sondeData$year<- as.integer(strftime(sondeData$dateTime, format="%Y"))

# add dewpoint depression
sondeData$dpd<-sondeData$tmpc-sondeData$dwpc

# month of interest
mo=7

# subset data by time and month
subsetData<- sondeData[ which(sondeData$hour==12 & sondeData$month == mo),]
# only mandatory levels
subsetData<- subsetData[ which(subsetData$levelcode=="4" | subsetData$levelcode=="M"),]
# get levels
subsetData<- subset(subsetData, subset = pressure_mb %in% c(1000,925,850,700,500))

# spread the height columns
cols <- c("dateTime", "pressure_mb", "height_m")
    subsetData<-subsetData[, ..cols]
# deal with duplicates
subsetData<-unique(subsetData, by = c("dateTime","pressure_mb"))
# convert to numeric
subsetData$height_m<-as.numeric(subsetData$height_m)
# spread    
subsetData<-spread(subsetData, pressure_mb, height_m)

# calculate thickness values
subsetData$year<- as.integer(strftime(subsetData$dateTime, format="%Y"))
subsetData$thick1000_500<-subsetData$`500`-subsetData$`1000`
subsetData$thick850_700<-subsetData$`700`-subsetData$`850`
# clean up bad data <0
# subsetData[subsetData < -50] <- NA

# plot hist by year
library(ggplot2)
ggplot(subsetData, aes(x=as.factor(year), y=thick1000_500)) + 
  geom_boxplot()+
  geom_hline(yintercept=5400, linetype="dashed", 
             color = "red", size=1)
# all points
ggplot(subsetData, aes(x=(year), y=thick1000_500)) + 
  geom_point()+
  geom_hline(yintercept=5400, linetype="dashed", 
             color = "red", size=1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# get Tucson ACIS snow observations
library(RCurl)
library(jsonlite)
# download data in JSON format and convert
jsonQuery='{"sid":"028820","sdate":"por","edate":"por","elems":"1,2,43,4,10,11"}' # sid = station id, 028820=Tucson, arizona
out<-postForm("http://data.rcc-acis.org/StnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# get data from list
data<-data.frame(out$data)
colnames(data)<-c("date","t_max","t_min","t_mean","precip","snow","snowD")
data$date<-as.Date(as.character(data$date))
data$snow<-as.character(data$snow)
data$snowD<-as.character(data$snowD)
data$precip<-as.character(data$precip)
# fix M and T's
data$precip[data$precip=="T"]  <- "0.01" 
data$snow[data$snow=="T"]  <- "0.01" 
data$snowD[data$snowD=="T"]  <- "0.01" 
data$precip[data$precip=="M"]  <- "0.00" 
data$snow[data$snow=="M"]  <- "0.00" 
data$snowD[data$snowD=="M"]  <- "0.00" 

# convert columns to numeric
unfactorize<-c("t_max","t_min","t_mean","precip","snow","snowD")
data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))
# find days with snow in month of interest
snowDays<- data[ which(format(data$date, format="%m") =="02" & data$snow>0),]
  #snowDays$date<-snowDays$date-1
# join thickness to obs
subsetData$date<-as.Date(subsetData$dateTime)
snowDays<-merge(snowDays,subsetData,by="date")

# plot snow days
ggplot(snowDays, aes(x=(year), y=thick850_700, size=snow)) + 
  geom_point()+
  # geom_hline(yintercept=5400, linetype="dashed", 
  #            color = "red", size=1)+
  geom_hline(yintercept=1540, linetype="dashed", 
             color = "red", size=1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #ylim(5250,5750)+
  ylim(1450,1620)+
  ggtitle("February KTUS 12Z 850-700mb Thicknesses and Daily Total Snowfall (1958-2019)")+
  xlab("Feb-Year")+
  ylab("12Z 850-700mb Thickness (m)")+
    labs(size = "Total Snow (in)")




# precip days
precipDays<- data[ which(format(data$date, format="%m") =="02" & data$precip>0),]
  subsetData$date<-as.Date(subsetData$dateTime)
  precipDays<-merge(precipDays,subsetData,by="date")
# recode precip to type
  precipDays$ptype <- ifelse(precipDays$snow > 0, 
                          c("snow"), c("rain"))


  
  
  # plot precip days
  ggplot(precipDays, aes(x=(year), y=thick850_700, color=ptype)) + 
    geom_point()+
    geom_hline(yintercept=1540, linetype="dashed", 
               color = "red", size=1)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ylim(1475,1620)+
    ggtitle("February KTUS 12Z 850-700mb Thicknesses and Daily Total Snowfall (1958-2019)")+
    xlab("Feb-Year")+
    ylab("12Z 850-700mb Thickness (m)")
  
  library(dplyr)
  precipDaysYear <- precipDays %>%
    group_by(year) %>%
    summarise(precipEvents = n())
  
  ggplot(precipDaysYear,aes(x=year, y=precipEvents))+
    geom_bar(stat = 'identity')+
    ggtitle("February Precip Events (1958-2019)")+
    xlab("Feb-Year")+
    ylab("Number of Precip Events")
  