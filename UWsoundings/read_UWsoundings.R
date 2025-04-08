# scrape info from UofWyo Skew T/radiosonde data
# adapted from getUWOparams.R
# MAC 06/04/2020

library(rvest)
library(stringr)
library(tidyr)
#library(httr)
library(readr)

# functions
extractLevel <- function(x,levName) {
  level<-data.frame((unlist(x)))
  level<-level %>% separate(1,paste0(c("pressure","hgt","temp","dwpt","frpt","relh","reli",
                                       "mixr","drct","sknt","thta","thte","thtv"),levName),
                            sep =c(7,14,21,28,35,42,49,56,63,70,77,84,91))
  level[,1:13] <- (apply(level, 2, function(x) as.numeric(as.character(x))))
  #level$date<-dateRange
  return(level)
}

# vars -- "CAPE", "pwat
extractVar <- function(x,name){
  if(length(x)==0){
    x<-NA
  }
  var<-data.frame(matrix(unlist(x)))
  var<-var %>% separate(1,c(NA,name), sep = ":")
  var[,1]<-as.numeric(var[,1])
  #var$date<-dateRange
  return(var)
}

# set years to process
yrs<-seq(2007,2022,1)

# levels lists
mb850yr=list()
mb500yr=list()
mb850all=list()
mb500all=list()

# index lists
indexYr= list()
indexAll= list()

# loop through years
for (k in 1:length(yrs)) {
  
  # read in year text file
  rawText<-read_lines(paste0("./UWsoundings/KTUS/tus_",yrs[k],".out"),n_max = 200000)
  # find rows for each hour/day within year, create dataframe
  datesDF<-as.data.frame(rawText[which(unlist(gregexpr('TUS Observations', rawText))==1)])
    colnames(datesDF)<-"dateString"
  datesDF<-as.data.frame(stringr::str_split_fixed(datesDF$dateString,' ',7))
    datesDF<-datesDF[,4:7]
    colnames(datesDF)<-c("hour","day","month","year")
    datesDF$date<-as.Date(paste0(datesDF$year,"-",datesDF$month,"-",datesDF$day), "%Y-%b-%d")
  datesDF<-cbind.data.frame(datesDF,which(unlist(gregexpr('TUS Observations', rawText))==1))
  colnames(datesDF)[ncol(datesDF)]<-c("rowStart")
  datesDF$rowEnd<-c(datesDF$rowStart[2:nrow(datesDF)],length(rawText))
    datesDF$rowEnd<-c(datesDF$rowEnd[1:nrow(datesDF)-1]-1, datesDF$rowEnd[nrow(datesDF)])
  
  # loop through day/hours in year 
  for(i in 1:nrow(datesDF)){
    textBlock<-paste0(rawText[datesDF$rowStart[i]:datesDF$rowEnd[i]])
    #mb850[[k]]<-stringr::str_extract(textBlock[which(unlist(gregexpr('850.0', textBlock))!=-1)], "850.0.{0,21}")
    # 850 check if exists
    if(length(which(unlist(gregexpr('850.0', textBlock))!=-1)!=0)){
      mb850yr[[i]]<-cbind.data.frame(datesDF$date[i],datesDF$hour[i],extractLevel(textBlock[which(unlist(gregexpr('850.0', textBlock))!=-1)], "850"))
    }else{
      mb850yr[[i]]<-NA
    }
    # 500 check if exists
    if(length(which(unlist(gregexpr('500.0', textBlock))!=-1)!=0)){
      mb500yr[[i]]<-cbind.data.frame(datesDF$date[i],datesDF$hour[i],extractLevel(textBlock[which(unlist(gregexpr('500.0', textBlock))!=-1)], "500"))
    }else{
      mb500yr[[i]]<-NA
    }
    
    # get indices
    indexYr[[i]]<-cbind.data.frame(datesDF$date[i],datesDF$hour[i],
    extractVar(textBlock[which(unlist(gregexpr('Convective Available Potential Energy', textBlock))!=-1)],"CAPE"),
    extractVar(textBlock[which(unlist(gregexpr('Lifted index', textBlock))!=-1)],"LI"),
    extractVar(textBlock[which(unlist(gregexpr('SWEAT index', textBlock))!=-1)],"SWEAT"),
    extractVar(textBlock[which(unlist(gregexpr('Bulk Richardson Number', textBlock))!=-1)[1]],"RICH"),
    extractVar(textBlock[which(unlist(gregexpr('for entire sounding', textBlock))!=-1)],"PW")
    )
  }    
  
  # add each year to full list  
  mb850all[[k]]<-do.call(rbind, mb850yr)
  mb500all[[k]]<-do.call(rbind, mb500yr)
  indexAll[[k]]<-do.call(rbind, indexYr)
    
  print(yrs[k])
  
}

# create full df of years
mb850all<-do.call(rbind, mb850all)
mb500all<-do.call(rbind, mb500all)
indexAll<-do.call(rbind, indexAll)

# clean up outliers
indexAll<-subset(indexAll, PW<55)

# fix colnames
colnames(indexAll)[1:2]<-c("date","hour")
colnames(mb500all)[1:2]<-c("date","hour")
colnames(mb850all)[1:2]<-c("date","hour")

# clean up outliers
mb850all<-subset(mb850all, pressure850==850)
mb500all<-subset(mb500all, pressure500==500)

save(mb850all, mb500all, indexAll, file="./UWsoundings/KTUS_2007_2022_sounding_values.RData")

