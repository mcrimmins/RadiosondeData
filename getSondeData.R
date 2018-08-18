# get data from https://mesonet.agron.iastate.edu/archive/raob/

#library(jsonlite)
library(RJSONIO)
library(RCurl)
library(reshape2)
library(ggplot2)

# loop through days
dateRange<-format(seq(as.Date("2017/6/1"), as.Date("2017/8/16"), by = "day"), format="%Y%m%d")

for (k in 1:length(dateRange)) {
    
  raw_data <- getURL(paste0('https://mesonet.agron.iastate.edu/json/raob.py?ts=',dateRange[k],'0000&station=KTUS'))

    data <- fromJSON(raw_data)

      # find length of list
      listLength<-length(data[["profiles"]][[1]][["profile"]])
      j=1; datalist = list()
      
      # combine into list
      for (i in 1:listLength){
        #temp[i]<-class(data[["profiles"]][[1]][["profile"]][[i]])
        if(class(data[["profiles"]][[1]][["profile"]][[i]])=="numeric"){
          tempData<-as.data.frame(data[["profiles"]][[1]][["profile"]][[i]])
          datalist[[j]] <- tempData; j=j+1
        }
      }
      # flatten into dataframe
      sondeDay = do.call(cbind, datalist)
      colnames(sondeDay)<-as.character(sondeDay[1,])
      # add some vars
      sondeDay$vars<-rownames(sondeDay)
      sondeDay$date<-dateRange[k]
      # keep only certain cols
      keyLevels <- c("850", "700", "500","400","300","250","200","vars","date")
      sondeDay <- sondeDay[keyLevels]
      # drop first column, varying name
      #sondeDay[,1:2] <- NULL
      
      # store tempGrids in stack  
      if (k==1){
        DFbind <- sondeDay
      }else{
        DFbind <- rbind(DFbind, sondeDay) # brick or stack?
      }
      print(dateRange[k])
}
      
# select var to plot
subData <- DFbind[ which(DFbind$vars=="drct"),]
#subData <- subData[,c("500","vars","date")]
meltData<-melt(subData)
meltData$date<-as.Date(meltData$date, format="%Y%m%d")

ggplot(meltData, aes(date, value, color=variable)) + 
geom_line()+
  ylim(0,100)+
  theme_bw()+
  ylab("knots")+
  ggtitle("2017 00Z KTUS Soundings - Wind speed at mandatory levels")

ggplot(meltData, aes(date, value)) + 
  geom_line()+
  facet_wrap(~variable)+
  theme_bw()+
  ylab("Degrees")+
  ggtitle("2017 00Z KTUS Soundings - Wind dir at mandatory levels")
