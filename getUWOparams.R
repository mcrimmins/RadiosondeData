# scrape info from UofWyo Skew T/radiosonde data
# MAC 08/07/2019

library(rvest)
library(stringr)
library(tidyr)
library(httr)

# functions
extractLevel <- function(x,levName) {
  level<-data.frame(matrix(unlist(x)))
  level<-level %>% separate(1,paste0(c("pressure","hgt","temp","dwpt"),levName), sep =c(7,14,21,28))
  level <- as.data.frame(sapply( level, as.numeric ))
  level$date<-dateRange
  return(level)
}

# vars -- "CAPE", "pwat
extractVar <- function(x,name){
  var<-data.frame(matrix(unlist(x)))
  var<-var %>% separate(1,c(NA,name), sep = ":")
  var[,1]<-as.numeric(var[,1])
  var$date<-dateRange
  return(var)
}

# station number
station<-72274

# loop through days
dateRange<-seq(as.Date("2022/06/15"), as.Date("2022/08/10"), by = "day")
capeList= list()
pwatList= list()
mb925=list()
mb850=list()
mb500=list()

httr::set_config(httr::user_agent("crimmins@arizona.edu; +https://cals.arizona.edu/climate/"))

for (k in 1:length(dateRange)) {
  
  year<-format(dateRange[k], "%Y")
  month<-format(dateRange[k], "%m")
  day<- format(dateRange[k], "%d")
  
  httr::set_config(httr::user_agent("crimmins@arizona.edu; +https://cals.arizona.edu/climate/"))
  #URL<-'http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=2019&MONTH=08&FROM=0600&TO=0600&STNM=72274'
  URL<-paste0('http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=',year,'&MONTH=',month,'&FROM=',day,'00&TO=',day,'00&STNM=',station)
  
  rawText<-read_html(URL)%>% html_nodes("pre") %>% html_text()
  #awText<-read_html(temp)%>% html_nodes("pre") %>% html_text()
  
  # extract key vars
  mb925[[k]]<-stringr::str_extract(rawText[1], "925.0.{0,21}")
  mb850[[k]]<-stringr::str_extract(rawText[1], "850.0.{0,21}")
  mb500[[k]]<-stringr::str_extract(rawText[1], "500.0.{0,21}")
  capeList[[k]]<-stringr::str_extract(rawText[2], "Convective Available Potential Energy:.{0,20}")
  pwatList[[k]]<-stringr::str_extract(rawText[2], "for entire sounding:.{0,6}")
  
  print(dateRange[k])
  
  #Sys.sleep(sample(10, 1) * 0.25)
  Sys.sleep(sample(c(5:7),1))
}

# extract lists into dataframes
mb925<-extractLevel(mb925,"925")
mb850<-extractLevel(mb850, "850")
mb500<-extractLevel(mb500, "500")
cape<-extractVar(capeList,"cape")
pwat<-extractVar(pwatList,"pwat")

# combine into single dataframe
allVars<-cbind(mb925$date,mb925[,2:4],mb850[,2:4],mb500[,2:4],cape$cape,pwat$pwat)
#allVars<-cbind(mb850$date,mb850[,2:4],mb500[,2:4],cape$cape,pwat$pwat)

#save(allVars, file=paste0("./UWsoundings/UWyo_sounding_vars_",station,"_",dateRange[1],"_",dateRange[length(dateRange)],".RData"))


# get Tucson ACIS snow observations
library(RCurl)
library(jsonlite)
dateRange<-seq(as.Date("2020/07/01"), as.Date("2020/09/30"), by = "day")
dateRange<-dateRange-1
# download data in JSON format and convert
jsonQuery=paste0('{"sid":"028820","sdate":"',dateRange[1],'","edate":"',dateRange[length(dateRange)],'","elems":"1,2,43,4,10,11"}') # sid = station id, 028820=Tucson, arizona
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

# add precip to allVars
allVars$precip<-data$precip+1

#write.csv(allVars, file = "KTUS2018.csv")

# plot them out
library(cowplot)
library(ggplot2)

p1<-ggplot(allVars, aes(mb925$date,cape$cape))+
  geom_bar(stat = 'identity', fill="firebrick")+
  xlab("")+
  ylab("J/kg")+
  ggtitle("KTUS 00Z Sounding Indices - CAPE")

p2<-ggplot(allVars, aes(mb925$date,pwat$pwat))+
  geom_bar(stat = 'identity', fill='darkgreen')+
  xlab("")+
  ylab("mm")+
geom_hline(yintercept = 25)+
  ggtitle("Precipitable Water")

p3<-ggplot(data, aes(date+1,precip*25.4))+
  geom_bar(stat = 'identity', fill='green')+
  xlab("")+
  ylab("mm")+
  ggtitle("TIA Total Precip")

p4<-ggplot(allVars, aes(mb925$date,dwpt850))+
  geom_bar(stat = 'identity', fill='orange')+
  xlab("")+
  ylab("C")+
  ggtitle("850mb Dewpoint")

p5<-ggplot(allVars, aes(mb925$date,temp500))+
  geom_bar(stat = 'identity', fill='darkblue')+
  xlab("")+
  ylab("C")+
geom_hline(yintercept = -5)+
  ggtitle("500mb Temp")


plot_grid(p1,p2,p4,p5,p3, nrow = 5)

plot_grid(p1,p2,p3, nrow = 3)

#library("PerformanceAnalytics")
#chart.Correlation(allVars[,2:13], histogram=TRUE, pch=19)

# # extract CAPE into DF
# capeDF<-data.frame(matrix(unlist(capeList)))
# capeDF<-capeDF %>% separate(1,c(NA,"CAPE"), sep = ":")
# capeDF$CAPE<-as.numeric(capeDF$CAPE)
# capeDF$date<-dateRange
# 
# # extract PWAT
# pwatDF<-data.frame(matrix(unlist(pwatList)))
# pwatDF<-pwatDF %>% separate(1,c(NA,"pwat"), sep = ":")
# pwatDF$pwat<-as.numeric(pwatDF$pwat)
# pwatDF$date<-dateRange
