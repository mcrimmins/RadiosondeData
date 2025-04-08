# scrape info from UofWyo Skew T/radiosonde data
# adapted from getUWOparams.R
# MAC 06/04/2020

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
dateRange<-seq(as.Date("1973/01/01"), as.Date("2019/12/31"), by = "day")
capeList= list()
pwatList= list()
#mb925=list()
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
  #mb925[[k]]<-stringr::str_extract(rawText[1], "925.0.{0,21}")
  mb850[[k]]<-stringr::str_extract(rawText[1], "850.0.{0,21}")
  mb500[[k]]<-stringr::str_extract(rawText[1], "500.0.{0,21}")
  capeList[[k]]<-stringr::str_extract(rawText[2], "Convective Available Potential Energy:.{0,20}")
  pwatList[[k]]<-stringr::str_extract(rawText[2], "for entire sounding:.{0,6}")
  
  print(dateRange[k])
  
    #Sys.sleep(sample(10, 1) * 0.25)
    Sys.sleep(sample(5,1))
}

# extract lists into dataframes
#mb925<-extractLevel(mb925,"925")
mb850<-extractLevel(mb850, "850")
mb500<-extractLevel(mb500, "500")
cape<-extractVar(capeList,"cape")
pwat<-extractVar(pwatList,"pwat")

# combine into single dataframe
#allVars<-cbind(mb925$date,mb925[,2:4],mb850[,2:4],mb500[,2:4],cape$cape,pwat$pwat)
allVars<-cbind(mb850$date,mb850[,2:4],mb500[,2:4],cape$cape,pwat$pwat)

save(allVars, file=paste0("./UWsoundings/UWyo_sounding_vars_",station,"_",dateRange[1],"_",dateRange[length(dateRange)],".RData"))
