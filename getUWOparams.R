# scrape info from UofWyo Skew T/radiosonde data
# MAC 08/07/2019

library(rvest)
library(stringr)
library(tidyr)

# loop through days
dateRange<-seq(as.Date("2019/06/15"), as.Date("2019/09/30"), by = "day")
capeList= list()
pwatList= list()

for (k in 1:length(dateRange)) {

  year<-format(dateRange[k], "%Y")
  month<-format(dateRange[k], "%m")
  day<- format(dateRange[k], "%d")
  
#URL<-'http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=2019&MONTH=08&FROM=0600&TO=0600&STNM=72274'
URL<-paste0('http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=',year,'&MONTH=',month,'&FROM=',day,'00&TO=',day,'00&STNM=72274')

rawText<-read_html(URL)%>% html_nodes("pre") %>% html_text()

capeList[[k]]<-stringr::str_extract(rawText[2], "Convective Available Potential Energy:.{0,20}")
pwatList[[k]]<-stringr::str_extract(rawText[2], "for entire sounding:.{0,6}")

print(dateRange[k])

}

# extract CAPE into DF
capeDF<-data.frame(matrix(unlist(capeList)))
capeDF<-capeDF %>% separate(1,c(NA,"CAPE"), sep = ":")
capeDF$CAPE<-as.numeric(capeDF$CAPE)
capeDF$date<-dateRange

# extract PWAT
pwatDF<-data.frame(matrix(unlist(pwatList)))
pwatDF<-pwatDF %>% separate(1,c(NA,"pwat"), sep = ":")
pwatDF$pwat<-as.numeric(pwatDF$pwat)
pwatDF$date<-dateRange

# plot them out
library(cowplot)

p1<-ggplot(capeDF, aes(date,CAPE))+
  geom_bar(stat = 'identity', fill="firebrick")+
  ggtitle("2018 Tucson 5pm SkewT - CAPE")

p2<-ggplot(pwatDF, aes(date,pwat))+
  geom_bar(stat = 'identity', fill='darkgreen')+
  ggtitle("2018 Tucson 5pm SkewT - PWAT")

plot_grid(p1,p2, nrow = 2)