# Event chart of roaming vs settled periods
library(ggplot2)
library(dplyr)

rm(list=ls())
#Import the raw 'dist2centroid' dataframes
adult17<-read.csv("output/comp.lavielle.adult.2017.df.csv")
colt17<-read.csv("output/lavielle.colt.2017.df.csv")
adult16<-read.csv("output/comp.lavielle.adult.df.2016.csv")
colt16<-read.csv("output/comp.lavielle.colt.df.2016.csv")

#Fix name mistake
colt17$id<-recode(colt17$id, "(Pelican Rapids colt)"="6A (Pelican Rapids colt)")

#add year and age
adult17$year<-2017
colt17$year<-2017
adult16$year<-2016
colt16$year<-2016

adult17$age<-"adult"
colt17$age<-"colt"
adult16$age<-"adult"
colt16$age<-"colt"

#Take out adult roaming at beginning and end of season 2016
# The beginning (only one crane) because it was getting to its territory
adult16<-adult16[!(adult16$id=="4E Larson adult #1"&adult16$mode=="roam"),]
adult16<-adult16[!(adult16$id=="5J (Deglman #3)"&adult16$mode=="roam"),]
adult16<-adult16[!(adult16$id=="2M (Bagley)"&adult16$mode=="roam"),]
adult16<-adult16[!(adult16$id=="6E (Linden-Kologi)"&adult16$mode=="roam"),]
#Not removing Waubun1 because the 2nd roaming is legit and the first was cut out in last script.

# The one remaining adult roaming period (Santiago #1) was during the middle of the season
# and seems like it should stay in.
#combine
dat<-rbind(adult17, colt17, adult16, colt16)


apply(is.na(dat),2,sum)
dat<-na.omit(dat)
#Event Chart
p<-ggplot(data=dat, aes(x=day, y=id, colour=mode))+geom_line(size=1.5)+
  facet_grid(age~year, scales='free')+ggtitle("Movement Phases for April 1 to August 30, 2016-2017")+
  theme(plot.title=element_text(hjust=0.5))+xlab("Julian Date")+
  theme(strip.text = element_text(size=14))

averages<-dat%>%
  group_by(age, year, mode, id)%>%
  summarise(sum(length(unique(day))))

# For some reason the 3-day roaming period for Pelican Rapids colt 2017 isn't showing up in 
# the plot, maybe because it's such a short period, it's in the dataframe correctly though.

# p1<-ggplot(data=dat, aes(x=day, y=id, colour=age))+geom_line(size=1.5)+
#   facet_grid(mode~., scales='free')+ggtitle("Movement Phases for April 1 to August 30, 2016")+
#   theme(plot.title=element_text(hjust=0.5))+xlab("Julian Date")+
#   theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
#         strip.text = element_text(size=14))

##################################################################
#Spruce up this image (shows dist2centroid vs time)
ggplot(dat, aes(x=day, y=dist2centroid, colour=age))+
  facet_grid(~year)+geom_line()+stat_smooth()+
  xlab("Julian Date")+ylab("Distance to Centroid")+
  ggtitle("Roaming over Time 2016-2017")+
  theme(plot.title=element_text(hjust=0.5))
##############################################################
