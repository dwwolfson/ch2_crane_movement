# Figure for time vs dist2centroid

rm(list=ls())
library(ggplot2)
adult17<-read.csv("output/comp.lavielle.adult.2017.df.csv")
colt17<-read.csv("output/lavielle.colt.2017.df.csv")
adult16<-read.csv("output/comp.lavielle.adult.df.2016.csv")
colt16<-read.csv("output/comp.lavielle.colt.df.2016.csv")


adult16$age<-"adult"
adult16$year<-2016
colt16$age<-"colt"
colt16$year<-2016
adult17$age<-"adult"
adult17$year<-2017
colt17$age<-"colt"
colt17$year<-2017

str(adult16)
ggplot(adult16, aes(day, dist2centroid))+
  geom_point()+geom_smooth()
adults<-rbind(adult17, adult16)
colts<-rbind(colt16, colt17)
colts<-na.omit(colts)
ggplot(colts, aes(day, dist2centroid, colour=id))+
  geom_line()+facet_grid(~year)

df<-rbind(adult17, colt17, adult16, colt16)
names(df)
str(df)
ggplot(df, aes(x=day, y=dist2centroid, colour=mode))+
  geom_point()+geom_smooth()+facet_grid(~age)

