# Load tum files COlts 2017
library(dplyr)
library(ggplot2)
rm(list=ls())

#Colts 2017
list.files("Data/tum files/2017/Colts")
mud.df<-read.csv("Data/tum files/2017/Colts/0M (N of Mud Lake).s0.004.a2900.00.csv")
staples.df<-read.csv("Data/tum files/2017/Colts/5A (W Staples colt #1).s0.008.a30000.00.csv")
stock.df<-read.csv("Data/tum files/2017/Colts/5C (Stockyard colt).s0.0015.a125500.00.csv")
pr.df<-read.csv("Data/tum files/2017/Colts/6A (Pelican Rapids colt).s0.005.a40500.00.csv")                 
nora.df<-read.csv("Data/tum files/2017/Colts/7E (Nora Township colt).s0.03.a111000.00.csv")
melby.df<-read.csv("Data/tum files/2017/Colts/7J (Melby colt #1).s0.002.a282500.00.csv")                 
##########################################################################################
#assign periods of roaming and settled

# Mud Lake
mud.df$phase<-"settled"

#Staples
staples.df$phase<-"settled"

#Stockyard
stock.df$phase<-NA
stock.df[13:296,"phase"]<-"roam"         #cut off first 12 locations for spring migration
stock.df[297:7314, "phase"]<-"settled"  

#Pelican Rapids
pr.df$phase<-NA
pr.df[c(1:4275, 4375:5630), "phase"]<-"settled"
pr.df[c(4276:4374, 5631:6260), "phase"]<-"roam"

#Nora
nora.df$phase<-NA
nora.df[50:8124, "phase"]<-"settled"   #cut off first 49 locations for spring migration

#Melby
melby.df$phase<-NA
melby.df[3800:7588, "phase"]<-"settled"
melby.df[1:3799, "phase"]<-"roam"

# Put dataframes back together (there can't be any other dataframes in the 
# global environment for this to work)
tum.df<-do.call(rbind, eapply(.GlobalEnv,function(x) if(is.data.frame(x)) x)) #41877 obs
apply(is.na(tum.df),2,sum)
tum.df<-na.omit(tum.df)

#aggregate revisit and duration rates during each phase
t<-tum.df%>%
  group_by(phase, id)%>%
  summarise(mean.duration=mean(mnlv.86400),
            sd.duration=sd(mnlv.86400),
            max.duration=max(mnlv.86400),
            mean.revisitation=mean(nsv.86400),
            sd.revisitation=sd(nsv.86400),
            max.revisitation=max(nsv.86400))
# write.csv(t, "output/colts2017.tum.csv")

ggplot(t, aes(x=phase, y=mean.duration))+geom_boxplot()
ggplot(t, aes(x=phase, y=mean.revisitation))+geom_boxplot()
#