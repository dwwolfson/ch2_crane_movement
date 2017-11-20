#Load tum df's for Adults 2017
library(dplyr)
library(ggplot2)
rm(list=ls())


#Adults 2017
list.files("Data/tum files/2017/Adults")
deg1.df<-read.csv("Data/tum files/2017/Adults/0K Deglman #1.s0.004.a16500.00.csv")
san3.df<-read.csv("Data/tum files/2017/Adults/1J (Santiago #3).s0.00175.a75500.00.csv")
bag.df<-read.csv("Data/tum files/2017/Adults/2M (Bagley).s0.002.a16000.00.csv")
san1.df<-read.csv("Data/tum files/2017/Adults/4J (Santiago #1).s0.002.a52500.00.csv")
waub.df<-read.csv("Data/tum files/2017/Adults/4K (Waubun #1).s0.00375.a42500.00.csv")
ogema.df<-read.csv("Data/tum files/2017/Adults/4M (Ogema adult).s0.006.a26000.00.csv")
hel.df<-read.csv("Data/tum files/2017/Adults/5K (Helliksen July adult 2015).s0.01.a17500.00.csv")
deg2.df<-read.csv("Data/tum files/2017/Adults/8K (Deglman #2).s0.004.a43500.00.csv")
stock.df<-read.csv("Data/tum files/2017/Adults/8M (Stockyard adult).s0.002.a44000.00.csv")
san2.df<-read.csv("Data/tum files/2017/Adults/9K (Santiago #2).s0.0013.a12000.00.csv")
inman.df<-read.csv("Data/tum files/2017/Adults/9M (Inman adult).s0.02.a17900.02.csv")
##########################################################################################
#assign periods of roaming and settled

# Deglman #1
deg1.df$phase<-"settled"

#Santiago #3
san3.df$phase<-NA
san3.df[c(1:999), "phase"]<-"roam"  # not including 5971:6011 (fall staging)
san3.df[1000:5970, "phase"]<-"settled"

#Bagley
bag.df$phase<-"settled"

#Santiago #1
san1.df$phase<-NA
san1.df[c(1:1740, 2550:7534),"phase"]<-"settled"
san1.df[1741:2549, "phase"]<-"roam"

#Waubun
waub.df$phase<-NA
waub.df[50:5588, "phase"]<-"settled"  #removed the first 49 loc's for spring migration

#Ogema
ogema.df$phase<-"settled"

#Helliksen
hel.df$phase<-"settled"

#Deglman #2
deg2.df$phase<-"settled"

#Stockyard
stock.df$phase<-NA
stock.df[80:3102,"phase"]<-"settled"  #cut off the first 79 locations for spring migrations

#Santiago #2
san2.df$phase<-
san2.df[80:8099,"phase"]<-"settled" #cut off first 79 locations for spring migration

#Inman
inman.df$phase<-"settled"

# Put dataframes back together (there can't be any other dataframes in the 
# global environment for this to work)
tum.df<-do.call(rbind, eapply(.GlobalEnv,function(x) if(is.data.frame(x)) x))
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
# write.csv(t, "output/adults2017.tum.csv")

ggplot(t, aes(x=phase, y=mean.duration))+geom_boxplot()
ggplot(t, aes(x=phase, y=mean.revisitation))+geom_boxplot()





