#Load tum dataframes and assign movmement phases to all locations
library(dplyr)
library(ggplot2)
rm(list=ls())
#Adults 2016
larson.df<-read.csv("Data/tum files/Adults/4E Larson adult #1.s0.0075.a31000.00.csv")
inman.df<-read.csv("Data/tum files/Adults/9M (Inman adult).s0.003.a8500.00.csv")
san2.df<-read.csv("Data/tum files/Adults/9K (Santiago #2).s0.0035.a17500.00.csv")
stock.df<-read.csv("Data/tum files/Adults/8M (Stockyard adult).s0.001.a8000.00.csv")
san1.df<-read.csv("Data/tum files/Adults/4J (Santiago #1).s0.002.a48000.00.csv")
ras.df<-read.csv("Data/tum files/Adults/3K (Rasmussen adult).s0.004.a23000.00.csv")
deg2.df<-read.csv("Data/tum files/Adults/8K (Deglman #2).s0.0075.a36000.00.csv")
waub2.df<-read.csv("Data/tum files/Adults/0J (Waubun adult #2).s0.02.a18000.00.csv")
deg3.df<-read.csv("Data/tum files/Adults/5J (Deglman #3).s0.0025.a19000.00.csv")
bag.df<-read.csv("Data/tum files/Adults/2M (Bagley).s0.004.a32000.00.csv")
ogema.df<-read.csv("Data/tum files/Adults/4M (Ogema adult).s0.005.a15000.00.csv")
hel.df<-read.csv("Data/tum files/Adults/5K (Helliksen July adult 2015).s0.01.a18000.00.csv")
deg1.df<-read.csv("Data/tum files/Adults/0K Deglman #1.s0.004.a30000.00.csv")
san3.df<-read.csv("Data/tum files/Adults/1J (Santiago #3).s0.004.a23500.00.csv")
waub1.df<-read.csv("Data/tum files/Adults/4K (Waubun #1).s0.004.a33500.00.csv")
lind.df<-read.csv("Data/tum files/Adults/6E (Linden-Kologi).s0.002.a37000.00.csv")
###################################################################################

#assign periods of roaming and settled

#Larson #1
larson.df$phase<-NA
# larson.df[6723:6910,"phase"]<-"roam"  #these are excluded beceause they are fall staging
larson.df[1:6722,"phase"]<-"settled"

#Inman
inman.df$phase<-"settled"

#Santiago #2
san2.df$phase<-"settled"

#Stockyard
stock.df$phase<-"settled"

#Santiago #1
san1.df$phase<-NA
san1.df[4286:4669,"phase"]<-"roam"
san1.df[c(1:4285, 4670:7944),"phase"]<-"settled"

#Rasmussen
ras.df$phase<-"settled"

#Deglman #2
deg2.df$phase<-"settled"

#Waubun #2
waub2.df$phase<-"settled"

#Deglman #3
deg3.df$phase<-NA
deg3.df[1:5800,"phase"]<-"settled"
# deg3.df[5801:7004,"phase"]<-"roam"  #these are excluded because they are fall staging

#Bagley
bag.df$phase<-NA
# bag.df[6351:6888,"phase"]<-"roam"  #these are excluded because they are fall staging
bag.df[1:6350,"phase"]<-"settled"

#Ogema
ogema.df$phase<-"settled"

#Helliksen July 2015
hel.df$phase<-"settled"

#Deglman #1
deg1.df$phase<-"settled"

#Santiago #3
san3.df$phase<-"settled"

#Waubun #1
waub1.df$phase<-NA
waub1.df[c(5123:5327),"phase"]<-"roam"     # the first 107 locations were excluded becuase they were spring migration
waub1.df[c(108:5122, 5328:7564),"phase"]<-"settled"

#Linden-Kologi
lind.df$phase<-NA
# lind.df[4601:4924,"phase"]<-"roam"   #removed for fall staging
lind.df[1:4600,"phase"]<-"settled"

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
write.csv(t, "output/adults2016.tum.csv")

ggplot(t, aes(x=phase, y=mean.duration))+geom_boxplot()
ggplot(t, aes(x=phase, y=mean.revisitation))+geom_boxplot()

