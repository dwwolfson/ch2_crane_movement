#using time-use metrics from t-locoh
library(dplyr)
library(ggplot2)
rm(list=ls())

#All colts from 2016 (April 1- August 30)
cal.df<-read.csv("Data/tum files/0C (Callaway colt 2014).s0.004.a260000.00.csv")
mud.df<-read.csv("Data/tum files/0M (N of Mud Lake).s0.002.a235000.00.csv")
melby2.df<-read.csv("Data/tum files/1K (Melby colt #2).s0.001.a74000.00.csv")
hel.df<-read.csv("Data/tum files/2A (Helliksen July 2015 colt).s0.1.a92000.00.csv")
dower.df<-read.csv("Data/tum files/4A (Dower colt).s0.002.a215000.00.csv")
staples.df<-read.csv("Data/tum files/5A (W Staples colt #1).s0.002.a80000.00.csv")
stock.df<-read.csv("Data/tum files/5C (Stockyard colt).s0.002.a190000.00.csv")
pelican.df<-read.csv("Data/tum files/6A (Pelican Rapids colt).s0.002.a305000.00.csv")
rice.lake.df<-read.csv("Data/tum files/6C (Rice Lake colt 2014).s0.005.a101000.00.csv")
larson2.df<-read.csv("Data/tum files/6M (Larson colt #2).s0.002.a2e+05.00.csv")
nora.df<-read.csv("Data/tum files/7E (Nora Township colt).s0.0025.a94000.00.csv")
melby.df<-read.csv("Data/tum files/7J (Melby colt #1).s0.002.a176000.00.csv")
larson1.df<-read.csv("Data/tum files/7K (Larson colt #1).s0.003.a160000.00.csv")


#assign periods of roaming and settled
#callaway
cal.df$phase<-NA
cal.df[817:5791,"phase"]<-"settled"
cal.df[1:816,"phase"]<-"roam"

#dower
dower.df$phase<-NA
dower.df[c(1:1287, 2275:2611),"phase"]<-"roam"
dower.df[c(1288:2274, 2612:8203),"phase"]<-"settled"

#helliksen
hel.df$phase<-"settled"

#larson #1
larson1.df$phase<-NA
larson1.df[c(1:1007, 1880:3400, 3458:7102),"phase"]<-"settled"
larson1.df[c(1008:1879, 3401:3457),"phase"]<-"roam"

#larson #2
larson2.df$phase<-NA
larson2.df[c(431:479, 2231:2399),"phase"]<-"roam"
larson2.df[c(1:430,480:2230, 2400:4683),"phase"]<-"settled"

#melby #1
melby.df$phase<-NA
melby.df[c(1:250, 1250:1350, 3825:3875, 4600:4850),"phase"]<-"roam"
melby.df[c(251:1249, 1351:3824, 3876:4599, 4850:6875),"phase"]<-"settled"

#melby #2 
melby2.df$phase<-NA
melby2.df[c(267:300, 1200:1280),"phase"]<-"roam"
melby2.df[c(6:266, 301:1199, 1281:3287),"phase"]<-"settled"

# mud lake
mud.df$phase<-NA
mud.df[1:1470, "phase"]<-"roam"
mud.df[1471:7547, "phase"]<-"settled"

#nora
nora.df$phase<-NA
nora.df[1:211, "phase"]<-"roam"
nora.df[212:7452, "phase"]<-"settled"

#pelican rapids
pelican.df$phase<-NA
pelican.df[62:1120, "phase"]<-"roam"           #took out first 2 days of spring migration
pelican.df[1121:7085, "phase"]<-"settled"

#rice lake  !!this crane had lots of gaps in data this summer!!
rice.lake.df$phase<-NA
rice.lake.df[1:3868, "phase"]<-"settled"  

# staples
staples.df$phase<-NA
staples.df[1:280, "phase"]<-"roam"
staples.df[281:8120,"phase"]<-"settled"

#stock
stock.df$phase<-NA
stock.df[c(34:500, 1470:1600,2650:3000, 3700:3750),"phase"]<-"roam"  #took out first 33 locations for spring migration
stock.df[c(501:1469, 1601:2649, 3001:3699, 3751:7088),"phase"]<-"settled"

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
# write.csv(t, "output/colts2016.tum.csv")
            
ggplot(t, aes(x=phase, y=mean.duration))+geom_boxplot()
ggplot(t, aes(x=phase, y=mean.revisitation))+geom_boxplot()
