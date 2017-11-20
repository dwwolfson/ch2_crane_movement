#Comparisons of colts/adults 
rm(list=ls())
library(ggplot2)
library(gridExtra)
adults16<-read.csv("output/adults2016.tum.csv")
colts16<-read.csv("output/colts2016.tum.csv")
adults17<-read.csv("output/adults2017.tum.csv")
colts17<-read.csv("output/colts2017.tum.csv")

adults16$age<-"adult"
adults16$year<-2016

adults17$age<-"adult"
adults17$year<-2017

colts16$age<-"colt"
colts16$year<-2016

colts17$age<-"colt"
colts17$year<-2017

df<-rbind(adults16, colts16, adults17, colts17)
df$year<-as.factor(df$year)

#mean duration
ggplot(df, aes(x=phase, y=mean.duration, color=age))+
      geom_boxplot()+facet_wrap(~year)

#hard to see because one huge outlier.  consider removing
g1.duration<-ggplot(df, aes(x=phase, y=mean.duration, color=age))+
  geom_boxplot()+facet_wrap(~year)+ylim(c(0,300))+
  ggtitle("Average duration of hull visit time (two outliers removed)")+
  theme(plot.title=element_text(hjust=0.5))#alternative (filtered y-axis)
#Not super different between the two movement phases

#mean revisitation
g1.revisit<-ggplot(df, aes(x=phase, y=mean.revisitation, color=age))+
  geom_boxplot()+facet_wrap(~year)+
  ggtitle("Average number of hulls revisited")+
  theme(plot.title=element_text(hjust=0.5))

#max revisitation
g2.revisit<-ggplot(df, aes(x=phase, y=max.revisitation, color=age))+
  geom_boxplot()+facet_wrap(~year)+
  ggtitle("Max number of hulls revisited")+
  theme(plot.title=element_text(hjust=0.5))

#max duration
g2.duration<-ggplot(df, aes(x=phase, y=max.duration, color=age))+
  geom_boxplot()+facet_wrap(~year)+
  ggtitle("Max duration of hull visit time (6 outliers removed)")+
  theme(plot.title=element_text(hjust=0.5))+ylim(c(0,1500))

#sd duration
g3.duration<-ggplot(df, aes(x=phase, y=sd.duration, color=age))+
  geom_boxplot()+facet_wrap(~year)+
  ggtitle("Standard deviation of duration of hull visit time (one outlier removed)")+
  theme(plot.title=element_text(hjust=0.5))+ylim(c(0,700))

#sd revisitation
g3.revisit<-ggplot(df, aes(x=phase, y=sd.revisitation, color=age))+
  geom_boxplot()+facet_wrap(~year)+
  ggtitle("Standard deviation of number of hulls revisited")+
  theme(plot.title=element_text(hjust=0.5))

grid.arrange(g1.revisit, g2.revisit, g3.revisit)
grid.arrange(g1.duration, g2.duration, g3.duration)

