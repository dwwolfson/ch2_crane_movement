# Centroid Lavielle Colts 2017
library(dplyr)
library(sp)
library(lubridate)
library(tlocoh)
library(adehabitatLT)

rm(list=ls())
source("Scripts/distance_to_centroid_function.R")

setwd("C:/Users/David/Documents/R/CraneMovement/scripts/t-locoh")

#Import csv of all locations from Movebank for April 1-August 31, 2017
df<-read.csv("Data/all_cranes_2017_april1_thru_aug31.csv")

#Drop columns
df<-df[,c(4,5, 17, 23)]
head(df)

#Renames variables to make simpler
colnames(df)[3]<-"id"
colnames(df)[4]<-"loctime"

df$loctime<-as.character(df$loctime)
df$loctime<-as.POSIXct(df$loctime, format="%Y-%m-%d %H:%M:%S",
                       tz="America/Chicago")
apply(is.na(df),2,sum)
df<-na.omit(df)
summary(df$location.long)
df<-df[df$location.long<(-93),] 

#Assign population, gender, age
df$age<-NULL
adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" ,"5K (Helliksen July adult 2015)", "5M (Beaulieu adult)" , "6E (Linden-Kologi)", "8K (Deglman #2)" ,
              "8M (Stockyard adult)", "9A  (Santer)" , "9C (Helliksen adult April 2015)",
              "9K (Santiago #2)" ,"9M (Inman adult)" )

df<-mutate(df, age=ifelse(id%in%adult.list, "adult", "colt")) 

#Assign population
MCP.list<-c("2A (Helliksen July 2015 colt)" ,"2E Beaulieu colt" ,"3E (Wambach adult)",
            "4K (Waubun #1)" ,"4M (Ogema adult)" ,"5K (Helliksen July adult 2015)",
            "5M (Beaulieu adult)","6E (Linden-Kologi)" ,
            "7C (Helliksen colt 2014)" ,"9C (Helliksen adult April 2015)","9J  (Waubun colt)")
df$pop<-NULL
df<-mutate(df, pop=ifelse(id%in%MCP.list, "MCP", "EP"))

#Assign gender
male.list<-c("3A  (Barb Wire)","2E Beaulieu colt" ,"3M  (Callaway adult)",
             "7M (E of Mud Lake colt)", "7A  (Hackensack)", "7C (Helliksen colt 2014)",
             "2A (Helliksen July 2015 colt)" ,"9C (Helliksen adult April 2015)",
             "5K (Helliksen July adult 2015)", "9M (Inman adult)","1A (Inman colt #2)" ,
             "3C (Larson adult #2)" ,  "7K (Larson colt #1)" , "6E (Linden-Kologi)",
             "2C (Melby adult)", "1K (Melby colt #2)","1M (Min/Maint Rd)",
             "7E (Nora Township colt)" ,"6A (Pelican Rapids colt)","3K (Rasmussen adult)",
             "6C (Rice Lake colt 2014)" , "3E (Wambach adult)" ,"9J  (Waubun colt)", "0J (Waubun adult #2)")
df$gender<-NULL
df<-mutate(df, gender=ifelse(id%in%male.list, "male", "female"))

#Drop adults
df<-df[df$age=="colt",]

df<-droplevels(df)
table(df$id)

#Spatial Stuff
df.sp<-SpatialPoints(df[c("location.long","location.lat")], 
                     proj4string =CRS("+proj=longlat +ellps=WGS84"))

df.sp<-spTransform(df.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                        +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))

df.utm<-coordinates(df.sp)
colnames(df.utm)<-c("x","y")
df$loctime<-as.character(df$loctime)
df$loctime<-as.POSIXct(x=df$loctime,format="%Y-%m-%d %H:%M:%S", tz="America/Chicago")
df.loctimes<-df$loctime
df.lxy<-xyt.lxy(xy=df.utm, dt=df.loctimes,id=df$id,
                proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5
                                  +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs " ),
                tau.diff.max = 0.25)

#   !!! Note that I got the following error message until I added the tau.diff.max argument:
#   Error in xyt.rw.params.dt.int(id = id, xy = xy, dt = dt, dt.int.round.to = dt.int.round.to,  : 
# tlocoh is having a problem computing the 'maximum observed point-to-point speed'. When it
# filters out pairs of points whose sampling interval differs from the median by more than
# tau.diff.max (0.23), there are no pairs left! Try increasing tau.diff.max, or set
# tau.diff.max to zero to disabling filtering completely.

# All the other t-locoh analysis has the default tau.diff.max level of 0.02.  This filters the 
# points differently and might have a big impact, I'm not sure at all.



table(df$id)
larson1<-lxy.subset(df.lxy, id="7K (Larson colt #1)")
plot(larson1)
d2ctr(larson1)
# Larson colt #1 is missing almost the whole season and should be excluded.  This transmitter 
# is still connecting, but for some reason missed the entire summer of data.  Argh!

melby<-lxy.subset(df.lxy, id="1K (Melby colt #2)")
plot(melby)
d2ctr(melby)
# I think this one shoudl be dropped as well because there is no data until almost June,
# so the period of most interest in the beginning of the summer is missing.

dower<-lxy.subset(df.lxy, id="4A (Dower colt)")
plot(dower)
d2ctr(dower)
# This one is a little harder to decide.  It missed a bunch of data (basically nothing from mid-June
# till mid-August), but it has info from APril 1-mid-June.  I'm incline to include it, but I think
# for the sake of consistency, it should be dropped since it has so few relative to other colts.
# (This one has a huge chunk of data missing Jan-March, then Andrew changed the duty-cycle to only 
# get 6 locations a day, but it didn't help.)  What he did was acutally harmful instead of helpful
# because then it didn't collect full data all summer.  Basically he said it was dying, so between
# 1) it's be labeled inconsistent by CTT, 2) It had zero for a huge range, 3) It was taking locs
# at a much coarser scale when it was getting data ( I think).  So it's cut.

mud<-lxy.subset(df.lxy, id="0M (N of Mud Lake)")
plot(mud)
d2ctr(mud)
#This colt is missing data from July and August, but has steady data before that.  
#I'll include.
######################################################################################
# # So the excluded colts from 2017 are:
# 1) 1K Melby #2
# 2) 2A Hellikson
# 3) Barb Wire
# 4) dower
# 5) 7K larson1
# 
# So there are only 6 colts left.
######################################################################################
#dataframe to store comprehensive results
res<-data.frame()
dat<-data.frame(id=NA, set_roam=NA, n_days=NA, n_obs=NA, max=NA, sd=NA, mean=NA, median=NA, IQR=NA)
###############################################################################
# 0M (N of Mud Lake)
mud<-lxy.subset(df.lxy, id="0M (N of Mud Lake)")
plot(mud)
mud.d2<-d2ctr(mud)
mud.d2$day<-yday(mud.d2$date)
# Super settled for the entire period

#settled
set<-mud.d2
s1<-max(set$dist2centroid)  #379
s2<-sd(set$dist2centroid)    #36
s3<-mean(set$dist2centroid)  #14
s4<-median(set$dist2centroid) #
s5<-IQR(set$dist2centroid)    #

dat[1,]<-c("0M (N of Mud Lake)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

mud.d2$mode<-"settled"
mud.d2$id<-"0M (N of Mud Lake)"
res<-rbind(res, mud.d2)
#############################################################################
# 5A (W Staples colt #1)
staples<-lxy.subset(df.lxy, id="5A (W Staples colt #1)")
staples.d2<-d2ctr(staples)
plot(staples)
staples.d2$day<-yday(staples.d2$date)
# staples.lav<-lavielle(staples.d2$dist2centroid, Lmin=5, Kmax=10)
# chooseseg(staples.lav)
# findpath(staples.lav, 5)
# Settled the whole time?  Although you can tell by the color of the points when plotting 
# the lxy object that there was "relative roaming", and also by looking at the d2ctr plot.


# settled
set<-staples.d2
s1<-max(set$dist2centroid)  #4,511
s2<-sd(set$dist2centroid)   #563
s3<-mean(set$dist2centroid) #711
s4<-median(set$dist2centroid) # 504
s5<-IQR(set$dist2centroid)    # 539
dat[2,]<-c("5A (W Staples colt #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)


s.df<-staples.d2
s.df$mode<-"settled"
s.df$id<-"5A (W Staples colt #1)"
res<-rbind(res, s.df)
###################################################################################
# 5C (Stockyard colt)
stock<-lxy.subset(df.lxy, id="5C (Stockyard colt)")
stock.d2<-d2ctr(stock)
plot(stock)
stock.d2$day<-yday(stock.d2$date)
stock.lav<-lavielle(stock.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(stock.lav)
findpath(stock.lav, 5)
# Roaming the first half of April.  I'll cut off the first day of migration.

# settled
set<-stock.d2[c(297:7314),]
s1<-max(set$dist2centroid)  #51,238
s2<-sd(set$dist2centroid)   #3,315
s3<-mean(set$dist2centroid) #32,265
s4<-median(set$dist2centroid) #32,292
s5<-IQR(set$dist2centroid)    #2,684
dat[3,]<-c("5C (Stockyard colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

#roam
roam<-stock.d2[c(13:296),]
r1<-max(roam$dist2centroid)   #170,755
r2<-sd(roam$dist2centroid)    #27,009
r3<-mean(roam$dist2centroid)   #61,395
r4<-median(roam$dist2centroid)  # 57,855
r5<-IQR(roam$dist2centroid)     #41,573
dat[4,]<-c("5C (Stockyard colt)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

st.df<-stock.d2
st.df$mode<-NA
st.df[c(297:7314),"mode"]<-"settled"
st.df[c(13:296),"mode"]<-"roam"
st.df$id<-"5C (Stockyard colt)"
res<-rbind(res, st.df)
#############################################################################
# 6A (Pelican Rapids colt)
pr<-lxy.subset(df.lxy, id="6A (Pelican Rapids colt)")

pr.d2<-d2ctr(pr)
plot(pr)
pr.d2$day<-yday(pr.d2$date)
pr.lav<-lavielle(pr.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(pr.lav)
findpath(pr.lav, 5)
pr.d2$date[c(1,4275, 4375, 5630, 6260)]
#Not the most encamped colt, but relatively settled until Aug 20, 
# then roaming, but it's fall staging.  

#Fall staging roaming should be removed, but there is legit roaming in late July???
# I'll just chop the last roaming off now, because it would be hard to subset it out 
# of the bigger dataframe later (I think...)

# This is an example (maybe the only one) where the locations are included for the centroid
# summaries, but not in the res dataframe.   *******************************

# settled
set<-pr.d2[c(1:4275, 4375:5630),]
s1<-max(set$dist2centroid)  #5,121
s2<-sd(set$dist2centroid)   #1,133
s3<-mean(set$dist2centroid) #1,508
s4<-median(set$dist2centroid) # 1,360
s5<-IQR(set$dist2centroid)    # 2,378
dat[5,]<-c("(Pelican Rapids colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

#roam
roam<-pr.d2[c(4276:4374, 5631:6260),]
r1<-max(roam$dist2centroid)   #12,845
r2<-sd(roam$dist2centroid)    #4,583
r3<-mean(roam$dist2centroid)   #6,291
r4<-median(roam$dist2centroid)  # 6,477
r5<-IQR(roam$dist2centroid)     # 9,388
dat[6,]<-c("(Pelican Rapids colt)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)


p.df<-pr.d2
p.df$mode<-NA
p.df[c(4276:4374),"mode"]<-"roam"
p.df[c(1:4275, 4375:5630),"mode"]<-"settled"
p.df$id<-"6A (Pelican Rapids colt)"
res<-rbind(res, p.df)
####################################################################################
# 7E (Nora Township colt) 
nora<-lxy.subset(df.lxy, id="7E (Nora Township colt)")
nora.d2<-d2ctr(nora)
plot(nora)
nora.d2$day<-yday(nora.d2$date)
nora.lav<-lavielle(nora.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(nora.lav)
findpath(nora.lav, 2)
#  ****Excluding the first part en route to territory********
#The whole season is very settled, but there is a large spike from when the colt
# was still arriving to its HR for the summer.  I'll exclude that from the 
# distance metrics and assign settled to the entire time series.

# settled
set<-nora.d2[c(50:8124),]
s1<-max(set$dist2centroid)  #9,628
s2<-sd(set$dist2centroid)   #1,172
s3<-mean(set$dist2centroid) #2,600
s4<-median(set$dist2centroid) # 2,286
s5<-IQR(set$dist2centroid)    # 1,320
dat[7,]<-c("7E (Nora Township colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)


n.df<-nora.d2[50:8124,]
n.df$mode<-"settled"
n.df$id<-"7E (Nora Township colt)"
res<-rbind(res, n.df)
######################################################################################
# 7J (Melby colt #1)
melby<-lxy.subset(df.lxy, id="7J (Melby colt #1)")
melby.d2<-d2ctr(melby)
plot(melby)
melby.d2$day<-yday(melby.d2$date)
melby.lav<-lavielle(melby.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(melby.lav)
findpath(melby.lav, 6)
melby.d2$date[c(1,2400, 3560, 7588)]
# The most prospecting of any colt I think.  Full roaming for all of April and 
# half of May. Sorta settled for a bit the second half of May, but I'm going to say
# roaming till the end of May for simplicity sake instead of trying to chop it up too fine.

# settled
set<-melby.d2[c(3800:7588),]
s1<-max(set$dist2centroid)  #64,071
s2<-sd(set$dist2centroid)   #4,288
s3<-mean(set$dist2centroid)#53,512
s4<-median(set$dist2centroid) # 53,896
s5<-IQR(set$dist2centroid)    # 2,472
dat[8,]<-c("7J (Melby colt #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

#roam
roam<-melby.d2[c(1:3799),]
r1<-max(roam$dist2centroid)   # 154,636
r2<-sd(roam$dist2centroid)    # 40,157
r3<-mean(roam$dist2centroid)   # 87,089
r4<-median(roam$dist2centroid)  # 72,120
r5<-IQR(roam$dist2centroid)     # 76,232
dat[9,]<-c("7J (Melby colt #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)


m.df<-melby.d2
m.df$mode<-NA
m.df[c(1:3799),"mode"]<-"roam"
m.df[c(3800:7588),"mode"]<-"settled"
m.df$id<-"7J (Melby colt #1)"
res<-rbind(res, m.df)
###############################################################################
write.csv(dat, "output/colt.centroid.summaries.2017.df.csv")
write.csv(res, "output/lavielle.colt.2017.df.csv")
