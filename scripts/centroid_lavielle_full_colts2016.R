# JUne 2017 
# T-Locoh for revisitation/duration of use

library(ggplot2)
library(tlocoh)
library(lubridate)
library(sp)
library(rgdal)
library(dplyr)
library(adehabitatLT)


rm(list=ls())
source("Scripts/distance_to_centroid_function.R")
df<-read.csv("~/R/CraneMovement/scripts/t-locoh/Data/df16.csv") #takes a little while to load
df<-df[,-1]

#SUbset to only spring 2016 for now 
#copy of full dataframe
df1<-df
df<-df[df$Month>3&df$Month<9,]

#Drop adults
df<-df[df$age=="colt",]

#Also drop a colt that had issues with sampling schedules (see below)
# df<-df[!df$id=="6M (Larson colt #2)",]  could drop because it had a large gap missing
# df<-df[!df$id=="1K (Melby colt #2)",]
# df<-df[!df$id=="6C (Rice Lake colt 2014)",]
# df<-df[!df$id=="6M (Larson colt #2)",]
# df<-df[!df$id=="7K (Larson colt #1)",]
df<-droplevels(df)
unique(df$id)



df<-df[df$location.long<(-93),]
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
                                  +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs " ))
########################################################
# lxy.plot.pt2ctr(df.lxy)
#Dataframe to hold comprehensive results
res<-data.frame()

#Dataframe to hold summary stat results
dat<-data.frame(id=NA, set_roam=NA, n_days=NA, n_obs=NA, max=NA, sd=NA, mean=NA, median=NA, IQR=NA)

#callaway
callaway<-lxy.subset(df.lxy, id="0C (Callaway colt 2014)")
cal.d2<-d2ctr(callaway)
cal.d2$day<-yday(cal.d2$date)
cal.lav<-lavielle(cal.d2$dist2centroid, Lmin = 5,Kmax = 10)
chooseseg(cal.lav)
findpath(cal.lav, 2)  #2 segments, break at 815
cal.d2$date[c(1,816)]      #Crossed -93 lon on May 7, Date of settling  May 24
# Relatively clean period of "mess" and then a clean settlement
set<-cal.d2[c(817:5791),]
s1<-max(set$dist2centroid)  #44,576
s2<-sd(set$dist2centroid)   #3,853
s3<-mean(set$dist2centroid)  #10,635
s4<-median(set$dist2centroid) # 9,652
s5<-IQR(set$dist2centroid) # 2,098
dat[1,]<-c("0C (Callaway colt 2014)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-cal.d2[c(1:816),]
r1<-max(roam$dist2centroid)   #210,638
r2<-sd(roam$dist2centroid)    #38,929
r3<-mean(roam$dist2centroid)   #85,269
r4<-median(roam$dist2centroid) # 83,366
r5<-IQR(roam$dist2centroid) # 52,181
dat[2,]<-c("0C (Callaway colt 2014)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

res<-cal.d2
res$id<-"0C (Callaway colt 2014)"
res$mode<-NA
res[1:816,"mode"]<-"roam"
res[817:5791,"mode"]<-"settled"
########################################################

#N of Mud Lake
mud<-lxy.subset(df.lxy, id="0M (N of Mud Lake)")
mud.d2<-d2ctr(mud)  
mud.d2$day<-yday(mud.d2$date)
mud.lav<-lavielle(mud.d2$dist2centroid, Lmin=5, Kmax = 10)
chooseseg(mud.lav)
findpath(mud.lav, 2)   #2 segments, break at 1468
mud.d2$date[c(1,1470)]      #Crossed -93 long on April 15, Date of settling May 11
# Relatively clean period of "mess" and then a clean settlement
set<-mud.d2[c(1471:7547),]
s1<-max(set$dist2centroid)  #65,363
s2<-sd(set$dist2centroid)   #6,023
s3<-mean(set$dist2centroid)  #21,569
s4<-median(set$dist2centroid) # 19,665
s5<-IQR(set$dist2centroid) # 465
dat[3,]<-c("0M (N of Mud Lake)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-mud.d2[c(1:1470),]
r1<-max(roam$dist2centroid)   #230,335
r2<-sd(roam$dist2centroid)    #34,262
r3<-mean(roam$dist2centroid)   #103,516
r4<-median(roam$dist2centroid) # 112,891
r5<-IQR(roam$dist2centroid) # 42,987
dat[4,]<-c("0M (N of Mud Lake)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

mud.df<-rbind(roam, set)
mud.df$mode<-NA
mud.df[1:1470,"mode"]<-"roam"
mud.df[1471:7547,"mode"]<-"settled"
mud.df$id<-"0M (N of Mud Lake)"
res<-rbind(res, mud.df)
############################################################
#Helliksen  !!! This one doesn't work out so great, mainly because there some big 1-day spikes that 
# aren't all being picked up by laviell method
helliksen<-lxy.subset(df.lxy, id="2A (Helliksen July 2015 colt)")
hel.d2<-d2ctr(helliksen)
hel.d2$day<-yday(hel.d2$date)
hel.lav<-lavielle(hel.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(hel.lav)
findpath(hel.lav, 4) 
#Upon looking at the kml from the CTT website, it was settled the whole year

set<-hel.d2
s1<-max(set$dist2centroid)  #12,523
s2<-sd(set$dist2centroid)   # 1,029
s3<-mean(set$dist2centroid)  # 1,271
s4<-median(set$dist2centroid) # 1059
s5<-IQR(set$dist2centroid) # 1,009
dat[5,]<-c("2A (Helliksen July 2015 colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)


h.df<-hel.d2
h.df$mode<-"settled"
h.df$id<-"2A (Helliksen July 2015 colt)"
res<-rbind(res, h.df)

############################################################
#Larson colt #1
larson1<-lxy.subset(df.lxy, id="7K (Larson colt #1)")
lar1.d2<-d2ctr(larson1)
lar1.d2$day<-yday(lar1.d2$date)
lar1.lav<-lavielle(lar1.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(lar1.lav)
findpath(lar1.lav, 6)  #not the greatest fit, but 3 settled phases and two roaming


set<-lar1.d2[c(1:1007, 1880:3400, 3458:7102),]
s1<-max(set$dist2centroid)  # 104,536
s2<-sd(set$dist2centroid)   # 24,515
s3<-mean(set$dist2centroid)  # 33,436
s4<-median(set$dist2centroid) # 26,914
s5<-IQR(set$dist2centroid) # 6,479
dat[6,]<-c("7K (Larson colt #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-lar1.d2[c(1008:1879, 3401:3457),]
r1<-max(roam$dist2centroid)   # 151,096
r2<-sd(roam$dist2centroid)    # 27,819
r3<-mean(roam$dist2centroid)   # 44,222
r4<-median(roam$dist2centroid) # 33,233
r5<-IQR(roam$dist2centroid) # 29,698
dat[7,]<-c("7K (Larson colt #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

lar1.df<-lar1.d2
lar1.df$mode<-NA
lar1.df[c(1008:1879, 3401:3457),"mode"]<-"roam"
lar1.df[c(1:1007, 1880:3400, 3458:7102),"mode"]<-"settled"
lar1.df$id<-"7K (Larson colt #1)"
res<-rbind(res, lar1.df)

#######################################################################################################
#dower   !!!!! THis one doesn't work well because it settled in more than one place !!!!
dower<-lxy.subset(df.lxy, id="4A (Dower colt)")
dow.d2<-d2ctr(dower)
dow.d2$day<-yday(dow.d2$date)
dow.lav<-lavielle.default(dow.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(dow.lav)   #by the method, I should choose 3 segments, and that 
# makes sense because it settled for a while, then moved a bunch more, and
# then settled longer for the rest of the summer
findpath(dow.lav, 6)
dow.d2$date[c(1,1287,2274,2611,8203)]  #crossed -93 on April 13
#  Segment 1 (Roaming): April 13-May 7
# Segment 2 (First settled phase): May 7-May 24
# Segment 3  Second brief roaming phase: May 24-May 29
# Segment 4 (Main Settlement phase) May 24 till August 31

set<-dow.d2[c(1288:2274, 2612:8203),]
s1<-max(set$dist2centroid)  #127,041
s2<-sd(set$dist2centroid)   #26,358
s3<-mean(set$dist2centroid)  #27,740    #It seems strange that the sd's are close to equal...
# I think that is a result of the two different settled areas   **********************************
s4<-median(set$dist2centroid) # 16,420
s5<-IQR(set$dist2centroid) # 1,658
dat[8,]<-c("4A (Dower colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-dow.d2[c(1:1287, 2275:2611),]
r1<-max(roam$dist2centroid)   #222,492
r2<-sd(roam$dist2centroid)    #25,824
r3<-mean(roam$dist2centroid)   #34,529
r4<-median(roam$dist2centroid) # 28,464
r5<-IQR(roam$dist2centroid) # 38,607
dat[9,]<-c("4A (Dower colt)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

d.df<-dow.d2
d.df$mode<-NA
d.df[c(1:1287, 2275:2611),"mode"]<-"roam"
d.df[c(1288:2274, 2612:8203),"mode"]<-"settled"
d.df$id<-"4A (Dower colt)"
res<-rbind(res, d.df)

##################################################################
#Staples    
staples<-lxy.subset(df.lxy, id="5A (W Staples colt #1)")
staples.d2<-d2ctr(staples)
staples.d2$day<-yday(staples.d2$date)
staples.lav<-lavielle(staples.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(staples.lav)      
findpath(staples.lav, 3)
staples.d2$date[c(1,280)]        
#Roaming April 1-9, Settle April 9, but moved around a lot for being "settled"

set<-staples.d2[c(281:8120),]
s1<-max(set$dist2centroid)  # 20,376
s2<-sd(set$dist2centroid)   # 2,688
s3<-mean(set$dist2centroid) # 5,361
s4<-median(set$dist2centroid) # 5,056
s5<-IQR(set$dist2centroid) # 3,857
dat[10,]<-c("5A (W Staples colt #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-staples.d2[c(1:280),]
r1<-max(roam$dist2centroid)   # 82,947
r2<-sd(roam$dist2centroid)    # 24,354
r3<-mean(roam$dist2centroid)  # 30,815
r4<-median(roam$dist2centroid) # 19,176
r5<-IQR(roam$dist2centroid) # 14,915
dat[11,]<-c("5A (W Staples colt #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

s.df<-staples.d2
s.df$mode<-NA
s.df[c(1:280),"mode"]<-"roam"
s.df[281:8120,"mode"]<-"settled"
s.df$id<-"5A (W Staples colt #1)"
res<-rbind(res, s.df)


###############################################################
#stock    !!!!!!!!! another example of two different setttled areas !!!!!!!!!!!!!

#  Will manually segment this one ************

stock<-lxy.subset(df.lxy, id="5C (Stockyard colt)")
stock.d2<-d2ctr(stock)
stock.d2$day<-yday(stock.d2$date)
stock.lav<-lavielle(stock.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(stock.lav)
findpath(stock.lav, 7)
stock.d2$date[c(1,34,2718,3721)]    #Cross -93 on April 29,
# still migrating for the first big chunk 1-34 (the first day of APril 29)
# Was settled April 29-June 13 (with a minor bit of roaming),
# relocated to a different area of settlement June 13-July 1
# On July 1st, seemingly returned to first place of settlement

set<-stock.d2[c(501:1469, 1601:2649, 3001:3699, 3751:7088),]
s1<-max(set$dist2centroid)  # 118,008
s2<-sd(set$dist2centroid)   # 27,953
s3<-mean(set$dist2centroid) # 33,028
s4<-median(set$dist2centroid) # 25,124
s5<-IQR(set$dist2centroid) # 8,876
dat[12,]<-c("5C (Stockyard colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-stock.d2[c(34:500, 1470:1600,2650:3000, 3700:3750),]
r1<-max(roam$dist2centroid)   # 159,975
r2<-sd(roam$dist2centroid)    # 48,300
r3<-mean(roam$dist2centroid)  # 64,520
r4<-median(roam$dist2centroid) # 41,302
r5<-IQR(roam$dist2centroid) # 88,027
dat[13,]<-c("5C (Stockyard colt)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

st.df<-stock.d2
st.df$mode<-NA
st.df[c(34:500, 1470:1600,2650:3000, 3700:3750),"mode"]<-"roam"
st.df[c(501:1469, 1601:2649, 3001:3699, 3751:7088),"mode"]<-"settled"
st.df$id<-"5C (Stockyard colt)"
res<-rbind(res, st.df)
###  Manually chose segmentation on this one  ###

###############################################################
#pelican
pelican<-lxy.subset(df.lxy, id="6A (Pelican Rapids colt)")
pelican.d2<-d2ctr(pelican)
pelican.d2$day<-yday(pelican.d2$date)
pelican.lav<-lavielle(pelican.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(pelican.lav)
findpath(pelican.lav, 3)
pelican.d2$date[c(1,39,1102)]  
# THe first day the crane crossed -93 was id'ed as a segment as well, 
#probably because it covered a lot of ground that day.
#the other segments are: Roaming April 15-May 7
# May 7-end: settled

set<-pelican.d2[c(1121:7085),]
s1<-max(set$dist2centroid)  #25,954
s2<-sd(set$dist2centroid)   #2,414
s3<-mean(set$dist2centroid)  #5,672
s4<-median(set$dist2centroid) # 4,994
s5<-IQR(set$dist2centroid) # 2,152
dat[14,]<-c("6A (Pelican Rapids colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-pelican.d2[c(62:1120),]
r1<-max(roam$dist2centroid)   #139,897
r2<-sd(roam$dist2centroid)    #36,103
r3<-mean(roam$dist2centroid)   #43,758
r4<-median(roam$dist2centroid) # 32,272
r5<-IQR(roam$dist2centroid) # 42,990
dat[15,]<-c("6A (Pelican Rapids colt)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

p.df<-pelican.d2
p.df$mode<-NA
p.df[62:1120,"mode"]<-"roam"
p.df[1121:7085,"mode"]<-"settled"
p.df$id<-"6A (Pelican Rapids colt)"
res<-rbind(res, p.df)

# a pretty easy clear cut example, good one to demonstrate the technique!  ****
###############################################################
#rice.lake
rice.lake<-lxy.subset(df.lxy, id="6C (Rice Lake colt 2014)")
ri.d2<-d2ctr(rice.lake)
ri.d2$day<-yday(ri.d2$date)
ri.lav<-lavielle(ri.d2$dist2centroid, Lmin=5, Kmax=20)
chooseseg(ri.lav)
findpath(ri.lav, 8)
#It seems really hard to segment this one, both because there are big gaps, 
# but mostly because there isn't as much large variation.  THis is because the 
# crane didn't take any long distance roaming trips, so the whole summer is settled.

set<-ri.d2[1:3868,]
s1<-max(ri.d2$dist2centroid) #27, 026
s2<-sd(ri.d2$dist2centroid) #4,409
s3<-mean(ri.d2$dist2centroid)  #11,027
s4<-median(set$dist2centroid) # 12,158
s5<-IQR(set$dist2centroid) # 5,419
dat[16,]<-c("6C (Rice Lake colt 2014)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5) #no roaming!

r.df<-ri.d2
r.df$mode<-"settled"
r.df$id<-"6C (Rice Lake colt 2014)"
res<-rbind(res, r.df)

###############################################################
#larson2
larson2<-lxy.subset(df.lxy, id="6M (Larson colt #2)")
lar2.d2<-d2ctr(larson2)
lar2.d2$day<-yday(lar2.d2$date)
lar2.lav<-lavielle(lar2.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(lar2.lav)
findpath(lar2.lav, 3)
#This one is hard because it's still making its way back to the natal territory and doesn't get there until
# the second week of May.  That's giving it huge dist2centroid values for that time.
# Because of this I'm going to cut out first 479 location

set<-lar2.d2[c(480:2230, 2400:4683),]  #cut out 1:430, stuck around southern MN
s1<-max(set$dist2centroid)  # 121,455
s2<-sd(set$dist2centroid)   # 14,064
s3<-mean(set$dist2centroid)  # 41,763
s4<-median(set$dist2centroid) # 43,196
s5<-IQR(set$dist2centroid) # 12,111
dat[17,]<-c("6M (Larson colt #2)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-lar2.d2[2231:2399,]  #cut out 430:479, because not done with migration
r1<-max(roam$dist2centroid)   # 78,368
r2<-sd(roam$dist2centroid)    # 17,538
r3<-mean(roam$dist2centroid)  # 65,225
r4<-median(roam$dist2centroid) # 73,717
r5<-IQR(roam$dist2centroid) # 3,373
dat[18,]<-c("6M (Larson colt #2)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

l2.df<-lar2.d2
l2.df$mode<-NA
l2.df[c(2231:2399),"mode"]<-"roam"
l2.df[c(480:2230, 2400:4683),"mode"]<-"settled"
l2.df$id<-"6M (Larson colt #2)"
res<-rbind(res, l2.df)


###############################################################
#nora
nora<-lxy.subset(df.lxy, id="7E (Nora Township colt)")
nora.d2<-d2ctr(nora)
nora.d2$day<-yday(nora.d2$date)
nora.lav<-lavielle(nora.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(nora.lav)
findpath(nora.lav, 3)
nora.d2$date[c(1,211, 7452)]  
# Crossed -93 on April 22.
#Roaming from April 22 till April 26
# Settled from April 26-Aug 31
#  I'm going to not count the period of migrating back to the natal territory
# since it clearly beelined back and stuck around the area

set<-nora.d2[c(212:7452),]
s1<-max(set$dist2centroid)  #59,125
s2<-sd(set$dist2centroid)   #5,441
s3<-mean(set$dist2centroid)  #3,762
s4<-median(set$dist2centroid) # 2,325
s5<-IQR(set$dist2centroid) # 2,117
dat[19,]<-c("7E (Nora Township colt)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)



n.df<-nora.d2
n.df$mode<-"settled"
n.df$id<-"7E (Nora Township colt)"
res<-rbind(res, n.df)


###############################################################
#melby1
melby<-lxy.subset(df.lxy, id="7J (Melby colt #1)")
melby.d2<-d2ctr(melby)
melby.d2$day<-yday(melby.d2$date)
melby.lav<-lavielle(melby.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(melby.lav)
findpath(melby.lav, 6)
# melby.d2$date[c(1,1262)]
# There are a lot of locations in southern MN, but it's wandering, not beelining, so it stays.

set<-melby.d2[c(251:1249, 1351:3824, 3876:4599, 4850:6875),]
s1<-max(set$dist2centroid)  #322,264
s2<-sd(set$dist2centroid)   #75,328
s3<-mean(set$dist2centroid)  #83,514
s4<-median(set$dist2centroid) # 83,124
s5<-IQR(set$dist2centroid) # 60,126
dat[20,]<-c("7J (Melby colt #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-melby.d2[c(1:250, 1250:1350, 3825:3875, 4600:4850),]
r1<-max(roam$dist2centroid)   # 313,895
r2<-sd(roam$dist2centroid)    # 75,150
r3<-mean(roam$dist2centroid)  # 139,718
r4<-median(roam$dist2centroid) # 129,582
r5<-IQR(roam$dist2centroid) # 109,073
dat[21,]<-c("7J (Melby colt #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

m.df<-melby.d2
m.df$mode<-NA
m.df[c(1:250, 1250:1350, 3825:3875, 4600:4850),"mode"]<-"roam"
m.df[c(251:1249, 1351:3824, 3876:4599, 4850:6875),"mode"]<-"settled"
m.df$id<-"7J (Melby colt #1)"
res<-rbind(res, m.df)
# Crossed -93 on APril 17
# RElatively settled from April 17-May 19
# Relocated to a different settled area on May 19 till end of Aug
###############################################################################
# Melby #2
melby2<-lxy.subset(df.lxy, id="1K (Melby colt #2)")
melby2.d2<-d2ctr(melby2)
melby2.d2$day<-yday(melby2.d2$date)
melby2.lav<-lavielle(melby2.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(melby2.lav)
findpath(melby2.lav, 8)
# First 5 loc's removed because migrating back.

set<-melby2.d2[c(6:266, 301:1199, 1281:3287),]
s1<-max(set$dist2centroid)   # 90,826
s2<-sd(set$dist2centroid)    # 12,115
s3<-mean(set$dist2centroid)  # 42,577
s4<-median(set$dist2centroid) # 39,567
s5<-IQR(set$dist2centroid) # 27,616
dat[22,]<-c("1K (Melby colt #2)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

roam<-melby2.d2[c(267:300, 1200:1280),]
r1<-max(roam$dist2centroid)   # 112,895
r2<-sd(roam$dist2centroid)    # 27,265
r3<-mean(roam$dist2centroid)  # 36,770
r4<-median(roam$dist2centroid) # 31,834
r5<-IQR(roam$dist2centroid) # 56,659
dat[23,]<-c("1K (Melby colt #2)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

m2.df<-melby2.d2
m2.df$mode<-NA
m2.df[c( 267:300, 1200:1280),"mode"]<-"roam"
m2.df[c(6:266, 301:1199, 1281:3287),"mode"]<-"settled"
m2.df$id<-"1K (Melby colt #2)"
res<-rbind(res, m2.df)
#################################################################################

#Export data
write.csv(res, "output/comp.lavielle.colt.df.2016.csv")
write.csv(dat, "output/colt.centroid.df.2016.csv")

#Visualizations
str(dat)
for(i in 3:9){
  dat[,i]<-as.numeric(dat[,i])
}

#compute standard error for error bars
alldat<-mutate(alldat, se=sd/sqrt(n_obs))

ggplot(dat, aes(x=id, y=mean, group=set_roam, colour=set_roam))+
  geom_line()+geom_point()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), 
                                         position=position_dodge(0.05))
colt.df<-dat
colt.df$age<-"colt"
adult.df<-read.csv("Data/adult.centroid.df.csv")
adult.df$age<-"adult"
adult.df<-adult.df[,-1]
alldat<-rbind(colt.df, adult.df)
alldat$mean.km<-alldat$mean/1000
write.csv(alldat, "Data/all.dist.dat.csv")

ggplot(alldat, aes(x=id, y=mean, group=age, colour=age))+
  geom_line()+geom_point()+facet_wrap(~set_roam)

p.sd<-ggplot(alldat, aes(x=id, y=mean/1000, colour=set_roam))+
  geom_point()+geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000))+ 
              facet_wrap(~age)+
              theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
                    axis.ticks.x = element_blank())+
  labs(title="Mean Distance from Centroid April 1-Sept 1, 2016 (error bars 1 std dev)",
       y="Mean Distance from Centroid (in km)", color="Movement Phase")+
  theme(plot.title=element_text(hjust=0.5))
              
p.se<-ggplot(alldat, aes(x=id, y=mean/1000, colour=set_roam))+
  geom_point()+geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000, 
                                        ))+facet_wrap(~age)+
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.x = element_blank())+
  labs(title="Mean Distance from Centroid April 1-Sept 1, 2016 (error bars 1 std error)",
      y="Distance from Centroid (in km)", color="Movement Phase")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(alldat, aes(x=id, y=max/1000, colour=set_roam))+
  geom_point(position=position_dodge(0.7),size=2.5)+facet_wrap(~age)+
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.ticks.x = element_blank())+
  labs(title="Maximum Distance from Centroid April 1-Sept 1, 2016",
       y="Distance from Centroid (in km)", color="Movement Phase")+
     theme(plot.title=element_text(hjust=0.5))


