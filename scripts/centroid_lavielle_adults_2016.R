###############################################################################
#Segmentation by use of dist2centroid function
# Adults 2016
library(tlocoh)
library(adehabitatLT)
library(lubridate)
rm(list=ls())
source("Scripts/distance_to_centroid_function.R")
df<-read.csv("~/R/CraneMovement/scripts/t-locoh/Data/df16.csv") #takes a little while to load
df<-df[df$Month>3&df$Month<9,]

full.df<-df
#Drop colts
df<-df[df$age=="adult",]

df<-df[!df$id=="3C (Larson adult #2)",]
df<-df[!df$id=="3E (Wambach adult)",]
df<-df[!df$id=="5M (Beaulieu adult)",]
df<-df[!df$id=="9C (Helliksen adult April 2015)",]
df<-droplevels(df)
ids<-unique(df$id)
table(df$id)

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


# lxy.plot.pt2ctr(df.lxy)  # have to maximize plot window first to display

#dataframe to store comprehensive results
res<-data.frame()


dat<-data.frame(id=NA, set_roam=NA, n_days=NA, n_obs=NA, max=NA, sd=NA, mean=NA, median=NA, IQR=NA)
############################################################################
#Larson adult #1
larson<-lxy.subset(df.lxy, id="4E Larson adult #1")
larson.d2<-d2ctr(larson)
larson.d2$day<-yday(larson.d2$date)
larson.lav<-lavielle(larson.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(larson.lav)
findpath(larson.lav, 2)
larson.d2$date[c(1,6723)]
#There on April 1.  Stayed on territory until Aug 28

# settled
set<-larson.d2[c(1:6722),]
s1<-max(set$dist2centroid)  #6706
s2<-sd(set$dist2centroid)   #563
s3<-mean(set$dist2centroid)#1404
s4<-median(set$dist2centroid) # 1457
s5<-IQR(set$dist2centroid) # 689
dat[1,]<-c("4E Larson adult #1", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

#roam
roam<-larson.d2[c(6723:6910),]
r1<-max(roam$dist2centroid)   #49,372
r2<-sd(roam$dist2centroid)    #1,968
r3<-mean(roam$dist2centroid)   #47,473
r4<-median(roam$dist2centroid) # 47,710
r5<-IQR(roam$dist2centroid) #1,811
dat[2,]<-c("4E Larson adult #1", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

l.df<-larson.d2
l.df$mode<-NA
l.df[6723:6910,"mode"]<-"roam"
l.df[1:6722,"mode"]<-"settled"
l.df$id<-"4E Larson adult #1"
res<-rbind(res, l.df)
############################################################################
#Helliksen July 2015
helliksen<-lxy.subset(df.lxy, id="5K (Helliksen July adult 2015)")
helliksen.d2<-d2ctr(helliksen)
helliksen.d2$day<-yday(helliksen.d2$date)
helliksen.lav<-lavielle(helliksen.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(helliksen.lav)
findpath(helliksen.lav, 2)
# Upon looking at the plot of locations for this crane, you can tell 
# that it's settled the whole time

#Settled
set<-helliksen.d2[c(1:7554),]
s1<-max(set$dist2centroid)   # 3924
s2<-sd(set$dist2centroid)    # 354
s3<-mean(set$dist2centroid)  # 574
s4<-median(set$dist2centroid) # 493
s5<-IQR(set$dist2centroid) # 277
dat[3,]<-c("5K (Helliksen July adult 2015)", "settled",length(unique(set$day)),nrow(set),s1, s2, s3, s4, s5)

h.df<-helliksen.d2
h.df$mode<-NA
h.df[1:7554,"mode"]<-"settled"
h.df$id<-"5K (Helliksen July adult 2015)"
res<-rbind(res, h.df)

############################################################################
# Deglman #3      !!!Picked by eyeball instead !!!!!!
deg3<-lxy.subset(df.lxy, id="5J (Deglman #3)")
deg3.d2<-d2ctr(deg3)
deg3.d2$day<-yday(deg3.d2$date)
deg3.lav<-lavielle(deg3.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(deg3.lav)  #either 2 or 3 acceptable
findpath(deg3.lav, 6)
deg3.d2$date[c(1,5800)]
#There on April 1.  Stayed on territory until Aug 12
# After August 12 showed behavior typical of fall staging(although it had started even before that)

#Max distance from centroid during settled
set<-deg3.d2[c(1:5800),]
s1<-max(set$dist2centroid)   # 7973   #!! This is an example of lavielle not picking the beginning of the fall staging  !!
s2<-sd(set$dist2centroid)    # 429
s3<-mean(set$dist2centroid)  # 578
s4<-median(set$dist2centroid) # 580
s5<-IQR(set$dist2centroid) # 132
dat[4,]<-c("5J (Deglman #3)", "settled",length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

#Roaming
roam<-deg3.d2[c(5801:7004),]
r1<-max(roam$dist2centroid)   # 16,420
r2<-sd(roam$dist2centroid)    # 4,357
r3<-mean(roam$dist2centroid)  # 4,027
r4<-median(roam$dist2centroid) # 1379
r5<-IQR(roam$dist2centroid) #7029
dat[5,]<-c("5J (Deglman #3)", "roam",length(unique(roam$day)), nrow(roam), r1, r2, r3, r4, r5)

d.df<-deg3.d2
d.df$mode<-NA
d.df[5801:7004,"mode"]<-"roam"
d.df[1:5800,"mode"]<-"settled"
d.df$id<-"5J (Deglman #3)"
res<-rbind(res, d.df)

##################################################################################################
# Santiago #2      !!!  Two settled periods with a roaming period in between.   !!!!!!!!!!!!!
sant2<-lxy.subset(df.lxy, id="9K (Santiago #2)")
sant2.d2<-d2ctr(sant2)
sant2.d2$day<-yday(sant2.d2$date)
sant2.lav<-lavielle(sant2.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(sant2.lav)  #doesn't quite work so well, I have to pick 6 to get the breaks I want
findpath(sant2.lav, 6)
sant2.d2$date[c(1,8400)]
# Upon checking the trajectory plot, you can see that 
# this crane was settled the whole time

#Settled
set<-sant2.d2[c(1:8400),]
s1<-max(set$dist2centroid)   #6,616
s2<-sd(set$dist2centroid)    #671
s3<-mean(set$dist2centroid)  #708
s4<-median(set$dist2centroid) # 536
s5<-IQR(set$dist2centroid) #298
dat[6,]<-c("9K (Santiago #2)","settled",length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

s.df<-sant2.d2
s.df$mode<-NA
s.df[c(1:8400),"mode"]<-"settled"
s.df$id<-"9K (Santiago #2)"
res<-rbind(res, s.df)

##################################################################################################
# Waubun #2
waub2<-lxy.subset(df.lxy, id="0J (Waubun adult #2)")
waub2.d2<-d2ctr(waub2)
waub2.d2$day<-yday(waub2.d2$date)
waub2.lav<-lavielle(waub2.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(waub2.lav)  #the whole season is stable and settled
findpath(waub2.lav, 2)
waub2.d2$date[c(1,7012)]
# Upon looking at the trajectory, the whole season is settled

#Settled
set<-waub2.d2[c(1:7012),]
s1<-max(set$dist2centroid)   #2314
s2<-sd(set$dist2centroid)    #224
s3<-mean(set$dist2centroid)   #322
s4<-median(set$dist2centroid) # 252
s5<-IQR(set$dist2centroid) #212
dat[7,]<-c("0J (Waubun adult #2)", "settled",length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

w.df<-waub2.d2
w.df$mode<-NA
w.df[1:7012,"mode"]<-"settled"
w.df$id<-"0J (Waubun adult #2)"
res<-rbind(res, w.df)
##################################################################################################
# Rasmussen
ras<-lxy.subset(df.lxy, id="3K (Rasmussen adult)" )
ras.d2<-d2ctr(ras)
ras.d2$day<-yday(ras.d2$date)
ras.lav<-lavielle(ras.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(ras.lav)  #the whole season is stable and settled
findpath(ras.lav, 3)
ras.d2$date[c(1, 4172)]
# Upon looking at the trajectory, all settled

#Settled
set<-ras.d2[c(1:4172),]
s1<-max(set$dist2centroid)   # 7999
s2<-sd(set$dist2centroid)    # 557
s3<-mean(set$dist2centroid)  # 883
s4<-median(set$dist2centroid) # 757
s5<-IQR(set$dist2centroid) # 759
dat[8,]<-c("3K (Rasmussen adult)", "settled",length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

r.df<-ras.d2
r.df$mode<-"settled"
r.df$id<-"3K (Rasmussen adult)"
res<-rbind(res, r.df)

##################################################################################################
# Waubun #1    !!! choosing only a few segments doesn't work better than eyeballing !!!!!
waub1<-lxy.subset(df.lxy, id="4K (Waubun #1)")
waub1.d2<-d2ctr(waub1)
waub1.d2$day<-yday(waub1.d2$date)
waub1.lav<-lavielle(waub1.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(waub1.lav)  #only a small period of roaming
findpath(waub1.lav, 5)
waub1.d2$date[c(1,108, 5122, 5328,7564)]
#Crossed -93 on April 4.  Roamed(more accurately was still getting to territory) from APril 4-July 16.
# Roaming from July 16-JUly 20.  Settled from July 20-Aug 31 (although with an elevated period for most of August)
#  ***I'm going to exclude 1:107 as spring migration but keep in 5123:5327 as roaming**

#Settled
set<-waub1.d2[c(108:5122, 5328:7564),]
s1<-max(set$dist2centroid)   #4510
s2<-sd(set$dist2centroid)    #701
s3<-mean(set$dist2centroid)   #599
s4<-median(set$dist2centroid) # 300
s5<-IQR(set$dist2centroid) # 454
dat[9,]<-c("4K (Waubun #1)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)


#Roaming
roam<-waub1.d2[5123:5327,]
r1<-max(roam$dist2centroid)   #10,420
r2<-sd(roam$dist2centroid)     #3,351
r3<-mean(roam$dist2centroid)   # 5,296
r4<-median(roam$dist2centroid) # 6,429
r5<-IQR(roam$dist2centroid) # 6,777
dat[10,]<-c("4K (Waubun #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

w.df<-waub1.d2
w.df$mode<-NA
w.df[c(5123:5327),"mode"]<-"roam"
w.df[c(108:5122, 5328:7564),"mode"]<-"settled"
w.df$id<-"4K (Waubun #1)"
res<-rbind(res, w.df)

##############################################################################################
# Inman
inman<-lxy.subset(df.lxy, id="9M (Inman adult)")
inman.d2<-d2ctr(inman)
inman.d2$day<-yday(inman.d2$date)
# Upon looking at trajectory, it's settled the whole season

#Settled
set<-inman.d2[c(1:8738),]
s1<-max(set$dist2centroid)   # 2,751
s2<-sd(set$dist2centroid)    # 180
s3<-mean(set$dist2centroid)  # 320
s4<-median(set$dist2centroid) #  307
s5<-IQR(set$dist2centroid) # 236
dat[11,]<-c("9M (Inman adult)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

i.df<-inman.d2
i.df$mode<-"settled"
i.df$id<-"9M (Inman adult)"
res<-rbind(res, i.df)
##################################################################################################
# Stockyard
stock<-lxy.subset(df.lxy, id="8M (Stockyard adult)")
stock.d2<-d2ctr(stock)
stock.d2$day<-yday(stock.d2$date)
# Settled all season

#Settled
set<-stock.d2[c(1:5005),]
s1<-max(set$dist2centroid)   # 1,990
s2<-sd(set$dist2centroid)    # 327
s3<-mean(set$dist2centroid)  # 967
s4<-median(set$dist2centroid) # 961
s5<-IQR(set$dist2centroid) # 368
dat[12,]<-c("8M (Stockyard adult)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

s.df<-stock.d2
s.df$mode<-"settled"
s.df$id<-"8M (Stockyard adult)"
res<-rbind(res, s.df)
################################################################################################
# Santiago #1   !! This one doesn't really work  !!
# The dist2cent values are inflated on this crane because it staged for a 
# week at a place kinda far away, so the centroid is in between the two areas
san1<-lxy.subset(df.lxy, id="4J (Santiago #1)")
san1.d2<-d2ctr(san1)
san1.d2$day<-yday(san1.d2$date)
san1.lav<-lavielle(san1.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(san1.lav)  
findpath(san1.lav, 3)
san1.d2$date[c(1,4285, 4670,7944)]
#Crossed -93 on April 4.  A one week roaming phase from June 20-27

#Settled
set<-san1.d2[c(1:4285, 4670:7944),]
s1<-max(set$dist2centroid)   # 56,316
s2<-sd(set$dist2centroid)    # 5,481
s3<-mean(set$dist2centroid)  # 47,210
s4<-median(set$dist2centroid) # 44,130
s5<-IQR(set$dist2centroid) # 10,071
dat[13,]<-c("4J (Santiago #1)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)


#Roaming
roam<-san1.d2[4286:4669,]
r1<-max(roam$dist2centroid)    # 51,401
r2<-sd(roam$dist2centroid)     # 4,037
r3<-mean(roam$dist2centroid)   # 30,064
r4<-median(roam$dist2centroid) # 30,251
r5<-IQR(roam$dist2centroid) # 81
dat[14,]<-c("4J (Santiago #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

s.df<-san1.d2
s.df$mode<-NA
s.df[4286:4669,"mode"]<-"roam"
s.df[c(1:4285, 4670:7944),"mode"]<-"settled"
s.df$id<-"4J (Santiago #1)"
res<-rbind(res, s.df)

################################################################################################
# Bagley
bag<-lxy.subset(df.lxy, id="2M (Bagley)")
bag.d2<-d2ctr(bag)
bag.d2$day<-yday(bag.d2$date)
bag.lav<-lavielle(bag.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(bag.lav)  
findpath(bag.lav, 2)
bag.d2$date[c(1,6361,6888)]
#On territory April 1.  Went to different (staging) area on Aug 20

#Settled
set<-bag.d2[1:6350,]
s1<-max(set$dist2centroid)   # 8,066
s2<-sd(set$dist2centroid)    # 1,164
s3<-mean(set$dist2centroid)  # 2,734
s4<-median(set$dist2centroid) # 3,091
s5<-IQR(set$dist2centroid) # 2,040
dat[15,]<-c("2M (Bagley)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)


#Roaming
roam<-bag.d2[6351:6888,]
r1<-max(roam$dist2centroid)    # 36,911
r2<-sd(roam$dist2centroid)     # 4,149
r3<-mean(roam$dist2centroid)   # 31,522
r4<-median(roam$dist2centroid) # 32,546
r5<-IQR(roam$dist2centroid) # 1,478
dat[16,]<-c("2M (Bagley)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

b.df<-bag.d2
b.df$mode<-NA
b.df[6351:6888,"mode"]<-"roam"
b.df[1:6350,"mode"]<-"settled"
b.df$id<-"2M (Bagley)"
res<-rbind(res, b.df)

################################################################################################
# Deglman #2
deg2<-lxy.subset(df.lxy, id="8K (Deglman #2)")
deg2.d2<-d2ctr(deg2)
deg2.d2$day<-yday(deg2.d2$date)
# Upon looking at trajectory plot and dist2cent plot, settled the whole season

#Settled
set<-deg2.d2[1:4582,]
s1<-max(set$dist2centroid)   # 7,888
s2<-sd(set$dist2centroid)    # 1,293
s3<-mean(set$dist2centroid)  # 2,075
s4<-median(set$dist2centroid) # 1,593
s5<-IQR(set$dist2centroid) # 1,867
dat[17,]<-c("8K (Deglman #2)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

d.df<-deg2.d2
d.df$mode<-"settled"
d.df$id<-"8K (Deglman #2))"
res<-rbind(res, d.df)

################################################################################################
# Deglman #1
deg1<-lxy.subset(df.lxy, id="0K Deglman #1")
deg1.d2<-d2ctr(deg1)
deg1.d2$day<-yday(deg1.d2$date)
# Upon looking at traj and dist2cent, there is increased movement in august, but 
# not long-range roaming

#Settled
set<-deg1.d2 #3936
s1<-max(set$dist2centroid)   # 3,648
s2<-sd(set$dist2centroid)    # 557
s3<-mean(set$dist2centroid)  # 748
s4<-median(set$dist2centroid) # 628
s5<-IQR(set$dist2centroid) #557
dat[18,]<-c("0K Deglman #1", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

d.df<-deg1.d2
d.df$mode<-"settled"
d.df$id<-"0K Deglman #1"
res<-rbind(res, d.df)

################################################################################################
# Ogema  !! This is a hard one to segment, see dist2cent plot and trajectory  !!
ogema<-lxy.subset(df.lxy, id="4M (Ogema adult)")
ogema.d2<-d2ctr(ogema)
ogema.d2$day<-yday(ogema.d2$date)
# This one was kinda hard, but based on the max/sd/mean of if all were settled,
# I decided to call the whole season settled

#Settled4
set<-ogema.d2 #8530 locs
s1<-max(set$dist2centroid)   # 4,161
s2<-sd(set$dist2centroid)    # 619
s3<-mean(set$dist2centroid)  # 716
s4<-median(set$dist2centroid) #  466
s5<-IQR(set$dist2centroid) # 845
dat[19,]<-c("4M (Ogema adult)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)

o.df<-ogema.d2
o.df$mode<-"settled"
o.df$id<-"4M (Ogema adult)"
res<-rbind(res, o.df)

################################################################################################
# Santiago #3
san3<-lxy.subset(df.lxy, id="1J (Santiago #3)")
san3.d2<-d2ctr(san3)
san3.d2$day<-yday(san3.d2$date)
# Upon looking at traj plot and dist2cent, settled all season

#Settled
set<-san3.d2 #7629
s1<-max(set$dist2centroid)   # 6,736
s2<-sd(set$dist2centroid)    # 885
s3<-mean(set$dist2centroid)  # 925
s4<-median(set$dist2centroid) # 742
s5<-IQR(set$dist2centroid) # 1,230
dat[20,]<-c("1J (Santiago #3)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)


s.df<-san3.d2
s.df$mode<-"settled"
s.df$id<-"1J (Santiago #3)"
res<-rbind(res, s.df)

################################################################################################
# Linden-kologi
lind<-lxy.subset(df.lxy, id="6E (Linden-Kologi)")
lind.d2<-d2ctr(lind)
lind.d2$day<-yday(lind.d2$date)
lind.lav<-lavielle(lind.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(lind.lav)  
findpath(lind.lav, 2)
lind.d2$date[c(1,4600,4924)]
#Crossed -93 longitude April 10.  Settled until it sent to stage on Aug 25

#Settled
set<-lind.d2[1:4600,]
s1<-max(set$dist2centroid)   # 11,158
s2<-sd(set$dist2centroid)    # 937
s3<-mean(set$dist2centroid)  # 2,829
s4<-median(set$dist2centroid) # 2,857
s5<-IQR(set$dist2centroid) # 1,184
dat[21,]<-c("6E (Linden-Kologi)", "settled", length(unique(set$day)),nrow(set), s1, s2, s3, s4, s5)


#Roaming
roam<-lind.d2[4601:4924,]
r1<-max(roam$dist2centroid)    # 51,331
r2<-sd(roam$dist2centroid)     # 17,325
r3<-mean(roam$dist2centroid)   # 38,081
r4<-median(roam$dist2centroid) # 46,579
r5<-IQR(roam$dist2centroid) # 1,203
dat[22,]<-c("6E (Linden-Kologi)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

l.df<-lind.d2
l.df$mode<-NA
l.df[4601:4924,"mode"]<-"roam"
l.df[1:4600,"mode"]<-"settled"
l.df$id<-"6E (Linden-Kologi)"
res<-rbind(res, l.df)
##############################################################################################
write.csv(res, "output/comp.lavielle.adult.df.2016.csv")
write.csv(dat, "output/comp.adult.centroid.summaries.df.2016.csv")

for(i in 3:7){
  dat[,i]<-as.numeric(dat[,i])
}

ggplot(dat, aes(x=id, y=mean, group=set_roam, colour=set_roam))+
  geom_point()+geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), 
                                         position=position_dodge(0.05))+coord_flip()
