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
summary(df$location.lat)
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

#Drop colts
df<-df[df$age=="adult",]

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
                                  +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs " ))
table(df$id)
# lxy.plot.pt2ctr(df.lxy)
##########################################
deg3<-lxy.subset(df.lxy, id="5J (Deglman #3)")
plot(deg3)
deg3.d2<-d2ctr(deg3)
#I think 5J Deglman 3 should be excluded because there is only a couple weeks of data.
##############################################
lind<-lxy.subset(df.lxy, id="6E (Linden-Kologi)")
plot(lind)
lind.d2<-d2ctr(lind)
# Linden-Kologi also is missing most of the locations from the time period, although 
# there are small chunks from time to time
####################################################################
wam<-lxy.subset(df.lxy, id="3E (Wambach adult)")
plot(wam)
wam.d2<-d2ctr(wam)
#This should be cut out as well.  There is only info from the first few weeks of April.
######################################################
ras<-lxy.subset(df.lxy, id="3K (Rasmussen adult)")
plot(ras)
ras.d2<-d2ctr(ras)
#This one should be cut off as well?  It basically doesn't have any 
# coverage from most of the summer
#####################################################################
stock<-lxy.subset(df.lxy, id="8M (Stockyard adult)")
plot(stock)
stock.d2<-d2ctr(stock)
summary(stock)
lxy.plot.pt2ctr(stock)
lxy.plot.freq(stock, deltat.by.date = T)
lxy.plot.freq(stock, cp= T)
lxy.plot.freq(stock, samp.by.date = T)
# It's probably worth including 8M for now, but it's missing a 
# chunk of data from late May till late June.
##################################################################

# So I'll be excluding Wambach, Rasmussen, and Linden-Kologi. (as well as Deglman #3)
# For a number of cranes there is a really high dist2centroid for the beginning of April,
# but I'll look at the trajectory and if it's basically a straight line towards the 
# territory, I'll consider it migration and not roaming, and try to make a note of it.

###########################################################################
#dataframe to store comprehensive results
res<-data.frame()
dat<-data.frame(id=NA, set_roam=NA, n_days=NA, n_obs=NA, max=NA, sd=NA, mean=NA, median=NA, IQR=NA)
###############################################################################
#Deglman adult #1
deg1<-lxy.subset(df.lxy, id="0K Deglman #1")
deg1.d2<-d2ctr(deg1)
plot(deg1)
deg1.d2$day<-yday(deg1.d2$date)
# deg1.lav<-lavielle(deg1.d2$dist2centroid, Lmin=5, Kmax=10)
deg1.d2$date[c(1,5188)]
#There on April 1.  Stayed on territory until Aug 30

# settled
set<-deg1.d2[c(1:5188),]
s1<-max(set$dist2centroid)  #3278
s2<-sd(set$dist2centroid)   #408
s3<-mean(set$dist2centroid) #528
s4<-median(set$dist2centroid) #500
s5<-IQR(set$dist2centroid) #526
dat[1,]<-c("0K Deglman #1", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4, s5)

d.df<-deg1.d2
d.df$mode<-"settled"
d.df$id<-"0K Deglman #1"
res<-rbind(res, d.df)
#############################################################################
#Santiago #3
san3<-lxy.subset(df.lxy, id="1J (Santiago #3)")
san3.d2<-d2ctr(san3)
plot(san3)
san3.d2$day<-yday(san3.d2$date)
san3.lav<-lavielle(san3.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(san3.lav)
findpath(san3.lav, 5)
san3.d2$date[c(1,1000,6723)]
# Like Deglman #2, this crane seems to be very active and not as settled as other adults,
# but it didn't have big long-distance movements, and limited itself to one overall area.
# I'm going to count the first three weeks as roaming, and if you look at a map, it's kinda 
# all over the place.  I'm going to hack off the end, because I think it started migrating
# to a staging area.

# settled
set<-san3.d2[c(1000:5970),]
s1<-max(set$dist2centroid)  #16,251
s2<-sd(set$dist2centroid)   #2,879
s3<-mean(set$dist2centroid)#2,883
s4<-median(set$dist2centroid) # 1315
s5<-IQR(set$dist2centroid) #3247
dat[2,]<-c("1J (Santiago #3)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3, s4,s5)

#roam
roam<-san3.d2[c(1:999),]
r1<-max(roam$dist2centroid)   #73,304
r2<-sd(roam$dist2centroid)    #15,635
r3<-mean(roam$dist2centroid)   #9,475
r4<-median(roam$dist2centroid) # 3152
r5<-IQR(roam$dist2centroid) #7706
dat[3,]<-c("1J (Santiago #3)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

s.df<-san3.d2[1:5970,]
s.df$mode<-NA
s.df[1:999,"mode"]<-"roam"
s.df[1000:5970,"mode"]<-"settled"
s.df$id<-"1J (Santiago #3)"
res<-rbind(res, s.df)
###################################################################################
#2M (Bagley)
bag<-lxy.subset(df.lxy, id="2M (Bagley)")
bag.d2<-d2ctr(bag)
bag.d2$day<-yday(bag.d2$date)
plot(bag)
lxy.plot.freq(bag, deltat.by.date = T)
lxy.plot.freq(bag, cp= T)
lxy.plot.freq(stock, samp.by.date = T) # a month missing mid-May to mid-July

bag.lav<-lavielle(bag.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(bag.lav)
findpath(bag.lav, 5)
bag.d2$date[c(1,3679,3678)]
#There on April 1.  Stayed on territory until August 31, when it seemed to start migration.
# Instead of counting the last part as roaming, I"m simply going to chop it off, since it's 
# less than a day.

# settled
set<-bag.d2[c(1:3660),]
s1<-max(set$dist2centroid)  #2,485
s2<-sd(set$dist2centroid)   #515
s3<-mean(set$dist2centroid) #693
s4<-median(set$dist2centroid) # 407
s5<-IQR(set$dist2centroid) #526
dat[4,]<-c("2M (Bagley)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

b.df<-bag.d2[c(1:3660),]
b.df$mode<-"settled"
b.df$id<-"2M (Bagley)"
b.df$day<-yday(b.df$date)
res<-rbind(res, b.df)
###############################################################################
# 4J (Santiago #1)
san1<-lxy.subset(df.lxy, id="4J (Santiago #1)")
san1.d2<-d2ctr(san1)
plot(san1)
san1.d2$day<-yday(san1.d2$date)
san1.lav<-lavielle(san1.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(san1.lav)
findpath(san1.lav, 3)
san1.d2$date[c(1,1000,6723)]
#There on April 1.  Roaming during April, then stayed setteld till middle of July,
# Roaming from mid July-end of August.  The roaming period wasn't the "wandering"
# of typical colts, it just relocated a few counties over to spend most of May
# at Wrightstown WMA (large WMA SE of 4 corners) where there was likely a huge 
# flock of non-breeding cranes.  (Probably a "young adult" without a territory)

# settled
set<-san1.d2[c(1:1740, 2550:7534),]
s1<-max(set$dist2centroid)  #12,510
s2<-sd(set$dist2centroid)   #1,981
s3<-mean(set$dist2centroid)#6,474
s4<-median(set$dist2centroid) # 6494
s5<-IQR(set$dist2centroid) #3477
dat[5,]<-c("4J (Santiago #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

#roam
roam<-san1.d2[c(1741:2549),]
r1<-max(roam$dist2centroid)   #48,773
r2<-sd(roam$dist2centroid)    #7,704
r3<-mean(roam$dist2centroid)   #46,459
r4<-median(roam$dist2centroid) # 47,972
r5<-IQR(roam$dist2centroid) #1045               #this shows that the period spent away was an outlier
dat[6,]<-c("4J (Santiago #1)", "roam", length(unique(roam$day)),nrow(roam), r1, r2, r3, r4, r5)

s.df<-san1.d2
s.df$mode<-NA
s.df[c(1741:2549),"mode"]<-"roam"
s.df[c(1:1740, 2550:7534),"mode"]<-"settled"
s.df$id<-"4J (Santiago #1)"
res<-rbind(res, s.df)
#######################################################################################
# 4K (Waubun #1)
waub1<-lxy.subset(df.lxy, id="4K (Waubun #1)")
waub1.d2<-d2ctr(waub1)
plot(waub1)
waub1.d2$day<-yday(waub1.d2$date)
waub1.lav<-lavielle(waub1.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(waub1.lav)
findpath(waub1.lav, 2)
waub1.d2$date[c(1,35,5588)]
#Arrived on April 1st (not before).  Settled all summer.  There was a huge movement in the 
# first day as the crane was still migrating there.  I'm going to chop that off.

# settled
set<-waub1.d2[c(50:5588),]
s1<-max(set$dist2centroid)  #20,266
s2<-sd(set$dist2centroid)   #843
s3<-mean(set$dist2centroid) #2,113
s4<-median(set$dist2centroid) # 1984
s5<-IQR(set$dist2centroid) #444
dat[7,]<-c("4K (Waubun #1)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)


w.df<-waub1.d2[c(50:5588),]
w.df$mode<-"settled"
w.df$id<-"4K (Waubun #1)"
res<-rbind(res, w.df)
###################################################################################
#4M (Ogema adult)
ogema<-lxy.subset(df.lxy, id="4M (Ogema adult)")
ogema.d2<-d2ctr(ogema)
ogema.d2$day<-yday(ogema.d2$date)
plot(ogema)
lxy.plot.freq(ogema, samp.by.date = T) 

# ogema.lav<-lavielle(ogema.d2$dist2centroid, Lmin=5, Kmax=10)
# chooseseg(ogema.lav)
# findpath(ogema.lav, 5)
ogema.d2$date[c(1,7838)]
#This crane pretty obviously was settled all season.

# settled
set<-ogema.d2[c(1:7838),]
s1<-max(set$dist2centroid)  #4,275
s2<-sd(set$dist2centroid)   #605
s3<-mean(set$dist2centroid) #1,021
s4<-median(set$dist2centroid) # 816
s5<-IQR(set$dist2centroid) #854
dat[8,]<-c("4M (Ogema adult)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

o.df<-ogema.d2
o.df$mode<-"settled"
o.df$id<-"4M (Ogema adult)"
res<-rbind(res, o.df)
###################################################################################
#5K (Helliksen July adult 2015)
hel<-lxy.subset(df.lxy, id="5K (Helliksen July adult 2015)")
hel.d2<-d2ctr(hel)
hel.d2$day<-yday(hel.d2$date)
plot(hel)
lxy.plot.freq(hel, samp.by.date = T) 

# hel.lav<-lavielle(hel.d2$dist2centroid, Lmin=5, Kmax=10)
# chooseseg(hel.lav)
# findpath(hel.lav, 5)
hel.d2$date[c(1,7838)]
#Poster child of being settled all year.

# settled
set<-hel.d2[c(1:6043),]
s1<-max(set$dist2centroid)  #2,241
s2<-sd(set$dist2centroid)   #303
s3<-mean(set$dist2centroid) #421
s4<-median(set$dist2centroid) # 369
s5<-IQR(set$dist2centroid) #354
dat[9,]<-c("5K (Helliksen July adult 2015)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

h.df<-hel.d2
h.df$mode<-"settled"
h.df$id<-"5K (Helliksen July adult 2015)"
res<-rbind(res, h.df)
###################################################################################
#8K (Deglman #2)
deg2<-lxy.subset(df.lxy, id="8K (Deglman #2)")
deg2.d2<-d2ctr(deg2)
deg2.d2$day<-yday(deg2.d2$date)
plot(deg2)
lxy.plot.freq(deg2, samp.by.date = T) 

deg2.lav<-lavielle(deg2.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(deg2.lav)
findpath(deg2.lav, 5)
deg2.d2$date[c(1,1000,4300,5736)]
# This crane was more on the "active" end for an adult, and didn't seem to be super settled to a 
# territory, but it didn't have big long-term roaming movmements, so settled all season.

# settled
set<-deg2.d2
s1<-max(set$dist2centroid)  #16,763
s2<-sd(set$dist2centroid)   #3004
s3<-mean(set$dist2centroid) #6322
s4<-median(set$dist2centroid) # 5297
s5<-IQR(set$dist2centroid) #3414
dat[10,]<-c("8K (Deglman #2)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)


d.df<-deg2.d2
d.df$mode<-"settled"
d.df$id<-"8K (Deglman #2)"
res<-rbind(res, d.df)
###################################################################################
# 8M (Stockyard adult)
stock<-lxy.subset(df.lxy, id="8M (Stockyard adult)")
stock.d2<-d2ctr(stock)
stock.d2$day<-yday(stock.d2$date)
plot(stock)
lxy.plot.freq(stock, samp.by.date = T) 

stock.lav<-lavielle(stock.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(stock.lav)
findpath(stock.lav, 3)
stock.d2$date[c(1,80,3102)]
# This crane was still migrating and didn't get to its territory until
# April 6th.  It is also missing data for after July 23.  I'm going to call
# it all settled and cut off the first week.

# settled
set<-stock.d2[c(80:3102),]
s1<-max(set$dist2centroid)  #9,193
s2<-sd(set$dist2centroid)   #835
s3<-mean(set$dist2centroid) #7668
s4<-median(set$dist2centroid) # 7893
s5<-IQR(set$dist2centroid) #1053
dat[11,]<-c("8M (Stockyard adult)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

s.df<-stock.d2[c(80:3102),]
s.df$mode<-"settled"
s.df$id<-"8M (Stockyard adult)"
res<-rbind(res, s.df)
#########################################################################
# 9K (Santiago #2)
san2<-lxy.subset(df.lxy, id="9K (Santiago #2)")
san2.d2<-d2ctr(san2)
san2.d2$day<-yday(san2.d2$date)
plot(san2)
lxy.plot.freq(san2, samp.by.date = T) 

san2.lav<-lavielle(san2.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(san2.lav)
findpath(san2.lav, 3)
san2.d2$date[c(1,80,8161)]
# So the crane was still migrating the first three days.  I'll cut those out
# as I did with the others.  Settled the whole time.

# settled
set<-san2.d2[c(80:8106),]
s1<-max(set$dist2centroid, na.rm=T)  #2,921
s2<-sd(set$dist2centroid, na.rm=T)   #268
s3<-mean(set$dist2centroid, na.rm=T) #337
s4<-median(set$dist2centroid) # 252
s5<-IQR(set$dist2centroid) #249
dat[12,]<-c("9K (Santiago #2)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

s.df<-san2.d2[c(80:8106),]
s.df$mode<-"settled"
s.df$id<-"9K (Santiago #2)"
res<-rbind(res, s.df)
################################################################################
# 9M (Inman adult)
inman<-lxy.subset(df.lxy, id="9M (Inman adult)")
inman.d2<-d2ctr(inman)
inman.d2$day<-yday(inman.d2$date)
plot(inman)
lxy.plot.freq(inman, samp.by.date = T) 

inman.lav<-lavielle(inman.d2$dist2centroid, Lmin=5, Kmax=10)
chooseseg(inman.lav)
findpath(inman.lav, 6)
# THings were very stable except one day with a huge movement.  After
# looking at it more closely, that is just one location that is an outlier mistake.
# I'll remove it.

inman.d2<-inman.d2[-7656,]
# Now the rest are obviously all settled.


# settled
set<-inman.d2
s1<-max(set$dist2centroid)  #2,898
s2<-sd(set$dist2centroid)   #228
s3<-mean(set$dist2centroid) #317
s4<-median(set$dist2centroid) # 261
s5<-IQR(set$dist2centroid) #167
dat[13,]<-c("9M (Inman adult)", "settled", length(unique(set$day)), nrow(set), s1, s2, s3,s4,s5)

i.df<-inman.d2
i.df$mode<-"settled"
i.df$id<-"9M (Inman adult)"
res<-rbind(res, i.df)
##############################################################################################
write.csv(res, "output/comp.lavielle.adult.2017.df.csv")
write.csv(dat, "output/comp.adult.centroid.summaries.2017.df.csv")












