#Create tum df adults 2017
# Adults 2017
rm(list=ls())

library(tlocoh)
library(sp)
library(dplyr)
library(lubridate)

df<-read.csv("Data/all_cranes_2017_april1_thru_aug31.csv") #takes a little while to load

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

#Exclude some adults.  Already documented which ones and why in the script: 'centroid lavielle17'
df<-df[!df$id=="3E (Wambach adult)",]
df<-df[!df$id=="6E (Linden-Kologi)",]
df<-df[!df$id=="3K (Rasmussen adult)",]
df<-df[!df$id=="5J (Deglman #3)",]
df<-droplevels(df)

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
###########################################################################
# Finding s-values  (S-value on graph where ptsh=0.6)
ids<-unique(df$id)
svals<-NA

# 9M Inman
lxy.ptsh.add(df.lxy, id=ids[1]) #0.02
svals[1]<-0.02

# 9K Santiago #2
lxy.ptsh.add(df.lxy, id=ids[2])  #0.0013
svals[2]<-0.0013

# 8M Stockyard
lxy.ptsh.add(df.lxy, id=ids[3])  #0.002
svals[3]<-0.002

# 4J Santiago #1
lxy.ptsh.add(df.lxy, id=ids[4])  #0.002
svals[4]<-0.002

# 8K Deglman #2
lxy.ptsh.add(df.lxy, id=ids[5])  #0.004
svals[5]<-0.004

# 2M Bagley
lxy.ptsh.add(df.lxy, id=ids[6])  #0.002
svals[6]<-0.002

# 4M Ogema
lxy.ptsh.add(df.lxy, id=ids[7])  #0.006
svals[7]<-0.006

# 5K Helliksen July
lxy.ptsh.add(df.lxy, id=ids[8])  #0.01
svals[8]<-0.01

# 0K Deglman #1
lxy.ptsh.add(df.lxy, id=ids[9])  #0.004
svals[9]<-0.004

# 1J Santiago #3
lxy.ptsh.add(df.lxy, id=ids[10])  #0.00175
svals[10]<-0.00175

# 4K Waubun #1
lxy.ptsh.add(df.lxy, id=ids[11])  #0.00375
svals[11]<-0.00375
###########################################################
# Find auto-a values and create/export time-use metric dataframe

# 9M Inman
inman<-lxy.nn.add(df.lxy, id="9M (Inman adult)",
                  s=svals[1], a=auto.a(nnn=20, ptp=0.98))
# a=17900
inman<-lxy.lhs(inman, id="9M (Inman adult)",
               s=svals[1], a=17900, iso.add=T)
inman.tum<-lhs.visit.add(inman, ivg=3600*24)
lhs.exp.csv(inman.tum, csv.save = T, dir="Data/tum files/2017/Adults")

# 9K Santiago #2
san2<-lxy.nn.add(df.lxy, id="9K (Santiago #2)",
                  s=svals[2], a=auto.a(nnn=20, ptp=0.98))
# a=12,000
san2<-lxy.lhs(san2, id="9K (Santiago #2)",
               s=svals[2], a=12000, iso.add=T)
san2.tum<-lhs.visit.add(san2, ivg=3600*24)
lhs.exp.csv(san2.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 8M Stockyard
stock<-lxy.nn.add(df.lxy, id="8M (Stockyard adult)",
                 s=svals[3], a=auto.a(nnn=20, ptp=0.98))
# a=44,000
stock<-lxy.lhs(stock, id="8M (Stockyard adult)",
              s=svals[3], a=44000, iso.add=T)
stock.tum<-lhs.visit.add(stock, ivg=3600*24)
lhs.exp.csv(stock.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 4J (Santiago #1)
san1<-lxy.nn.add(df.lxy, id="4J (Santiago #1)",
                  s=svals[4], a=auto.a(nnn=20, ptp=0.98))
# a=52,500
san1<-lxy.lhs(san1, id="4J (Santiago #1)",
               s=svals[4], a=52500, iso.add=T)
san1.tum<-lhs.visit.add(san1, ivg=3600*24)
lhs.exp.csv(san1.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 8K (Deglman #2) 
deg2<-lxy.nn.add(df.lxy, id="8K (Deglman #2)",
                 s=svals[5], a=auto.a(nnn=20, ptp=0.98))
# a=43,500
deg2<-lxy.lhs(deg2, id="8K (Deglman #2)",
              s=svals[5], a=43500, iso.add=T)
deg2.tum<-lhs.visit.add(deg2, ivg=3600*24)
lhs.exp.csv(deg2.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 2M (Bagley) 
bag<-lxy.nn.add(df.lxy, id="2M (Bagley)",
                 s=svals[6], a=auto.a(nnn=20, ptp=0.98))
# a=16,000
bag<-lxy.lhs(bag, id="2M (Bagley)",
              s=svals[6], a=16000, iso.add=T)
bag.tum<-lhs.visit.add(bag, ivg=3600*24)
lhs.exp.csv(bag.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 4M (Ogema adult) 
ogema<-lxy.nn.add(df.lxy, id="4M (Ogema adult)",
                s=svals[7], a=auto.a(nnn=20, ptp=0.98))
# a=26,000
ogema<-lxy.lhs(ogema, id="4M (Ogema adult)",
             s=svals[7], a=26000, iso.add=T)
ogema.tum<-lhs.visit.add(ogema, ivg=3600*24)
lhs.exp.csv(ogema.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 5K Helliksen July
hel<-lxy.nn.add(df.lxy, id="5K (Helliksen July adult 2015)",
                  s=svals[8], a=auto.a(nnn=20, ptp=0.98))
# a=17,500
hel<-lxy.lhs(hel, id="5K (Helliksen July adult 2015)",
               s=svals[8], a=17500, iso.add=T)
hel.tum<-lhs.visit.add(hel, ivg=3600*24)
lhs.exp.csv(hel.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 0K Deglman #1
deg1<-lxy.nn.add(df.lxy, id="0K Deglman #1",
                s=svals[9], a=auto.a(nnn=20, ptp=0.98))
# a=16,500
deg1<-lxy.lhs(deg1, id="0K Deglman #1",
             s=svals[9], a=16500, iso.add=T)
deg1.tum<-lhs.visit.add(deg1, ivg=3600*24)
lhs.exp.csv(deg1.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 1J (Santiago #3) 
san3<-lxy.nn.add(df.lxy, id="1J (Santiago #3)",
                 s=svals[10], a=auto.a(nnn=20, ptp=0.98))
# a=75,500
san3<-lxy.lhs(san3, id="1J (Santiago #3)",
              s=svals[10], a=75500, iso.add=T)
san3.tum<-lhs.visit.add(san3, ivg=3600*24)
lhs.exp.csv(san3.tum, csv.save = T, dir="Data/tum files/2017/Adults")


# 4K (Waubun #1)
waub<-lxy.nn.add(df.lxy, id="4K (Waubun #1)",
                 s=svals[11], a=auto.a(nnn=20, ptp=0.98))
# a=42,500
waub<-lxy.lhs(waub, id="4K (Waubun #1)",
              s=svals[11], a=42500, iso.add=T)
waub.tum<-lhs.visit.add(waub, ivg=3600*24)
lhs.exp.csv(waub.tum, csv.save = T, dir="Data/tum files/2017/Adults")
