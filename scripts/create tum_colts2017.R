#Colts tum df 2017
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
df$gender<-NULLe
df<-mutate(df, gender=ifelse(id%in%male.list, "male", "female"))

#Drop adults
df<-df[df$age=="colt",]

#Drop cranes with not enough data; only 6 retained
df<-df[!df$id=="1K (Melby colt #2)",]
df<-df[!df$id=="2A (Helliksen July 2015 colt)",]
df<-df[!df$id=="3A  (Barb Wire)",]
df<-df[!df$id=="4A (Dower colt)",]
df<-df[!df$id=="7K (Larson colt #1)",]
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
############################################################################################
# Finding s-values (S-value on graph where ptsh=0.6)
ids<-unique(df$id)
svals<-NA
ids

# 7E Nora Township colt
lxy.ptsh.add(df.lxy, id=ids[1])
svals[1]<-0.03

# 0M N of Mud Lake
lxy.ptsh.add(df.lxy, id=ids[2])
svals[2]<-0.004

# 5A Staples
lxy.ptsh.add(df.lxy, id=ids[3])
svals[3]<-0.008

# 6A Pelican Rapids
lxy.ptsh.add(df.lxy, id=ids[4])
svals[4]<-0.005

# 5C Stockyard 
lxy.ptsh.add(df.lxy, id=ids[5])
svals[5]<-0.0015

# 7J Melby #1
lxy.ptsh.add(df.lxy, id=ids[6])
svals[6]<-0.002

###########################################################
# Find auto-a values and create/export time-use metric dataframe

# 7E Nora Township colt
nora<-lxy.nn.add(df.lxy, id="7E (Nora Township colt)",
                 s=svals[1], a=auto.a(nnn=20, ptp=0.98))
nora<-lxy.lhs(nora, id="7E (Nora Township colt)",
              s=svals[1], a=111000, iso.add = T)
nora.tum<-lhs.visit.add(nora, ivg=3600*24)
lhs.exp.csv(nora.tum, csv.save=T, dir="Data/tum files/2017/Colts")

# 0M (N of Mud Lake)
mud<-lxy.nn.add(df.lxy, id="0M (N of Mud Lake)",
                 s=svals[2], a=auto.a(nnn=20, ptp=0.98))
mud<-lxy.lhs(mud, id="0M (N of Mud Lake)",
              s=svals[2], a=2900, iso.add = T)
mud.tum<-lhs.visit.add(mud, ivg=3600*24)
lhs.exp.csv(mud.tum, csv.save=T, dir="Data/tum files/2017/Colts")

# 5A (W Staples colt #1)
staples<-lxy.nn.add(df.lxy, id="5A (W Staples colt #1)",
                 s=svals[3], a=auto.a(nnn=20, ptp=0.98))
staples<-lxy.lhs(staples, id="5A (W Staples colt #1)",
              s=svals[3], a=30000, iso.add = T)
staples.tum<-lhs.visit.add(staples, ivg=3600*24)
lhs.exp.csv(staples.tum, csv.save=T, dir="Data/tum files/2017/Colts")

# 6A (Pelican Rapids colt)
pr<-lxy.nn.add(df.lxy, id="6A (Pelican Rapids colt)",
                 s=svals[4], a=auto.a(nnn=20, ptp=0.98))
pr<-lxy.lhs(pr, id="6A (Pelican Rapids colt)",
              s=svals[4], a=40500, iso.add = T)
pr.tum<-lhs.visit.add(pr, ivg=3600*24)
lhs.exp.csv(pr.tum, csv.save=T, dir="Data/tum files/2017/Colts")

# 5C (Stockyard colt)
stock<-lxy.nn.add(df.lxy, id="5C (Stockyard colt)",
                 s=svals[5], a=auto.a(nnn=20, ptp=0.98))
stock<-lxy.lhs(stock, id="5C (Stockyard colt)",
              s=svals[5], a=125500, iso.add = T)
stock.tum<-lhs.visit.add(stock, ivg=3600*24)
lhs.exp.csv(stock.tum, csv.save=T, dir="Data/tum files/2017/Colts")

# 7J (Melby colt #1)
melby<-lxy.nn.add(df.lxy, id="7J (Melby colt #1)",
                 s=svals[6], a=auto.a(nnn=20, ptp=0.98))
melby<-lxy.lhs(melby, id="7J (Melby colt #1)",
              s=svals[6], a=282500, iso.add = T)
melby.tum<-lhs.visit.add(melby, ivg=3600*24)
lhs.exp.csv(melby.tum, csv.save=T, dir="Data/tum files/2017/Colts")







