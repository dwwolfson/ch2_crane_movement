#Creating Time-Use Objects (Colts2016)
library(tlocoh)
library(sp)

rm(list=ls())
df<-read.csv("~/R/CraneMovement/scripts/t-locoh/Data/full.csv")
#could also import the yearly datasets
summary(df$`study-local-timestamp`) #from May 27 2014 -- June 26 2017
df<-df[,c(4,5,17,23)]

colnames(df)[3]<-"id"
colnames(df)[4]<-"loctime"

#SUbset to only spring 2016 for now 
apply(is.na(df),2,sum)
df<-na.omit(df)
df$Hour<-hour(df$loctime)
df$day<-yday(df$loctime)
df$Week<-week(df$loctime)
df$Month<-month(df$loctime)
df$year<-year(df$loctime)

#Select ranges of dates
df<-df[df$year=="2016",]
df<-df[df$Month>3&df$Month<9,]

#Exclude cranes with not enough locaitons to use
table(df$id)
df<-df[!df$id=="3A  (Barb Wire)",]  
df<-droplevels(df)

#Add age info
df$age<-NULL
adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" , "5K (Helliksen July adult 2015)", "5M (Beaulieu adult)" , 
              "6E (Linden-Kologi)", "8K (Deglman #2)","8M (Stockyard adult)", 
              "9A  (Santer)" , "9C (Helliksen adult April 2015)","9K (Santiago #2)" ,"9M (Inman adult)" )

df<-mutate(df, age=ifelse(id%in%adult.list, "adult", "colt")) 

#Drop adults
df<-df[df$age=="colt",]
df<-droplevels(df)

#Create spatial object to use for t-locoh
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
################################################################################################################
# 0C (Callaway)
callaway<-lxy.nn.add(df.lxy, id="0C (Callaway colt 2014)",
                     s=0.004, a=auto.a(nnn=20, ptp=0.98))
callaway<-lxy.lhs(callaway, id="0C (Callaway colt 2014)",
                  s=0.004, a=260000, iso.add=T)
callaway.tum<-lhs.visit.add(callaway, ivg=3600*24)
# lhs.exp.csv(callaway.tum, csv.save = T, dir="Data/tum files")

# 0M (Mud Lake)
mud<-lxy.nn.add(df.lxy, id="0M (N of Mud Lake)",
                s=0.002, a=auto.a(nnn=20, ptp=0.98))
mud<-lxy.lhs(mud, id="0M (N of Mud Lake)",
             s=0.002, a=235000, iso.add = T)
mud.tum<-lhs.visit.add(mud, ivg = 3600*24)
# lhs.exp.csv(mud.tum, csv.save = T, dir="Data/tum files")

# 1K (Melby #2)
melby2<-lxy.nn.add(df.lxy, id="1K (Melby colt #2)",
                   s=0.001, a=auto.a(nnn=20, ptp=0.98))
melby2<-lxy.lhs(melby2, id="1K (Melby colt #2)",
                s=0.001, a=74000, iso.add = T)
melby2.tum<-lhs.visit.add(melby2, ivg = 3600*24)
# lhs.exp.csv(melby2.tum, csv.save = T, dir="Data/tum files")

# 2A (Helliksen)
helliksen<-lxy.nn.add(df.lxy, id="2A (Helliksen July 2015 colt)",
                      s=0.1, a=auto.a(nnn=20, ptp=0.98))
helliksen<-lxy.lhs(helliksen, id="2A (Helliksen July 2015 colt)",
                   s=0.1, a=92000, iso.add = T)
helliksen.tum<-lhs.visit.add(helliksen, ivg = 3600*24)
# lhs.exp.csv(helliksen.tum, csv.save = T, dir="Data/tum files")

#4A (dower)
dower<-lxy.nn.add(df.lxy, id="4A (Dower colt)",
                  s=0.002, a=auto.a(nnn=20, ptp=0.98))
dower<-lxy.lhs(dower, id="4A (Dower colt)",
               s=0.002, a=215000, iso.add = T)
dower.tum<-lhs.visit.add(dower, ivg = 3600*24)
# lhs.exp.csv(dower.tum, csv.save = T, dir="Data/tum files")

# 5A (staples)
staples<-lxy.nn.add(df.lxy, id="5A (W Staples colt #1)",
                    s=0.002, a=auto.a(nnn=20, ptp=0.98))
staples<-lxy.lhs(staples, id="5A (W Staples colt #1)",
                 s=0.002, a=80000, iso.add = T)
staples.tum<-lhs.visit.add(staples, ivg = 3600*24)
# lhs.exp.csv(staples.tum, csv.save = T, dir="Data/tum files")

# 5C (stock)
stock<-lxy.nn.add(df.lxy, id="5C (Stockyard colt)",
                  s=0.002, a=auto.a(nnn=20, ptp=0.98))
stock<-lxy.lhs(stock, id="5C (Stockyard colt)",
               s=0.002, a=190000, iso.add = T)
stock.tum<-lhs.visit.add(stock, ivg = 3600*24)
# lhs.exp.csv(stock.tum, csv.save = T, dir="Data/tum files")

# 6A (pelican)
pelican<-lxy.nn.add(df.lxy, id="6A (Pelican Rapids colt)",
                    s=0.002, a=auto.a(nnn=20, ptp=0.98))
pelican<-lxy.lhs(pelican, id="6A (Pelican Rapids colt)",
                 s=0.002, a=305000, iso.add = T)
pelican.tum<-lhs.visit.add(pelican, ivg = 3600*24)
# lhs.exp.csv(pelican.tum, csv.save = T, dir="Data/tum files")

# 6C (rice lake)
rice.lake<-lxy.nn.add(df.lxy, id="6C (Rice Lake colt 2014)",
                      s=0.005, a=auto.a(nnn=20, ptp=0.98))
rice.lake<-lxy.lhs(rice.lake, id="6C (Rice Lake colt 2014)",
                   s=0.005, a=101000, iso.add = T)
rice.lake.tum<-lhs.visit.add(rice.lake, ivg = 3600*24)
# lhs.exp.csv(rice.lake.tum, csv.save = T, dir="Data/tum files")

# 6M (larson2)
larson2<-lxy.nn.add(df.lxy, id="6M (Larson colt #2)",
                    s=0.002, a=auto.a(nnn=20, ptp=0.98))
larson2<-lxy.lhs(larson2, id="6M (Larson colt #2)",
                 s=0.002, a=200000, iso.add = T)
larson2.tum<-lhs.visit.add(larson2, ivg = 3600*24)
# lhs.exp.csv(larson2.tum, csv.save = T, dir="Data/tum files")

# 7E (nora)
nora<-lxy.nn.add(df.lxy, id="7E (Nora Township colt)",
                 s=0.0025, a=auto.a(nnn=20, ptp=0.98))
nora<-lxy.lhs(nora, id="7E (Nora Township colt)",
              s=0.0025, a=94000, iso.add = T)
nora.tum<-lhs.visit.add(nora, ivg = 3600*24)
# lhs.exp.csv(nora.tum, csv.save = T, dir="Data/tum files")

# 7J (melby1)
melby1<-lxy.nn.add(df.lxy, id="7J (Melby colt #1)",
                   s=0.002, a=auto.a(nnn=20, ptp=0.98))
melby1<-lxy.lhs(melby1, id="7J (Melby colt #1)",
                s=0.002, a=176000, iso.add = T)
melby1.tum<-lhs.visit.add(melby1, ivg = 3600*24)
# lhs.exp.csv(melby1.tum, csv.save = T, dir="Data/tum files")

# 7K (larson1)
larson1<-lxy.nn.add(df.lxy, id="7K (Larson colt #1)",
                    s=0.003, a=auto.a(nnn=20, ptp=0.98))
larson1<-lxy.lhs(larson1, id="7K (Larson colt #1)",
                 s=0.003, a=160000, iso.add = T)
larson1.tum<-lhs.visit.add(larson1, ivg = 3600*24)
# lhs.exp.csv(larson1.tum, csv.save = T, dir="Data/tum files")

