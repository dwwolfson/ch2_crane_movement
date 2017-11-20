#Creating time-use objects
# Adults 2016

library(tlocoh)
library(sp)

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
unique(df$id)
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
##############################################################################################################
# Finding s-values
ids<-unique(df$id)
# 4E Larson #1
lxy.ptsh.add(df.lxy, id=ids[1]) #0.0075

# 9M Inman
lxy.ptsh.add(df.lxy, id=ids[2]) #0.003

# 9K Santiago #2
lxy.ptsh.add(df.lxy, id=ids[3])  #0.0035

# 8M (Stockyard adult)
lxy.ptsh.add(df.lxy, id=ids[4])  #0.01

# 4J (Santiago #1)
lxy.ptsh.add(df.lxy, id=ids[5])  #0.002

# 3K (Rasmussen adult)
lxy.ptsh.add(df.lxy, id=ids[6])  #0.004

# 8K (Deglman #2)
lxy.ptsh.add(df.lxy, id=ids[7])  #0.0075

# 0J (Waubun adult #2) 
lxy.ptsh.add(df.lxy, id=ids[8])  #0.02

# 5J (Deglman #3)
lxy.ptsh.add(df.lxy, id=ids[9])  #0.0025

# 2M (Bagley)
lxy.ptsh.add(df.lxy, id=ids[10])  #0.004

# 4M (Ogema adult)
lxy.ptsh.add(df.lxy, id=ids[11])  #0.005

# 5K (Helliksen July adult 2015)
lxy.ptsh.add(df.lxy, id=ids[12])  #0.01

# 0K Deglman #1
lxy.ptsh.add(df.lxy, id=ids[13])   #0.004

# 1J (Santiago #3) 
lxy.ptsh.add(df.lxy, id=ids[14])  #0.004

# 4K (Waubun #1) 
lxy.ptsh.add(df.lxy, id=ids[15])  #0.004

# 6E (Linden-Kologi)
lxy.ptsh.add(df.lxy, id=ids[16])   #0.002
#######################################################################
# Find auto-a values and create/export time-use dataframes

# 4E Larson #1
larson1<-lxy.nn.add(df.lxy, id="4E Larson adult #1",
                     s=0.0075, a=auto.a(nnn=20, ptp=0.98))
larson1<-lxy.lhs(larson1, id="4E Larson adult #1",
                 s=0.0075, a=31000, iso.add=T)
lar1.adult.tum<-lhs.visit.add(larson1, ivg=3600*24)
lhs.exp.csv(lar1.adult.tum, csv.save = T, dir="Data/tum files/Adults")

# 9M Inman
lxy.ptsh.add(df.lxy, id=ids[2]) #0.003
inman<-lxy.nn.add(df.lxy, id="9M (Inman adult)",
                    s=0.003, a=auto.a(nnn=20, ptp=0.98))
inman<-lxy.lhs(inman, id="9M (Inman adult)",
                 s=0.003, a=8500, iso.add=T)
inman.tum<-lhs.visit.add(inman, ivg=3600*24)
lhs.exp.csv(inman.tum, csv.save = T, dir="Data/tum files/Adults")

# 9K Santiago #2
lxy.ptsh.add(df.lxy, id=ids[3])  #0.0035
san2<-lxy.nn.add(df.lxy, id="9K (Santiago #2)",
                  s=0.0035, a=auto.a(nnn=20, ptp=0.98))
san2<-lxy.lhs(san2, id="9K (Santiago #2)",
               s=0.0035, a=17500, iso.add=T)
san2.tum<-lhs.visit.add(san2, ivg=3600*24)
lhs.exp.csv(san2.tum, csv.save = T, dir="Data/tum files/Adults")

# 8M (Stockyard adult)
lxy.ptsh.add(df.lxy, id=ids[4])  #0.01
stock<-lxy.nn.add(df.lxy, id="8M (Stockyard adult)",
                 s=0.001, a=auto.a(nnn=20, ptp=0.98))
stock<-lxy.lhs(stock, id="8M (Stockyard adult)",
              s=0.001, a=8000, iso.add=T)
stock.tum<-lhs.visit.add(stock, ivg=3600*24)
lhs.exp.csv(stock.tum, csv.save = T, dir="Data/tum files/Adults")

# 4J (Santiago #1)
lxy.ptsh.add(df.lxy, id=ids[5])  #0.002
san1<-lxy.nn.add(df.lxy, id="4J (Santiago #1)",
                  s=0.002, a=auto.a(nnn=20, ptp=0.98))
san1<-lxy.lhs(san1, id="4J (Santiago #1)",
               s=0.002, a=48000, iso.add=T)
san1.tum<-lhs.visit.add(san1, ivg=3600*24)
lhs.exp.csv(san1.tum, csv.save = T, dir="Data/tum files/Adults")

# 3K (Rasmussen adult)
lxy.ptsh.add(df.lxy, id=ids[6])  #0.004
ras<-lxy.nn.add(df.lxy, id="3K (Rasmussen adult)",
                 s=0.004, a=auto.a(nnn=20, ptp=0.98))
ras<-lxy.lhs(ras, id="3K (Rasmussen adult)",
              s=0.004, a=23000, iso.add=T)
ras.tum<-lhs.visit.add(ras, ivg=3600*24)
lhs.exp.csv(ras.tum, csv.save = T, dir="Data/tum files/Adults")

# 8K (Deglman #2)
lxy.ptsh.add(df.lxy, id=ids[7])  #0.0075
deg2<-lxy.nn.add(df.lxy, id="8K (Deglman #2)",
                s=0.0075, a=auto.a(nnn=20, ptp=0.98))
deg2<-lxy.lhs(deg2, id="8K (Deglman #2)",
             s=0.0075, a=36000, iso.add=T)
deg2.tum<-lhs.visit.add(deg2, ivg=3600*24)
lhs.exp.csv(deg2.tum, csv.save = T, dir="Data/tum files/Adults")

# 0J (Waubun adult #2) 
lxy.ptsh.add(df.lxy, id=ids[8])  #0.02
waub2<-lxy.nn.add(df.lxy, id="0J (Waubun adult #2)",
                 s=0.02, a=auto.a(nnn=20, ptp=0.98))
waub2<-lxy.lhs(waub2, id="0J (Waubun adult #2)",
              s=0.02, a=18000, iso.add=T)
waub2.tum<-lhs.visit.add(waub2, ivg=3600*24)
lhs.exp.csv(waub2.tum, csv.save = T, dir="Data/tum files/Adults")

# 5J (Deglman #3)
lxy.ptsh.add(df.lxy, id=ids[9])  #0.0025
deg3<-lxy.nn.add(df.lxy, id="5J (Deglman #3)",
                  s=0.0025, a=auto.a(nnn=20, ptp=0.98))
deg3<-lxy.lhs(deg3, id="5J (Deglman #3)",
               s=0.0025, a=19000, iso.add=T)
deg3.tum<-lhs.visit.add(deg3, ivg=3600*24)
lhs.exp.csv(deg3.tum, csv.save = T, dir="Data/tum files/Adults")

# 2M (Bagley)
lxy.ptsh.add(df.lxy, id=ids[10])  #0.004
bag<-lxy.nn.add(df.lxy, id="2M (Bagley)",
                  s=0.004, a=auto.a(nnn=20, ptp=0.98))
bag<-lxy.lhs(bag, id="2M (Bagley)",
               s=0.004, a=32000, iso.add=T)
bag.tum<-lhs.visit.add(bag, ivg=3600*24)
lhs.exp.csv(bag.tum, csv.save = T, dir="Data/tum files/Adults")

# 4M (Ogema adult)
lxy.ptsh.add(df.lxy, id=ids[11])  #0.005
ogema<-lxy.nn.add(df.lxy, id="4M (Ogema adult)",
                  s=0.005, a=auto.a(nnn=20, ptp=0.98))
ogema<-lxy.lhs(ogema, id="4M (Ogema adult)",
               s=0.005, a=15000, iso.add=T)
ogema.tum<-lhs.visit.add(ogema, ivg=3600*24)
lhs.exp.csv(ogema.tum, csv.save = T, dir="Data/tum files/Adults")

# 5K (Helliksen July adult 2015)
lxy.ptsh.add(df.lxy, id=ids[12])  #0.01
hel<-lxy.nn.add(df.lxy, id="5K (Helliksen July adult 2015)",
                  s=0.01, a=auto.a(nnn=20, ptp=0.98))
hel<-lxy.lhs(hel, id="5K (Helliksen July adult 2015)",
               s=0.01, a=18000, iso.add=T)
hel.tum<-lhs.visit.add(hel, ivg=3600*24)
lhs.exp.csv(hel.tum, csv.save = T, dir="Data/tum files/Adults")

# 0K Deglman #1
lxy.ptsh.add(df.lxy, id=ids[13])   #0.004
deg1<-lxy.nn.add(df.lxy, id="0K Deglman #1",
                  s=0.004, a=auto.a(nnn=20, ptp=0.98))
deg1<-lxy.lhs(deg1, id="0K Deglman #1",
               s=0.004, a=30000, iso.add=T)
deg1.tum<-lhs.visit.add(deg1, ivg=3600*24)
lhs.exp.csv(deg1.tum, csv.save = T, dir="Data/tum files/Adults")

# 1J (Santiago #3) 
lxy.ptsh.add(df.lxy, id=ids[14])  #0.004
san3<-lxy.nn.add(df.lxy, id="1J (Santiago #3)",
                  s=0.004, a=auto.a(nnn=20, ptp=0.98))
san3<-lxy.lhs(san3, id="1J (Santiago #3)",
               s=0.004, a=23500, iso.add=T)
san3.tum<-lhs.visit.add(san3, ivg=3600*24)
lhs.exp.csv(san3.tum, csv.save = T, dir="Data/tum files/Adults")

# 4K (Waubun #1) 
lxy.ptsh.add(df.lxy, id=ids[15])  #0.004
waub1<-lxy.nn.add(df.lxy, id="4K (Waubun #1)",
                  s=0.004, a=auto.a(nnn=20, ptp=0.98))
waub1<-lxy.lhs(waub1, id="4K (Waubun #1)",
               s=0.004, a=33500, iso.add=T)
waub1.tum<-lhs.visit.add(waub1, ivg=3600*24)
lhs.exp.csv(waub1.tum, csv.save = T, dir="Data/tum files/Adults")

# 6E (Linden-Kologi)
lxy.ptsh.add(df.lxy, id=ids[16])   #0.002
lind<-lxy.nn.add(df.lxy, id="6E (Linden-Kologi)",
                  s=0.002, a=auto.a(nnn=20, ptp=0.98))
lind<-lxy.lhs(lind, id="6E (Linden-Kologi)",
               s=0.002, a=37000, iso.add=T)
lind.tum<-lhs.visit.add(lind, ivg=3600*24)
lhs.exp.csv(lind.tum, csv.save = T, dir="Data/tum files/Adults")






