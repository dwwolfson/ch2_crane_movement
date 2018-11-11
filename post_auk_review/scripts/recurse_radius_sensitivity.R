library(data.table)
library(tidyverse)
library(recurse)
library(lubridate)
library(sp)
library(plotly)
library(lme4)

#import and rename columns
crane<-fread("raw_data/all_movebank_up_to_10_31_18/Grus canadensis Minnesota.csv")
names(crane)
colnames(crane)[4:5]<-c('long','lat')
colnames(crane)[17]<-'ID'
crane$timestamp<-as.POSIXct(crane$timestamp, format="%Y-%m-%d %H:%M:%OS", tz='UTC')

#get rid of missing locations
ids<-crane%>%select('long','lat', 'timestamp')%>%complete.cases
crane<-crane%>%filter(., ids==TRUE)

#Filter to just summer positions
summer14<-interval(ymd("20140501", tz="UTC"), ymd("20140801", tz="UTC") )
summer15<-interval(ymd("20150501", tz="UTC"), ymd("20150801", tz="UTC") )
summer16<-interval(ymd("20160501", tz="UTC"), ymd("20160801", tz="UTC") )
summer17<-interval(ymd("20170501", tz="UTC"), ymd("20170801", tz="UTC") )
summer18<-interval(ymd("20180501", tz="UTC"), ymd("20180801", tz="UTC") )
summers<-list(summer14, summer15, summer16, summer17, summer18)
ind<-crane$timestamp %within% summers
cranesummer<-crane %>% filter(ind==TRUE)


#Cut out locations east of -93 long
# We can switch to a method using NSD instead but stick with this for now
cranesummer<-cranesummer%>%filter(long<=-93)

# Select and rename variables

cranesummer <- cranesummer %>% select(x = long, y=lat, t=timestamp, id = ID) %>% as.tibble

# Plot data with a different color for each animal

# ggplot(cranesummer, aes(x,y, col=id)) + geom_point(size=2, alpha=0.2)+ theme(legend.position="none")

#Switch to UTM's instead of lat long
cr<-cranesummer
cr.sp<-SpatialPoints(cr[c('x','y')], proj4string = CRS("+proj=longlat +ellps=WGS84"))
#Albers Equal Area projection with specified meridians for boundaries
cr.sp<-spTransform(cr.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
colnames(cr)[1:2]<-c('lon', 'lat')
cr<-cbind(cr, cr.sp@coords)
rm(cr.sp)

#Add on adult v juv info
adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" , "5K (Helliksen July adult 2015)", "5M (Beaulieu adult)" ,
              "6E (Linden-Kologi)", "8K (Deglman #2)","8M (Stockyard adult)",
              "9A  (Santer)" , "9C (Helliksen adult April 2015)","9K (Santiago #2)" ,"9M (Inman adult)" )

cr<-mutate(cr, age=ifelse(id%in%adult.list, "adult", "colt"))

#add year
cr$year<-year(cr$t)

#Save to file 
# write_csv(cr, 'processed_data/projected_df.csv')

# Quantify revisits and duration associated with each location using a nested data frame
cr<-as.tibble(cr)
cranenest <- cr %>% nest(-id)
head(cranenest)

# Need to drop out cranes that died before continuing with analysis?
# Based on my knowledge of who died? Same as in ch2 to be able to compare results?

#Test radius values

unique.ids<-unique(cranesummer$id)
luid<-length(unique.ids)
df<-data.frame()

r<-seq(1,200, by = 10) # can change radius
for(j in 1:length(r)){
for(i in 1: luid){
  # pull off data from individual
  tempdat<-cr %>% filter(id==unique.ids[i])
  tempdat<-tempdat[,c('x', 'y', 'id', 't')]
  tempdat$id<-as.factor(tempdat$id)
  
  # estimate time durations and revisits for radius r
  temprecurs<-getRecursions(as.data.frame(tempdat), j)
  
  # append data 
   tmpdat<-data.frame(id=tempdat$id, timestamp=tempdat$t,
                                           revisits=temprecurs$revisits, 
                                           durations=temprecurs$residenceTime,
                                           timeunits=temprecurs$timeunits, radius=r[j])
   sums <- tmpdat %>% group_by(id) %>% summarize(meanrevisit=mean(revisits),
                                                              meandur=mean(durations),
                                                   varlog_visit=var(log(revisits)),
                                                   varlog_duration=var(log(durations)),
                                                   radius=r[j])
   df<-rbind(df, sums)
   
}}


ggplot(df, aes(radius, varlog_visit, color=id))+geom_line()+
  theme(legend.position = 'none')



df<-mutate(df, age=ifelse(id%in%adult.list, "adult", "colt"))

p<-ggplot(df, aes(radius, varlog_visit, color=id))+geom_line()+
  theme(legend.position = 'none')+facet_wrap(~age)





