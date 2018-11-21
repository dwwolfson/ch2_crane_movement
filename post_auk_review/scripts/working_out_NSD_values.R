#' 
#' 
#' Load libraries
#+warning=FALSE, message=FALSE
#library(feather)
library(data.table)
library(recurse)
library(leaflet)
library(tidyverse)
library(lubridate)
library(amt)
library(sp)
library(ezknitr)
options(width=160)

#' ## Data development
#' 
#' Read in data
crane <- read_csv("raw_data/all_movebank_up_to_10_31_18/Grus canadensis Minnesota.csv")
names(crane)[4:5]<-c("long", "lat")
names(crane)[17]<-"ID"
names(crane)[23]<-"Timestamp"
crane$Timestamp<-as.POSIXct(crane$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#' Get rid of missing locations
ids <- crane %>% select("long", "lat", "Timestamp") %>% complete.cases
crane<-crane %>% filter(., ids==TRUE)

#' Remove 2 outliers points
crane<-crane[crane$long<(-75),]


#Switch to UTM's instead of lat long
cr<-crane
cr.sp<-SpatialPoints(cr[c('long','lat')], proj4string = CRS("+proj=longlat +ellps=WGS84"))
#Albers Equal Area projection with specified meridians for boundaries
cr.sp<-spTransform(cr.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
cr<-cbind(cr, cr.sp@coords)
colnames(cr)[24:25]<-c('utm_x','utm_y')
cr<-cr[,c('Timestamp', 'long', 'lat', 'ID', 'utm_x', 'utm_y')]


adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" , "5K (Helliksen July adult 2015)", "5M (Beaulieu adult)" ,
              "6E (Linden-Kologi)", "8K (Deglman #2)","8M (Stockyard adult)",
              "9A  (Santer)" , "9C (Helliksen adult April 2015)","9K (Santiago #2)" ,"9M (Inman adult)" )
cr<-mutate(cr, age=ifelse(ID%in%adult.list, "adult", "colt"))

#' Make track in amt
trk <- mk_track(cr, .x=utm_x, .y=utm_y, .t=Timestamp, id = ID, age=age, lat=lat, long=long)


#' Most have regular sampling rates (but, there are some short intervals and some
#' that have 30 minute intervals).  Lets regularize these to have 1 obs every 30
#' minutes and only include bursts of observations if there are 3 in a row that
#' are 30 min apart.
regtrk<- trk %>%  nest(-id) %>% 
    mutate(regtrk = map(data, function(d) {
      d %>%
        track_resample(rate = minutes(30), tolerance = minutes(3)) %>% 
      filter_min_n_burst(min_n = 3) 
    })) %>% select(id, regtrk) %>% unnest()

#' Make this a trajectory again
class(regtrk)<-class(trk)


#' ## Calculate various summary statistics from the regularized track

#' Now, lets calculate step length, turn angles, angles relative to due N, 
#' and net-squared displacement for each individual.
trk2<-regtrk %>% nest(-id) %>% 
  mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd))%>%unnest()

#' Add on the week, month, year, and hour of the observation
trk3<-trk2%>% 
  mutate(
    week=week(t_),
    month = month(t_, label=TRUE), 
    year=year(t_),
    hour = hour(t_)
  )




#' ## Some plots
#' 
#+ fig.width=12, fig.height=8
ggplot(trk3, aes(x = t_, y=sqrt(nsd_)/1000, colour=age)) + geom_point()+facet_wrap(~id)+theme(legend.position = 'none')


#' Look at max NSD values if you use -93 to filter
long_filt<-trk3[trk3$long<=(-93)&trk3$lat>44,]
summary(sqrt(long_filt$nsd_)) # sqrt-NSD; actual displacement in meters
summary(sqrt(long_filt$nsd_)/1000) #sqrt-NSD; actual displacement in kilometers

summary(sqrt(trk3$nsd_))
summary(sqrt(trk3$nsd_)/1000)

#' 200km seems like a reasonable distance for being 'back on the summer range', based on looking at plots
#' that would be 4e+10, or 40,000,000,000 NSD in meters\  

#' CHeck for outliers
y14<-trk3[trk3$year==2014,]
y15<-trk3[trk3$year==2015,]
y16<-trk3[trk3$year==2016,]
y17<-trk3[trk3$year==2017,]

#' I cycled through each year in the first argument to check out crane locations
leaflet(y17)%>%
  addTiles()%>%
  setView(lat = 45.9,lng =  -95.0, zoom=6)%>%
  addCircles(~long, ~lat)
# I cycled through all the years and visually checked the maps of the points
# just to make sure nothing was too crazy.
# these meridians are picking up some crane stagin in SD right now, but that 
# shouldn't be the case once I apply the NSD cutoff (somewhere between 100 and 250km?)

# I'll need to be careful with the Sherburne cranes and the two that flew way up to Ontario.
# Maybe I should separate those and deal with them separately.

table(trk3$id, trk3$year)
# SHould probably remove cranes that were either known or likely mortalities before they fledged;
# (likely meaning they only gave a little bit of data, ie less than 300 locations)
morts<-c("0A  (Callaway colt 2015)", "0E (recovered from Ras colt #2)", "2K  (Rasmussen colt #1)",
         "4C (Star Lake/Tweeton)", "5E (Linden Kologi colt #2)", "7M (E of Mud Lake colt)")
filt<-trk3[!trk3$id%in%morts,]


nsd_db<-data.frame(id=NA, nsd_=NA, x_=NA, y_=NA, t_=NA, age=NA, lat=NA, long=NA, year=NA, spring_cutoff=NA,fall_cutoff=NA,nsd_cutoff=NA )
nsd_threshold<-40000000000

#' Compile filtered database
unique_ids<-unique(filt$id)
for(i in 1:length(unique_ids)){
  tmp<-filt[filt$id==unique_ids[i],]
  tmp_yrs<-unique(tmp$year)
    for(j in 1:length(tmp_yrs)){
      tmp_yr_db<-tmp[tmp$year==tmp_yrs[j],]
      tmp_nsd_filt<-tmp_yr_db[tmp_yr_db$nsd_<=40000000000,]
      if(nrow(tmp_nsd_filt)>0){
      tmp_nsd_filt$spring_cutoff<-as.POSIXct(min(tmp_nsd_filt$t_))
      tmp_nsd_filt$fall_cutoff<-as.POSIXct(max(tmp_nsd_filt$t_))
      tmp_nsd_filt$nsd_cutoff<-nsd_threshold
      #combine results
      nsd_db<-bind_rows(nsd_db, select(tmp_nsd_filt, -dir_abs, -dir_rel, -sl, -burst_, -week, -month, -hour))
      }
  }
}


#' look at an example to see if it is working
stock_colt<-nsd_db[nsd_db$id=="5C (Stockyard colt)",]
stock_vlines<-unique(stock_colt$spring_cutoff)
stock_vlines<-as.data.frame(stock_vlines[2:5])
colnames(stock_vlines)[1]<-'spring_starts'
stock_vlines$season<-'spring'
stock_fall_lines<-as.data.frame(unique(stock_colt$fall_cutoff)[-1])
colnames(stock_fall_lines)[1]<-'spring_starts' # this is just so names match
stock_fall_lines$season<-'fall'
stock_vlines<-rbind(stock_vlines, stock_fall_lines)
ggplot(filt[filt$id=="5C (Stockyard colt)",], aes(x=t_, y=nsd_))+geom_point()+
  geom_hline(aes(yintercept=nsd_threshold), color='red')+
  geom_vline(data=stock_vlines, aes(xintercept = spring_starts, color=season))+
  ylim(0,1000000000000)



unique(nsd_db$spring_cutoff)
unique(nsd_db$fall_cutoff)
#' These are all over the place, and probably not just becauase gps coverage is spotty.

ezspin("post_auk_review/scripts/working_out_NSD_values.R", out_dir = "post_auk_review/knit_htmls")
sessionInfo()