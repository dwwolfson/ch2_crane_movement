#' 
#' 
#' Load libraries
#+warning=FALSE, message=FALSE
library(data.table)
library(RColorBrewer)
library(recurse)
library(tidyverse)
library(lubridate)
library(amt)
library(sp)
library(lme4)
library(splines)
library(mgcv)
options(width=160)

#' Import dataset
crane<-read_csv("post_auk_review/processed_data/projected_df.csv") 

#' Make track in amt
trk <- mk_track(crane, .x=lon, .y=lat, .t=t, id = id, age=age)

#' Lets look at sampling rates for each individual
(timestats<-trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
    select(id, sr) %>% unnest)
print(timestats, n=1000)

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

#' Verify we have regular sampling rates
(timestats2<-regtrk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
    select(id, sr) %>% unnest)
print(timestats2, n=1000)

#' ## Calculate various summary statistics from the regularlized track

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


#' Get rid of individuals with little data

trange <- function(x) {
  max(x)- min(x)
}
nt<-trk3 %>% group_by(id) %>% summarize(rangets=trange(t_)) 

large.n.cranes<-nt %>% filter(rangets > 90)
large.n.cranes %>% select(id)

trk4<-trk3[trk3$id %in% large.n.cranes$id,]  
trk4<-trk4[trk4$id %in% c("3A (Barb Wire)", "7A (Hackensack)", "8J (Inman colt #1)") !=TRUE,]
class(trk4)<-class(trk)

#' One obs with really large nsd ....not sure how this crept in. Filter it out.
trk4 %>% filter(nsd_>200000000)
trk4 <- trk4 %>% filter(nsd_<200000000)

#' Project to utms
cr<-trk4
cr.sp<-SpatialPoints(cr[c('x_','y_')], proj4string = CRS("+proj=longlat +ellps=WGS84"))
#Albers Equal Area projection with specified meridians for boundaries
cr.sp<-spTransform(cr.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "))
colnames(cr)[6:7]<-c('lon', 'lat')
cr<-cbind(cr, cr.sp@coords)

#' Get recurse stats
cr$id<-as.factor(cr$id)
cr.r = cr[, c("x_", "y_", 'id', 't_')]

unique.ids<-unique(cr.r$id)
cr_recurse<-NULL
for(i in 1:length(unique.ids)){
  tmpdat<-cr.r%>%filter(id==unique.ids[i])
  tmprecurse<-getRecursions(tmpdat, 50, verbose = FALSE)
  cr_recurse<-rbind(cr_recurse, data.frame(id=tmpdat$id, timestamp=tmpdat$t_,
                                          revisits=tmprecurse$revisits,
                                          durations=tmprecurse$residenceTime,
                                          timeunits=tmprecurse$timeunits,
                                          radius='50_meters'))
  
} 

adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" , "5K (Helliksen July adult 2015)", "5M (Beaulieu adult)" ,
              "6E (Linden-Kologi)", "8K (Deglman #2)","8M (Stockyard adult)",
              "9A  (Santer)" , "9C (Helliksen adult April 2015)","9K (Santiago #2)" ,"9M (Inman adult)" )
cr_recurse<-mutate(cr_recurse, age=ifelse(id%in%adult.list, "adult", "colt"))
cr_recurse$day<-day(cr_recurse$timestamp)
cr_recurse$juliand<-yday(cr_recurse$timestamp)
cr_recurse$yr<-year((cr_recurse$timestamp))

summary(cr_recurse$revisits)
cr_recurse<-cr_recurse[cr_recurse$revisits<1200,]
hist(cr_recurse$revisits)

#natural cubic splines
m1<-lmer(revisits~age+ns(juliand, 5)+ (1|id), data=cr_recurse)
summary(m1)

m2<-lmer(revisits~age+ns(juliand, 5)+(1|id/yr), data=cr_recurse, REML = T)
summary(m2)
plot(m2)
qqnorm(resid(m2), main='lmer(revisits~age+ns(juliand, 5)+(1|id/yr), data=cr_recurse, REML = T)')
qqline(resid(m2))

m3<-lmer(log(revisits)~age+ns(juliand, 5)+ (1|id/yr), data=cr_recurse, REML=T)
plot(m3)
qqnorm(resid(m3), main='lmer(log(revisits)~age+ns(juliand, 5)+ (1|id/yr), data=cr_recurse, REML=T)')
abline(0,1)
summary(m3)

#b splines
cr_recurse$age<-as.factor(cr_recurse$age)
m4<-gam(revisits~age+s(juliand, by=age), data=cr_recurse)
plot(m4)

m5<-gam(log(revisits)~age+s(juliand, by=age), data=cr_recurse)
plot(m5)

m6<-gam(revisits~age+s(juliand, by=age)+s(id, bs="re"), data=cr_recurse)
plot(m6)

m7<-gam(log(revisits)~age+s(juliand, by=age)+s(id, bs="re"), data=cr_recurse)
plot(m7)


AIC(m5, m6, m7)
AIC(m1, m2, m3)

