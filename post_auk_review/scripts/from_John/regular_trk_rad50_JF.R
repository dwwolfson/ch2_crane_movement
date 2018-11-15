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
library(knitr)
knitr::opts_chunk$set(error = TRUE)

#' Import dataset (if running in console)
#crane<-read_csv("post_auk_review/processed_data/projected_df.csv") 

# If knitting
crane<-read_csv("../processed_data/projected_df.csv") 

crane$t<-as.character(crane$t)
crane$t<-as.POSIXct(crane$t, format="%Y-%m-%d %H:%M:%S",
                       tz="America/Chicago")
 
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
cr.r = cr[, c("x_", "y_",  't_', 'id')]

#' Look at reasonable radius based on step length
trk.cr <- mk_track(cr.r, .x=x_, .y=y_, .t=t_, id = id)
trk2.cr<-trk.cr %>% nest(-id) %>% 
  mutate(sl = map(data, step_lengths),
         nsd_=map(data, nsd))%>%unnest()
summary(trk2.cr$sl)  # this makes it look like 50 may be OK

#' Note, I think a larger radius may be appropriate since
#' over half of the step lengths are < 50.  Lets look at the
#' distribution of log(sl)
#+ fig.height=4, fig.width=8
par(mfrow=c(1,2))
hist(trk2.cr$sl, main="Step Lengths")
hist(log(trk2.cr$sl), main="log Step Lengths")
exp(5)
par(mfrow=c(1,1))

#' Peak appears to be around 5...exp(5)~ 150  
#' Lets compare a radius of 5 and 150 for an individual (you 
#' can choose the individual using i, below)
i<-10
unique.ids<-unique(cr.r$id)
tmpdat<-cr.r%>%filter(id==unique.ids[i])
tmpsl<-cr%>%filter(id==unique.ids[i])%>%select(sl)
tmprecurse<-getRecursions(tmpdat, 150, verbose = TRUE)
plot(tmprecurse, tmpdat)
drawCircle(mean(tmpdat$x_), mean(tmpdat$y_), 50) # original circle...small point
drawCircle(mean(tmpdat$x_), mean(tmpdat$y_), 150, lwd=2) # original circle...small point


cr_recurse<-NULL
for(i in 1:length(unique.ids)){
  tmpdat<-cr.r%>%filter(id==unique.ids[i])
  tmpsl<-cr%>%filter(id==unique.ids[i])%>%select(sl)
  tmprecurse<-getRecursions(tmpdat, 150, verbose = TRUE)
  cr_recurse<-rbind(cr_recurse, data.frame(id=tmpdat$id, timestamp=tmpdat$t_,
                                          revisits=tmprecurse$revisits,
                                          durations=tmprecurse$residenceTime,
                                          timeunits=tmprecurse$timeunits,
                                          radius='50_meters', 
                                          sl=tmpsl,x=tmpdat$x, y=tmpdat$y))
  
} 

#' Get adult juv and add to data frame 
adult.list<-c("0J (Waubun adult #2)", "0K Deglman #1","1J (Santiago #3)" ,
              "2C (Melby adult)" , "2M (Bagley)" , "3C (Larson adult #2)" ,
              "3E (Wambach adult)" , "3K (Rasmussen adult)",  "3M  (Callaway adult)" ,
              "4E Larson adult #1" , "4J (Santiago #1)",  "4K (Waubun #1)" ,  "4M (Ogema adult)" ,
              "5J (Deglman #3)" , "5K (Helliksen July adult 2015)", "5M (Beaulieu adult)" ,
              "6E (Linden-Kologi)", "8K (Deglman #2)","8M (Stockyard adult)",
              "9A  (Santer)" , "9C (Helliksen adult April 2015)","9K (Santiago #2)" ,"9M (Inman adult)" )
cr_recurse<-mutate(cr_recurse, age=ifelse(id%in%adult.list, "adult", "colt"))
cr_recurse$age<-as.factor(cr_recurse$age)

#' add day, julian day, year
cr_recurse$day<-day(cr_recurse$timestamp)
cr_recurse$juliand<-yday(cr_recurse$timestamp)
cr_recurse$yr<-year((cr_recurse$timestamp))

#' Summary of revisitation statistics and durations
#' 
#' - these are definitely skewed distributions!  
#' - Will want to think about modeling on log scale
#' - also have counts (revisits), but really large counts....
#+ fig.height=8, fig.width=8
summary(cr_recurse$revisits)
cr_recurse<-cr_recurse[cr_recurse$revisits<1200,]
par(mfrow=c(2,2))
hist(cr_recurse$revisits)
hist(cr_recurse$durations)

hist(log(cr_recurse$revisits))
hist(log(cr_recurse$durations))
par(mfrow=c(1,1))


#' Lets look at some plots of how things vary by adult/juv and 
#' over time
#' 
#' Order by adult/juv
cr_recurse <-mutate(cr_recurse, Subject = reorder(age, id))

#+ fig.height=18, fig.width=24
ggplot(cr_recurse, aes(x=as.Date(juliand, origin = as.Date("2018-01-01")),
                       y=log(durations), colour=age))  +
  facet_wrap(~Subject+id)+geom_smooth()+
  scale_x_date(date_labels = "%b")+ xlab("")

#+ fig.height=18, fig.width=24
ggplot(cr_recurse, aes(x=as.Date(juliand, origin = as.Date("2018-01-01")),
                       y=log(revisits), colour=age)) +
  facet_wrap(~Subject + id)+geom_smooth()+
  scale_x_date(date_labels = "%b")+ xlab("")

#' These are not all that wigly for each individual...
#' Could almost get away with something like:
#' 
m1<-lmer(log(revisits)~age+juliand + age*juliand+ (1|id/yr), data=cr_recurse, REML=T)
plot(m1)
summary(m1)
qqnorm(resid(m1), main='lmer(log(revisits)~age+ns(juliand, 5)+ (1|id/yr), data=cr_recurse, REML=T)')
abline(0,1)

#' Or, consider fitting using a count distribution 
#' (negative binomial), where the effects of covariates
#' enter on the log scale. Here, it can help to scale
#' julian date when fitting.
cr_recurse$sjd<-scale(cr_recurse$juliand)
m2<-glmer.nb(revisits~age+sjd + age*sjd+  (1|id), data=cr_recurse, REML=T)
plot(m2)
summary(m2)

#b splines
#m4<-gam(revisits~age+s(juliand, by=age), data=cr_recurse)
#plot(m4)
#' We fit similar models using gam, but this also allows
#' us to allow the effect of julian date to vary smoothly
m3<-gam(log(revisits)~age+s(juliand, by=age)+s(id, bs="re"), data=cr_recurse)
plot(m3)
summary(m3)

#' Gam also appears to allow fitting of negative binomial models.
#' This may be one to consider...
m4<-gam(revisits~age+s(juliand, by=age)+s(id, bs="re"), family=nb(), data=cr_recurse)
plot(m4)
summary(m4)  
gam.check(m4)


#' NOte, it is only approporiate to compare models fit to the same response
#' 
#' - comparisons of log(revisits) and revisits are not appropriate
#' - also, different fitting functions may not give same AIC for same model
#' 
#' So, I did not compare models using AIC.  I think we want to pick a reasonable 
#' model based on what we think is appropriate & then just do
#' some model checking to make sure assumptions are reasonably met.
#' 

#' Could also look to fit models to duration
d7<-gam(durations~age+s(juliand, by=age)+s(id, bs="re"), family=nb(), data=cr_recurse)
plot(d7)
gam.vcomp(d7)
summary(d7)
gam.check(d7)

#' For more on gams, see:
#' 
#' - https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/negbin.html
#' - https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html
#' 
#' 
#' And more on recurse
#' 
#' - https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html
 