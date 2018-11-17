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
library(knitr)
library(sjPlot)
library(mgcViz)
library(ezknitr)
options(width=160)


#' Import recurse dataset
cr_recurse<-read_csv("processed_data/recurse_dat_150m.csv")

#' ### Revisitation model fitting
m1<-lmer(log(revisits)~age+juliand + age*juliand+ (1|id/yr), data=cr_recurse, REML=T)

cr_recurse$sjd<-scale(cr_recurse$juliand)
m2<-glmer.nb(revisits~age+sjd + age*sjd+(1|id), data=cr_recurse, REML=T)

#' We fit similar models using gam, but this also allows
#' us to allow the effect of julian date to vary smoothly
m3<-gam(log(revisits)~age+s(juliand, by=age)+s(id, bs="re"), data=cr_recurse)

#' Gam also appears to allow fitting of negative binomial models.
#' This may be one to consider...
m4<-gam(revisits~age+s(juliand, by=age)+s(id, bs="re"), family=nb(), data=cr_recurse, REML=TRUE)

#' Effect of day by each crane
# m5<-gam(revisits~age + s(juliand, by=id), family=nb, data=cr_recurse, REML=TRUE) #didn't converge

# Negative binomial with random slopes
m6<-glmer.nb(revisits~age+juliand+(1+juliand|id), data=cr_recurse)
m7<-glmer.nb(revisits~age+sjd+(1+sjd|id), data=cr_recurse) #scaled version (julian day) of prior model; ran much faster

#########
#' Model diagnostics
plot(m1)
plot(m2)
m3<-getViz(m3)
check(m3, a.qq = list(method='tnorm'), a.hist = list(bins=20), a.respoi = list(size=1))
m4<-getViz(m4)
check(m4, a.qq = list(method='tnorm'), a.hist = list(bins=20), a.respoi = list(size=1))
plot(m6)
plot(m7)

############
#' Could also look to fit models to duration
d7<-gam(durations~age+s(juliand, by=age)+s(id, bs="re"), family=nb(), data=cr_recurse)
d8<-gam(log(durations)~age+s(juliand, by=age)+s(id, bs='re'), data=cr_recurse)

#' Model checking for all the gam's
d7<-getViz(d7)
check(d7, a.qq = list(method='tnorm'), a.hist = list(bins=20), a.respoi = list(size=1), main='title')

d8<-getViz(d8)
check(d8, a.qq = list(method='tnorm'), a.hist = list(bins=20), a.respoi = list(size=1))

ezspin(file = "scripts/recurse_model_fits.R",wd = "D:/Data_Documents/Cranes/ch2_crane_movement/post_auk_review",out_dir ="knit_htmls" )
