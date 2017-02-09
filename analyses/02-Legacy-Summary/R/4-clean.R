
dat$tmp<-1
dat$year<- as.numeric(format(dat$SETDATE, "%Y"))
dat<- subset(dat, SEASON!="HW")
dat$SEASON<- factor(dat$SEASON)
dat$BEND_INT<- as.integer(as.character(dat$BEND))

