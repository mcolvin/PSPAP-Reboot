#######################################################################
#
#  COST ANALYSIS 
#
#######################################################################
library(RODBC);library(plyr)

# MOVE COST SUMMARY OUT OF DBASE TO CSV AND DOCUMENT
# THE SHIT OUT OF IT
com4<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
dat<-sqlFetch(com4,"cost-summary")

names(dat)<-tolower(names(dat))
dat$tmp<-1 # TO SUM UP DEPLOYMENTS

## COST DATA FOR EACH FIELD CREW
## AND FISCAL YEAR
cost<-read.csv("_dat/sampling-cost.csv",
    skip=10)
names(cost)<-tolower(names(cost))
cost$year<- cost$fiscalyear-1 ## sampling year, sort of
cost$cost<-ifelse(cost$cost>0, cost$cost,NA)
## note: year is the season year, effort after 
## december 31 is included in the previous year
deployments<-aggregate(tmp~year+fieldoffice,
    dat,sum)

depcost<-merge(deployments,cost,
    by=c("year","fieldoffice"))
    
depcost$perdep<- depcost$cost/depcost$tmp
depcost$lperdep<- log(depcost$perdep)



tmp<-ddply(depcost,.(fieldoffice),summarize,
    mnlcost=mean(na.omit(lperdep)),
    mdcost=median(na.omit(perdep)),
    sdlcost=sd(na.omit(lperdep)),
    mncost=min(na.omit(perdep)),
    mxcost=max(na.omit(perdep)))


fo<-unique(depcost$fieldoffice)
for(i in 1:length(fo))
    {
    points(lperdep~year,depcost,
        subset=fieldoffice==fo[i],
        type='l',
        col=i,
        lwd=2)
    }

fit<-lm(lperdep~year+fieldoffice+year:fieldoffice,
    depcost)
fit<-lm(lperdep~fieldoffice,
    depcost)
depcost$ldepcosthat<- predict(fit,depcost)  
  
    