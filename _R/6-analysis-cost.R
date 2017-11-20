#######################################################################
#
#  COST ANALYSIS 
#
#######################################################################
library(RODBC);library(plyr)

# IMPORT EFFORT FOR LINKING TO COST
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
    mnlcost=mean(na.omit(perdep)),
    mdcost=median(na.omit(perdep)),
    sdlcost=sd(na.omit(perdep)),
    mncost=min(na.omit(perdep)),
    mxcost=max(na.omit(perdep)))

## Summary of deployments by year









