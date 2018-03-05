#######################################################################
#
#  COST ANALYSIS 
#
#######################################################################
library(RODBC);library(plyr)

# IMPORT EFFORT FOR LINKING TO COST
pcname<- Sys.info()[['nodename']]   

#if(pcname!="WFA-F3W30G2"){com4<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")}
#if(pcname=="WFA-F3W30G2"){com4<-odbcConnectAccess2007("C:/Users/sreynolds/Google Drive/Pallid-Sturgeon/analysis-effort/pallids.accdb")}
#dat<-sqlFetch(com4,"cost-summary")

com4<-odbcConnectAccess("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/20171103-pallid-dbase.mdb")
#if(pcname=="WFA-F3W30G2"){com4<-odbcConnectAccess2007("C:/Users/sreynolds/Google Drive/Pallid-Sturgeon/analysis-effort/20171103-pallid-dbase.mdb")}


dat<-sqlFetch(com4,"effort")

names(dat)<-tolower(names(dat))
dat$tmp<-1
daily<- aggregate(tmp~year+setdate+segment_id+fieldoffice+basin+bend,dat,sum) 
daily$tmp<-1

totalBends<- aggregate(tmp~fieldoffice+year,daily,sum)

## COST DATA FOR EACH FIELD CREW
## AND FISCAL YEAR
cost<-read.csv("_dat/sampling-cost.csv",
    skip=10)
names(cost)<-tolower(names(cost))
cost$year<- cost$fiscalyear-1 ## sampling year, sort of
cost$cost<-ifelse(cost$cost>0, cost$cost,NA)

bendCost<-merge(totalBends,cost,
    by=c("year","fieldoffice"))
bendCost$perdep<- bendCost$cost/bendCost$tmp
tmp<-ddply(bendCost,.(fieldoffice),summarize,
    mnlcost=mean(log(na.omit(perdep))),
    sdcost=sd(log(na.omit(perdep))))  
tmp$mncost<- exp(tmp$mnlcost)   

nbends<-data.frame(seg=c(1, 2,  3,  4,  7,  8,  9, 9, 10, 13, 14),
    nbends= c(0, 12, 21, 12, 12, 15, 10,10, 10, 11, 14),
    fieldoffice=c("MT","MT","MT","MR","SD","NE","NE","MO","MO","CF","CF"))
costSeg<- merge(nbends,tmp,by="fieldoffice",all.x=TRUE)
costSeg$k1<- costSeg$nbends*costSeg$mncost 
costSeg$k2<- costSeg$nbends*costSeg$mncost*2
costSeg$k3<- costSeg$nbends*costSeg$mncost*3   
costSeg$k4<- costSeg$nbends*costSeg$mncost*4     

## TOTAL COST TO SAMPLE STURGEON SEASON IN MILLIONS OF DOLLARS
totalCost<- apply(costSeg[,-c(1:6)],2,sum)/1000000     
     
   


   
    
    
    
head(daily)




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









