#######################################################################
#
#  THIS ANALYSIS PULLS DAYS SAMPLED FOR STURGEON AND SUMMARIZES BY SEGEMENT
#  TO SEE HOW SAMPLING WILL WORK WITH SEGMENT LEVEL AND MULTIPLE OCCASIONS
#  June 1 is day 181, September 1 is day 244
#######################################################################

library(plyr)
library(RODBC)
com<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/20171103-pallid-dbase.mdb")

## COMBINE SEGMENTS 5 AND 6
ef$SEGMENT_ID<- ifelse(ef$SEGMENT_ID %in% c(5,6), 5,ef$SEGMENT_ID)

effort<-sqlFetch(com,"EFFORT")
ef<-subset(effort,SEASON%in% c("ST" ,"FC"))
ef<-subset(ef, !(SEASON == "FC" & SEGMENT_ID>4))
ef$doy<-as.numeric(format(ef$SETDATE,"%j"))
ef$wk<- floor(ef$doy/7)
ef$tempExceeded<- ifelse(ef$TEMP<12.8 & ef$SEGMENT_ID>4,0,1)## NOT GILL NETS IN UPPER BASIN


## FIELD OFFICE FOR EACH SEGMENT
fo<- ddply(ef,.(SEGMENT_ID,FIELDOFFICE),summarize,
    temp=length(tempExceeded))
fo<-subset(fo,SEGMENT_ID %in% c(1,2,3,4,5,7,8,9,10,13,14)&FIELDOFFICE!= "MR")

## FIGURE OUT YEARS THAT ARE SAMPLED 
years<- lapply(segs,function(x)
    {
    pp<-subset(ef,SEGMENT_ID==x)
    return(length(unique(pp$YEAR)))
    
    })



## CALCULATE THE CRUDE PROBABILITY THAT THE 12.8 DEGREES WILL BE EXCEEDED.
### SUMMARIZE DAILY TEMPERATURE OBSERVATIONS WITHIN BEND AND YEAR
mntemp<-ddply(ef,.(SEGMENT_ID,YEAR,doy,wk,BEND),summarize,
    temp=mean(TEMP,na.rm=TRUE),
    pexceed=mean(tempExceeded,na.rm=TRUE))
mntemp$exceed<-ifelse(mntemp$pexceed>0.5,1,0) # bin things above 0.5 as no go.
    
### SUMMARIZE THE PROPORTION OF DAYS TEMP WAS EXCEEDED IN EACH WEEK
mntemp2<-ddply(mntemp,.(SEGMENT_ID,wk),summarize,
    temp=mean(exceed,na.rm=TRUE))    
mntemp2$season<-"Grey"
mntemp2[which(mntemp2$wk<= floor(181/7)),]$season<-"Spring"
mntemp2[which(mntemp2$wk>= floor(244/7)),]$season<-"Fall"
segs<- c(2,3,4,5,7,8,9,10,13,14)
out<-lapply(segs,function(x)
    {
    pp<-subset(mntemp2, SEGMENT_ID==x)
    #plot(temp~wk,pp)
    if(x<5)
        {
    ## FALL SAMPLING
    fallStart= min(pp[which(pp$season=="Fall"),]$wk)
    fallEnd= max(pp[which(pp$season=="Fall" ),]$wk)
    nFallWeeks= max(pp[which(pp$season=="Fall"),]$wk)-fallStart
    ## SPRING SAMPLING
    springStart= min(pp[which(pp$season=="Spring" ),]$wk)
    springEnd= max(pp[which(pp$season=="Spring" ),]$wk)
    nSpringWeeks= max(pp[which(pp$season=="Spring" ),]$wk)-springStart   
        }
    if(x>=5)
        {
    ## FALL SAMPLING
    fallStart= min(pp[which(pp$season=="Fall" & pp$temp<0.5),]$wk)
    fallEnd= max(pp[which(pp$season=="Fall" & pp$temp<0.5),]$wk)
    nFallWeeks= max(pp[which(pp$season=="Fall" & pp$temp<0.5),]$wk)-fallStart
    ## SPRING SAMPLING
    springStart= min(pp[which(pp$season=="Spring" & pp$temp<0.5),]$wk)
    springEnd= max(pp[which(pp$season=="Spring" & pp$temp<0.5),]$wk)
    nSpringWeeks= max(pp[which(pp$season=="Spring" & pp$temp<0.5),]$wk)-springStart   
        }
    return(data.frame(segment=x,fallStart=fallStart,fallEnd=fallEnd,nFallWeeks=nFallWeeks,
       springStart=springStart,springEnd=springEnd,nSpringWeeks=nSpringWeeks))
    })
out<-do.call("rbind",out)   

write.csv(out,"_output/weeks.csv")
## plot probability of temperature exceedance by segement
plot(TEMP~doy,ef,subset=SEGMENT_ID==13);abline(h=12.8);abline(v=181)
plot(temp~wk,mntemp2,subset=SEGMENT_ID==2);abline(v=181/7);
plot(temp~doy,mntemp,subset=SEGMENT_ID==3);abline(v=181/7)
plot(temp~doy,mntemp,subset=SEGMENT_ID==4);abline(v=181/7)

plot(temp~doy,mntemp,subset=SEGMENT_ID==7);abline(v=181/7)
plot(temp~doy,mntemp,subset=SEGMENT_ID==8);abline(v=181/7)
plot(temp~doy,mntemp,subset=SEGMENT_ID==9);abline(v=181/7)
plot(temp~doy,mntemp,subset=SEGMENT_ID==10);abline(v=181/7)
plot(temp~doy,mntemp,subset=SEGMENT_ID==13);abline(v=181/7)
plot(temp~doy,mntemp,subset=SEGMENT_ID==14);abline(v=181/7)


pspap<-data.frame(
    crew=c("MT","MT","MT","SD","
    SEGMENT_ID = c(2,3,4,7,8,9,10,10,13,14),
    nsample= c(12,21,12,12,15,10,10,10,11,14))
pspap$ncrews<-2    
pspap$nbendsperweek<-2    
pspap$nweeks<-pspap$nsample/(pspap$ncrews*pspap$nbendsperweek) 


100/7




#ef<-subset(ef, SEASON=="ST")

## The Sturgeon Season will begin in the fall when the water 
## temperature is < 12.8°C (55°F) and will continue through June 30th
## DOY=181. 
ef$samplingYear<- ifelse(ef$doy<=181, ef$YEAR-1,ef$YEAR)


ef[ef$SEGMENT_ID==2 & ef$samplingYear==2012,]
## MOST UPPER BASIN STURGEON SEASON SAMPLING HAPPENS IN THE SPRING

ddply(ef,.(SEGMENT_ID,samplingYear),summarize,
    start=min(ifelse(doy<=181, 1000, doy)))

 sort(ef[ef$SEGMENT_ID==2 & ef$YEAR==2012,]$SETDATE)
 ef[ef$SEGMENT_ID==2 & ef$samplingYear==2012,]$SETDATE

 
    (ifelse(ef[ef$SEGMENT_ID==2 & ef$samplingYear==2012,]$doy<=181, 1000,
        ef[ef$SEGMENT_ID==2 & ef$samplingYear==2012,]$doy))
ef[ef$SEGMENT_ID==2 & ef$samplingYear==2012,]$
