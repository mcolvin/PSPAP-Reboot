


source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

## QUERY LENGTH WEIGHT DATA FROM PSPAP DATABASE
#com99<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PDSG-LW/dat/dat.accdb")
com99<-odbcConnectAccess2007("C:/Users/sreynolds/Google Drive/Pallid-Sturgeon/Analysis/PDSG-LW/dat/dat.accdb")
eff<- sqlFetch(com99,"eff")
fish<- sqlFetch(com99,"fish")
stocked<- sqlFetch(com99,"stocked")
genetics<- sqlFetch(com99,"genetics")

names(eff)<-tolower(names(eff))
names(fish)<-tolower(names(fish))
names(stocked)<-tolower(names(stocked))
names(genetics)<- tolower(names(genetics))

genetics[genetics==""]<-NA
genetics$pittag_1<- toupper(genetics$pittag_1)
genetics$pittag_2<- toupper(genetics$pittag_2)
stocked$pit_tag<- toupper(stocked$pit_tag)
stocked$pit_tag_2<- toupper(stocked$pit_tag_2)
fish$tagnumber<- toupper(fish$tagnumber)


# CLEAN AND MANINIPULATE
tmp<- merge(eff,fish,by.x="uniqueidentifier",by.y="uniqueid")

# SUBSET OUT FISH THAT ARE OUTLIERS 
## REMOVE FISH WHERE WEIGHT AND LENGTH ARE NA OR NON-POSITIVE
tmp<-subset(tmp, !is.na(length))
tmp<-subset(tmp, !is.na(weight))
tmp<- subset(tmp, length>0)
tmp<- subset(tmp, weight>0)
## LOOK AT ONLY PS
tmp<-subset(tmp,species=="PDSG")
## FIT ln(weight) as a linear model of ln(length) 
## WHICH ASSUMES w=C*l^{beta_1}... WOULD WE EXPECT A CERTAIN VALUE FOR beta_1 BASED ON DEB???
tmp$llength<-log(tmp$length)
tmp$lweight<-log(tmp$weight)
fit<-lm(lweight~llength,tmp)
## REMOVE OUTLIERS BASED ON RESIDUALS....TAKE A LOOK AT!!! 
tmp$pred<- exp(fitted(fit))
tmp$residual<- resid(fit)
tmp<- subset(tmp, residual> -1.5)
tmp<- subset(tmp, residual< 1.5)
## REMOVE VERY SMALL FISH (ASSUME AGE-0, AND THEREFORE FALL INTO CATEGORY OF RECRUITS)
tmp<- subset(tmp, length>=200)
## USE ONLY THE PAST 2 YEARS OF DATA (2015 & 2016...2017???)
tmp<- subset(tmp, year>=2015)



#
out<-data.frame()
quantiles<- seq(0,100,by=1)/100
for(seg in unique(tmp$segment_id))
{
  q<-quantile(tmp[tmp$segment_id==seg,]$length, quantiles,
              na.rm = TRUE)
  app<- data.frame(segment=seg,quantile=quantiles,val=q)
  out<- rbind(out,app)
}

out<-out[order(out$segment, out$quantile),]
write.csv(out,"_output/length_inputs.csv")

head(out)


library(lattice)
presPlot()
out<-subset(out,segment %in% c(2,3,4,7,8,9,10,13,14))
out$segment<- factor(out$segment) 
xyplot(quantile~val,
       data=out,lwd=3,
       groups=segment,
       subset= segment %in% c(2,3,4,7,8,9,10,13,14),
       type='l',
       xlab="Length",
       ylab="Quantile",
       auto.key=list(columns = 1,
                     title="Segment",size=3,
                     space="right",
                     lines=TRUE,
                     points=FALSE))

