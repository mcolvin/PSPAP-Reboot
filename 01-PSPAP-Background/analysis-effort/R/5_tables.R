common<-UBcommon<-c("GN14","GN41","MF","OT16","TLC1","TLC2","TN")
LBcommon<-c("GN18","GN81","TLC4","TLS1","TLS2")
dat$gearC<-as.factor(unlist(lapply(dat$gear, function(x) 
{
  ifelse(any(common==x), x<-paste0(x,"*"),
         ifelse((dat$basin=="LB" && any(LBcommon==x))||(dat$basin=="UB" && x=="TLO1"),x<-paste0(x,"*"),
                unlist(levels(dat$gear))[x])
  )
}
)))
datS<-subset(dat,standard_gear=="yes")

tables<- function(n)
{
  if(n=="f1")
  {# TABLE OF GEAR USE OVER TIME BY BASIN FOR FIGURE 1
    dat$tmp<-1 # TO SUM FOR COUNTS
    tmp<- dcast(dat, basin+gear+gear_id+yr~"freq",value.var="tmp",sum)
    return(tmp)
  }
  if(n==1)
  {
    dat$tmp<-1 # TO SUM FOR COUNTS
    tmp<- dcast(dat, basin+gearC+gear_id~yr,value.var="tmp",sum)
    return(tmp)      
  }
  if(n==2)
  {
    datS$tmp<-1 # TO SUM FOR COUNTS
    tmp<-dcast(datS, basin+segment_id+bend+gearC+gear_id~yr,value.var="tmp",sum)
    return(tmp)      
  }
  if(n==3)
  {
    datS$tmp<-1 # TO SUM FOR COUNTS
    tmp<-dcast(datS, basin+segment_id+bend+gearC+gear_id~yr,value.var="tmp",sum)
    tmp$min<-apply(tmp[,6:19], 1, FUN=min)
    tmp$max<-apply(tmp[,6:19], 1, FUN=max)
    tmp$mean<-round(apply(tmp[,6:19], 1, FUN=mean),1)
    tmp$median<-apply(tmp[,6:19], 1, FUN=median)
    tmp$deployments<-apply(tmp[,6:19], 1, FUN=sum)
    tmp<-tmp[,c(1:5,20:24)]
    return(tmp)      
  }
  if(n==4)
  {
    #FIND STANDARD LB AND UB DATA
    datLB<-subset(datS, basin=="LB")
    datUB<-subset(datS, basin=="UB")
    #BUILD A TABLE OF EFFORT DATA FOR EACH GEAR BY BASIN
    datS$tmp<-1 #TO SUM FOR COUNTS
    eft<-dcast(datS, basin+gear+gear_id~"observations", value.var="tmp", sum)
    eft$mean_effort<-c(round(aggregate(datLB$effort,by=list(datLB$gear),mean)[,2]),round(aggregate(datUB$effort,by=list(datUB$gear),mean)[,2]))
    eft$sd_effort<-c(round(aggregate(datLB$effort,by=list(datLB$gear),sd)[,2]),round(aggregate(datUB$effort,by=list(datUB$gear),sd)[,2]))
    eft$min_effort<-c(aggregate(datLB$effort,by=list(datLB$gear),min)[,2],aggregate(datUB$effort,by=list(datUB$gear),min)[,2])
    eft$max_effort<-c(aggregate(datLB$effort,by=list(datLB$gear),max)[,2],aggregate(datUB$effort,by=list(datUB$gear),max)[,2])
    eft$median_effort<-c(round(aggregate(datLB$effort,by=list(datLB$gear),median)[,2]),round(aggregate(datUB$effort,by=list(datUB$gear),median)[,2]))
    #Requires Running Other Code (WILL WORK ON)
    eft$gamma_shape<-shapes
    eft$gamma_rate<-rates
    eft<-subset(eft,observations>=10)
    return(eft)
  }
}