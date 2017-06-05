
tables_effort<- function(n,dat=dat,...)
{
## EFFORT ANALYSIS
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
    datS<-subset(dat,standard_gear=="yes")
    datS$tmp<-1 # TO SUM FOR COUNTS
    tmp<-dcast(datS, basin+segment_id+bend+gearC+gear_id~yr,value.var="tmp",sum)
    return(tmp)      
    }
  if(n==3)
    {
    datS<-subset(dat,standard_gear=="yes")
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
    datS<-subset(dat,standard_gear=="yes")
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


tables<- function(n){

if(n==1)
    {
    # BEND SUMMARY TABLE
    ## THIS TABLE SUMMARIZES THE SAMPLING UNITS
    ## FOR BENDS IN RPMAN 2 (UPPER) AND 4 (LOWER)
    tmp<-ddply(bends,.(basin,b_segment),summarize,
        n_bends=length(id),
        min_rkm=min(length.rkm),
        mean_rkm=min(length.rkm),
        maximum_rkm=min(length.rkm))     
    return(format(tmp,digits=2))
    }
if(n==2)
  {
  tmp<-ddply(dens,.(rpma, segments, fish_type),summarize,
            min_dens=min(d_lower, na.rm=TRUE),
            mean_dens=density_mean[which.max(as.numeric(year))],
            max_dens=max(d_upper, na.rm=TRUE),
            max_year=max(as.numeric(year)),
            min_mean=min(density_mean, na.rm=TRUE),
            max_mean=max(density_mean, na.rm=TRUE))
  tmp$min_dens<-mapply(min, tmp$min_dens, tmp$min_mean, na.rm=TRUE)
  tmp$max_dens<-mapply(max, tmp$max_dens, tmp$max_mean, na.rm=TRUE)
  tmp$segs<-1
  tmp$segs[which(tmp$segments=="8")]<-8
  tmp$segs[which(tmp$segments=="10")]<-10
  tmp<-tmp[order(tmp$segs),]
  tmp$segments<-ifelse(tmp$segments=="1, 2, 3, 4, LY", "1-4", ifelse(tmp$segments=="8", "7-9", "   10, 13, 14"))
  tmp<-tmp[,1:7]
  tmp$ref<-c("[1]", "[2]-[4]", "[5]", "[5]", "[6]", "[6]")
  return(tmp)
  }

}