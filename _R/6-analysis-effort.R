

###########################
# FIT GAMMA DISTRIBUTIONS #
###########################
## 1.  
dfitfunLB<-function(x)
{
  datLBgear<-subset(datLB, gear==LBgears[x])
  dfit<-fitdistr(datLBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}

## 2.
dfitfunUB<-function(x)
{
  datUBgear<-subset(datUB, gear==UBgears[x])
  dfit<-fitdistr(datUBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}


# FIND STANDARD LOWER & UPPER BASIN GEARS
datLB<-subset(dat, standard_gear=="yes" & basin=="LB")
dim(datLB)
LBgears<-unlist(lapply(unlist(levels(datLB$gear)), function(x) 
{
  lg<-subset(datLB, gear==x)
  if(nrow(lg)!=0) return(x)
}
))

datUB<-subset(dat, standard_gear=="yes" & basin=="UB")
dim(datUB)
UBgears<-unlist(lapply(unlist(levels(datUB$gear)), function(x) 
{
  lg<-subset(datUB, gear==x)
  if(nrow(lg)!=0) return(x)
}
))

# MAKE A LIST OF DISTRIBUTION PARAMETERS
valuesLB<-unlist(lapply(c(1:12,15,16,18), dfitfunLB))
shapesLB<-c(valuesLB[(2*c(1:length(LBgears))-1)])
ratesLB<-c(valuesLB[(2*c(1:length(LBgears)))])

valuesUB<-unlist(lapply(1:length(UBgears), dfitfunUB))
shapesUB<-c(valuesUB[(2*c(1:length(UBgears))-1)])
ratesUB<-c(valuesUB[(2*c(1:length(UBgears)))])

shapes<-c(shapesLB[1:12],NA,NA,shapesLB[13:14],NA,shapesLB[15],shapesUB)
rates<-c(ratesLB[1:12],NA,NA,ratesLB[13:14],NA,ratesLB[15],ratesUB)


##################################
# CODE USED IN WRITE UP ANALYSIS #
##################################  
run_code<-FALSE #KEEP FROM RUNNING DURING SOURCING
if(run_code==TRUE)
{
  #MIN AND MAX DEPLOYMENTS OF A GEAR BY BASIN OVER ENTIRE PSPAP
  max(tables(4)$observations)
  which.max(tables(4)$observations)
  tables(4)[20,]
  
  min(tables(4)$observations)
  which.min(tables(4)$observations)
  tables(4)[19,]
  
  max(tables(4)$observations[1:12])
  which.max(tables(4)$observations[1:12])
  tables(4)[5,]
  
  min(tables(4)$observations[1:12])
  which.min(tables(4)$observations[1:12])
  tables(4)[9,]
  
# LOOK AT DATA BY BEND FOR ALL STANDARD GEARS COMBINED
  datS<-subset(dat,standard_gear=="yes")
  datS$tmp2<-1 # TO SUM FOR COUNTS
  tmp2<-dcast(datS, basin+segment_id+bend~yr,value.var="tmp2",sum)
  tmp2$min<-apply(tmp2[,4:17], 1, FUN=min)
  tmp2$max<-apply(tmp2[,4:17], 1, FUN=max)
  tmp2$mean<-round(apply(tmp2[,4:17], 1, FUN=mean),1)
  tmp2$median<-apply(tmp2[,4:17], 1, FUN=median)
  tmp2$deployments<-apply(tmp2[,4:17], 1, FUN=sum)
  tmp2<-tmp2[,c(1:3,18:22)]
  head(tmp2)
  
  #NO BEND WAS SAMPLED EVERY YEAR
    max(tmp2$min)
  
  #BENDS WITH MINIMUM SAMPLING
    min(tmp2$deployments)
    which.min(tmp2$deployments)
    tmp2[which(tmp2$deployments==1),]
    
      #FIND THE GEAR
      subset(tables(2),segment_id==13 & bend==45)
      subset(tables(2),segment_id==28 & bend==5)
      subset(tables(2),segment_id==15 & bend==21)
      subset(tables(2),segment_id==21 & (bend==2 | bend==3))
      
    #FIND THE BEND WITH MOST DEPLOYMENTS
    max(tmp2$deployments)
    which.max(tmp2$deployments)
    tmp2[484,]
    
    max(tmp2$deployments[1:12])
    which.max(tmp2$deployments[1:12])
    tmp2[4,]
    
#GEAR BY BEND
min(tables(3)$deployments)
which(tables(3)$deployments==1)

max(tables(3)$max)
tables(3)[which(tables(3)$max==204),]

max(tables(3)$max[1:1594])
tables(3)[which(tables(3)$max[1:1594]==32),]

max(tables(3)$deployments)
tables(3)[which(tables(3)$deployments==502),]

max(tables(3)$deployments[1:1594])
tables(3)[which(tables(3)$deployments[1:1594]==127),]


# EFFORT
min(tables(4)$mean_effort)
tables(4)[which(tables(4)$mean_effort==3),]
min(tables(4)$median_effort)
tables(4)[which(tables(4)$median_effort==2),]
  
min(tables(4)$mean_effort[13:20])
tables(4)[which(tables(4)$mean_effort==4),]
min(tables(4)$median_effort[13:20])
tables(4)[which(tables(4)$median_effort==4),]

max(tables(4)$mean_effort)
tables(4)[which(tables(4)$mean_effort==1336),]
max(tables(4)$median_effort)
tables(4)[which(tables(4)$median_effort==1341),]

max(tables(4)$mean_effort[13:20])
tables(4)[which(tables(4)$mean_effort==1182),]
max(tables(4)$median_effort[13:20])
tables(4)[which(tables(4)$median_effort==1180),]
}  
  
  

