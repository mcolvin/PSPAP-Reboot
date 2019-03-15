utility<- function(x,absolute=FALSE,best="low")
    {
    if(best=="low")
        {
        if(absolute==FALSE)
            {
            mx<-max(na.omit(x))
            mn<-min(na.omit(x))
            V<-(mx-x)/(mx-mn)
            }    
        if(absolute==TRUE)
            {
            mx<-max(abs(na.omit(x)))
            mn<-min(abs(na.omit(x)))
            V<-(mx-abs(x))/(mx-mn)
            }
        }
    if(best=="high")
        {
        if(absolute==FALSE)
            {
            mx<-max(na.omit(x))
            mn<-min(na.omit(x))
            V<-(x-mn)/(mx-mn)
            }    
        if(absolute==TRUE)
            {
            mx<-max(abs(na.omit(x)))
            mn<-min(abs(na.omit(x)))
            V<-(abs(x)-mn)/(mx-mn)
            }
        }
    return(V)}

library(reshape2)
library(plyr) 
library(data.table)   

#######################################################################
# GENERATE CPT
#######################################################################    
dat_a<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/abund_table.csv")   
dat_a<- subset(dat_a,(gear%in%c("GN14","TLC1","TN","OT16") &
        estimator %in% c("CPUE_AM","Mt_AM")))
xx<- ddply(dat_a, .(estimator,gear,ref_id),summarize,
    mean_q_input =mean(mean_q_input,na.rm=TRUE),
    B0_sd_input=mean(B0_sd_input,na.rm=TRUE),
    performance=mean(perform,na.rm=TRUE),
    bias = mean(bias,na.rm=TRUE),
    cv=mean(cv,na.rm=TRUE))
xx$tmp<-1
dat<-xx
## NORMALIZE CATCHABILITY FOR EACH GEAR
dat$q_norm<-0
for(i in unique(dat$gear))
    {
    dat$q_norm[which(dat$gear==i)]<- utility(dat$mean_q_input[which(dat$gear==i)],best="high")
    }
dat$q_bin<- 1
dat$q_bin[which(dat$q_norm>0.33)]<- 2       
dat$q_bin[which(dat$q_norm>0.66)]<- 3       




## FORMAT ABUNDANCE: SD Q
dat$qsd_norm<- utility(dat$B0_sd_input,best="high")
dat$qsd_bin<- 1
dat$qsd_bin[which(dat$qsd_norm>0.33)]<- 2       
dat$qsd_bin[which(dat$qsd_norm>0.66)]<- 3       


## FORMAT ABUNDANCE: BIAS
dat$bias_sc<- utility(dat$bias,absolute=TRUE,best="low")
dat$bias_bin<- 1
dat$bias_bin[which(dat$bias_sc>0.33)]<- 2       
dat$bias_bin[which(dat$bias_sc>0.66)]<- 3       
pp<-dcast(dat,
    q_bin+qsd_bin+estimator+ gear~bias_bin,
    value.var="tmp",sum)
write.csv(pp,"abund_bias_cpt.csv")

## FORMAT ABUNDANCE: CV
dat$cv_sc<- utility(dat$cv)
dat$cv_bin<- 1
dat$cv_bin[which(dat$cv_sc>0.33)]<- 2       
dat$cv_bin[which(dat$cv_sc>0.66)]<- 3       
dat$tmp<-1
pp<-dcast(dat,
    q_bin+qsd_bin+estimator+ gear~cv_bin,
    value.var="tmp",sum)
write.csv(pp,"abund_cv_cpt.csv")
 

## FORMAT ABUNDANCE: PERFORMANCE
dat$performance_sc<- utility(dat$performance,best="high")
dat$perf_bin<- 1
dat$perf_bin[which(dat$performance_sc>0.33)]<- 2       
dat$perf_bin[which(dat$performance_sc>0.66)]<- 3       
dat$tmp<-1
pp<-dcast(dat,
    q_bin+qsd_bin+estimator+ gear~perf_bin,
    value.var="tmp",sum)
write.csv(pp,"abund_perform_cpt.csv")







## TREND
dat_t<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/trnd_table.csv")   
dat_t<- subset(dat_t,(gear%in%c("GN14","TLC1","TN","OT16") &
        estimator %in% c("CPUE","Mt_AM")))
## NORMALIZE CATCHABILITY FOR EACH GEAR
qvals<- ddply(dat_t, .(gear),summarize,
        l1= min(mean_q_input),
        l2= max(mean_q_input))
dat_t$q_norm<-0
for(i in unique(dat_t$gear))
    {
    dat_t$q_norm[which(dat_t$gear==i)]<- utility(dat_t$mean_q_input[which(dat_t$gear==i)],best="high")
    }
dat_t$q_bin<- 1
dat_t$q_bin[which(dat_t$q_norm>0.33)]<- 2       
dat_t$q_bin[which(dat_t$q_norm>0.66)]<- 3       

## FORMAT TREND: SD Q
dat_t$qsd_norm<- utility(dat_t$B0_sd_input,best="high")
dat_t$qsd_bin<- 1
dat_t$qsd_bin[which(dat_t$qsd_norm>0.33)]<- 2       
dat_t$qsd_bin[which(dat_t$qsd_norm>0.66)]<- 3       

## FORMAT TREND: BIAS
dat_t$bias_sc<- utility(dat_t$bias,absolute=TRUE,best="low")
dat_t$bias_bin<- 1
dat_t$bias_bin[which(dat_t$bias_sc>0.33)]<- 2       
dat_t$bias_bin[which(dat_t$bias_sc>0.66)]<- 3       
pp<-dcast(dat_t,
    q_bin+qsd_bin+estimator+ gear~bias_bin,
    value.var="tmp",sum)
write.csv(pp,"trend_bias_cpt.csv")

## FORMAT TREND: CV
dat_t$cv_sc<- utility(dat_t$cv)
dat_t$cv_bin<- 1
dat_t$cv_bin[which(dat_t$cv_sc>0.33)]<- 2       
dat_t$cv_bin[which(dat_t$cv_sc>0.66)]<- 3       
dat_t$tmp<-1
pp<-dcast(dat_t,
    q_bin+qsd_bin+estimator+ gear~cv_bin,
    value.var="tmp",sum)
write.csv(pp,"trend_cv_cpt.csv")
 


## FORMAT TREND: PERFORMANCE
dat_t$performance_sc<- utility(dat_t$perform,best="high")
dat_t$perf_bin<- 1
dat_t$perf_bin[which(dat_t$performance_sc>0.33)]<- 2       
dat_t$perf_bin[which(dat_t$performance_sc>0.66)]<- 3       


dat_t$tmp<-1
pp<-dcast(dat_t,
    q_bin+qsd_bin+estimator+ gear~perf_bin,
    value.var="tmp",sum,
    subset=.(gear%in%c("GN14","TLC1","TN","OT16") &
        estimator %in% c("CPUE","Mt_AM")))
write.csv(pp,"trend_perform_cpt.csv")
 
 
 
 
 
dat$bias_sc<- utility(dat$bias,absolute=TRUE)
dat$cv_sc<- utility(dat$cv)
dat$performance_sc<- utility(dat$performance,best="high")
dat$U<- 0.5*dat$bias_sc+0.25*dat$cv_sc+0.25*dat$performance_sc


## TREND
dat_t<- as.data.frame(subset(dat_t,gear=="GN14")   )
dat_t$bias_sc<- utility(dat_t$bias,absolute=TRUE)
dat_t$cv_sc<- utility(dat_t$cv)
dat_t$performance_sc<- utility(dat_t$perform,best="high")
dat_t$U<- 0.5*dat_t$bias_sc+0.25*dat_t$cv_sc+0.25*dat_t$performance_sc


## MEAN
U1<-aggregate(U~estimator,dat, mean)
U2<-aggregate(U~estimator,dat_t, mean)
  
U1<-subset(U1,estimator%in%c("CPUE_AM","Mt_AM"))  
U2<-subset(U2,estimator%in%c("CPUE","Mt_AM"))

UU<- U1$U*0.5+U2$U*0.5
barplot(UU,horiz=TRUE,xlim=c(0,1),xlab="Expected Utility");box()



## BINNED Q
xxq<- ddply(dat_a, .(estimator,gear,ref_id,q_bin),summarize,
    performance=mean(perform,na.rm=TRUE),
    bias = mean(bias,na.rm=TRUE),
    cv=mean(cv,na.rm=TRUE)) 
dat<- as.data.frame(subset(xxq,gear=="GN14")   )
dat$bias_sc<- utility(dat$bias,absolute=TRUE)
dat$cv_sc<- utility(dat$cv)
dat$performance_sc<- utility(dat$performance,best="high")
dat$U<- 0.5*dat$bias_sc+0.25*dat$cv_sc+0.25*dat$performance_sc
   
U1Q<-aggregate(U~estimator+q_bin,dat, mean)    
U1Q<-subset(U1Q,estimator%in%c("CPUE_AM","Mt_AM"))  
 
U2Q<-aggregate(U~estimator+q_bin,dat_t, mean)
U2Q<-subset(U2Q,estimator%in%c("CPUE","Mt_AM"))

U1Q$UU<-U1Q$U*0.5+U2Q$U*0.5
U1Q$id<-c(1L,1L,2L,2L,3L,3L)
presPlot()
plot(UU~id,data=U1Q,subset=estimator=="CPUE_AM",type='b',pch=19,
    ylim=c(0.75,1),col="blue",ylab="",xlab="Catchability",
    xaxt="n",lwd=3,las=1)
axis(side=1,at=c(1,2,3),labels=c("Low","Medium","High"))
mtext(side=2,"Expected Utility",line=4,cex=1.5)
points(UU~id,U1Q,subset=estimator=="Mt_AM",type='b',pch=19,
    col="red",lwd=3)
