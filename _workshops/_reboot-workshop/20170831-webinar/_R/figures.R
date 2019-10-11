

#######################################################################
#
#  MOVEMENT PLOTS
#
#######################################################################
bends<- read.csv("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/_dat/bend-data.csv")
names(bends)<- tolower(names(bends))
bends<- subset(bends,rpma==4)
bends$rkm<- bends$bend_start_rkm+ bends$length.rkm/2

pd<- abs(outer(bends$rkm,bends$rkm,"-"))


## Rulo Bend
### LOW FIDELITY
indx<- which(bends$b_desc=="Rulo Bend")
xx<-c(pd[indx,])
B0<-B1<- -0.05 
y<- exp(B0+B1*xx)
y[indx]<-1
yy<-y/sum(y)
plot(yy~bends$rkm,type='l')

# MORE FIDELITY
B0<-B1<- -0.5 
y<- exp(B0+B1*xx)
y[indx]<-1
yy<-y/sum(y)
points(yy~bends$rkm,type='l')



B0<- seq(-100,-100,by=0.5)
B1<- seq(-100,100,by=1)
betas<-expand.grid(B0,B1)
p<-lapply(1:nrow(betas),function(x)
    {
    y<- betas[x,1]+betas[x,2]*xx
    y[which(xx==0)]<-0
    return(exp(y)/sum(exp(y)))
    })
p<-as.matrix(do.call("rbind",p))
matplot(xx,t(p),type='l')

library(rgdal)
watershed<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "missouri_ws")	
# LOAD PROCESSED US POLYGON
us<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "states_dissolve")	
# LOAD DATA PALLID STURGEON MANAGEMENT UNITS
manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "PallidSturgeon_ManagementUnits")
# HATCHERY DATA
chan<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
hatcheries<- sqlFetch(chan, "Hatchery_latlongs")
## LOAD PROCESSED MAJOR WATERBODIES
reservoirs<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "reservoirs")    
## LOAD BEND DATA 
bends<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "bends_sp_ll")
    
    
    
#######################################################################
# PLOT WATERSHED AREA   
#######################################################################
library(data.table)
library(ggmap)
moRivBasin<-fread("moRiverDrainage_raw.txt",sep=",",data.table=FALSE)
names(moRivBasin)<- c('x','y','id')
moRivBasin$id<-1
# TRANSPARENT RED
tred<-rgb(1,0,0,0.4)
# MAKE THE BOUNDING BOX
sbbox <- make_bbox(lon = moRivBasin$x, lat = moRivBasin$y, f = .1)
# PULL THE MAP GIVEN THE BOUNDING BOX
myMap <- get_map(location=sbbox,source="google", maptype="terrain")
ggmap(myMap) + geom_polygon(data = moRivBasin, 
    mapping = aes(x = x, y = y), 
   alpha = .5, color="darkred",fill="darkred") +
	theme ( 
		panel.grid.major = element_blank (), # remove major grid
		panel.grid.minor = element_blank (),  # remove minor grid
		axis.text = element_blank (), 
		axis.title = element_blank (),
		axis.ticks = element_blank ()) 


    
#######################################################################   
# PLOT THE MAIN RIVER   
#######################################################################
manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
    "PallidSturgeon_ManagementUnits")
xx<-lapply(1:nrow(manunits),function(x){
    unit<-as.character(manunits$Unit[x])
    name<-as.character(manunits$Name[x])
    out<-coordinates(manunits)[[x]]
    out<-as.data.frame(do.call("rbind",out))
    names(out)<- c("long","lat")    
    out$unit<-unit
    out$name<-name
    return(out)
    })   
xx<-do.call("rbind",xx) 
xx<-subset(xx, name!="Coastal Plains")
xx$group<-1   

ggmap(myMap) + geom_polygon(data = moRivBasin, 
    mapping = aes(x = x, y = y), 
   alpha = .5, color="darkred",fill="darkred") +
   geom_point(data=xx, mapping=aes(x=long, y=lat, group=group),
    col="darkblue",cex=0.75)+
	theme ( 
		panel.grid.major = element_blank (), # remove major grid
		panel.grid.minor = element_blank (),  # remove minor grid
		axis.text = element_blank (), 
		axis.title = element_blank (),
		axis.ticks = element_blank ())   
savePlot("big-river.tif",type="tif")
    
#######################################################################    
# ADD RESERVOIRS
#######################################################################
reservoirs<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "reservoirs") 
#install.packages("broom")    
ggmap(myMap) + 
    geom_polygon(data = moRivBasin, 
        mapping = aes(x = x, y = y), 
        alpha = .5, color="darkred",fill="darkred") +
    geom_point(data=xx, mapping=aes(x=long, y=lat, group=group),
        col="darkblue",cex=0.5) +
    geom_polygon(data=fortify(res),mapping = aes(x = long, y = lat,group=group), 
        alpha = 1, color="lightblue",fill="lightblue",cex=1.2) +
	theme ( 
		panel.grid.major = element_blank (), # remove major grid
		panel.grid.minor = element_blank (),  # remove minor grid
		axis.text = element_blank (), 
		axis.title = element_blank (),
		axis.ticks = element_blank ()) 
savePlot("big-river-reservoirs.tif",type="tif")



    
# ADD RPMA 2 AND 4    
bends<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    "bends_sp_ll")

ggmap(myMap) + 
    geom_polygon(data = moRivBasin, 
        mapping = aes(x = x, y = y), 
        alpha = .5, color="darkred",fill="darkred") +
    geom_point(data=xx, mapping=aes(x=long, y=lat, group=group),
        col="lightblue",cex=0.5) +  
    geom_polygon(data=fortify(res),mapping = aes(x = long, y = lat,group=group), 
        alpha = 1, color="lightblue",fill="lightblue",cex=1.2) +        
    geom_point(data=fortify(bends), mapping=aes(x=long, y=lat, group=group),
        col="darkblue")+
	theme ( 
		panel.grid.major = element_blank (), # remove major grid
		panel.grid.minor = element_blank (),  # remove minor grid
		axis.text = element_blank (), 
		axis.title = element_blank (),
		axis.ticks = element_blank ()) 

        
savePlot("big-river-reservoirs-rpmas.tif",type="tif")   
    
    
#######################################################################
# DESIGN PLOTS
#######################################################################    
library(data.table) 
dat_a<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/abund_table.csv")   
dat_t<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/trnd_table.csv")   
library(reshape2)
library(plyr)
xx<- ddply(dat_a, .(estimator,gear,ref_id),summarize,
    performance=mean(perform,na.rm=TRUE),
    bias = mean(bias,na.rm=TRUE),
    cv=mean(cv,na.rm=TRUE))
xxx<- ddply(xx, .(estimator,gear),summarize,
    performance=mean(performance,na.rm=TRUE),
    bias = mean(bias,na.rm=TRUE),
    cv=mean(cv,na.rm=TRUE))


dat<- as.data.frame(subset(xx,gear=="GN14")    )
    
# Histogram Colored (blue and red)

## ABUNDANCE: BIAS
brks<- seq(min(na.omit(dat$bias)),max(na.omit(dat$bias)), by=100)
d1<-hist(dat[which(dat$estimator=="CPUE_AM"),]$bias,breaks=brks,plot=F)
d2<-hist(dat[which(dat$gear=="GN14" & dat$estimator=="Mt_AM"),]$bias,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
presPlot()
plot(d2$density~d2$mid,xlim=c(-5500,300),type='n',
    xlab="Bias",ylab="Probability")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("abund-bias.tif",type="tif")   
dev.off()

## ABUNDANCE: PRECISION
brks<- seq(0,max(na.omit(dat$cv)+0.1), by=0.005)
d1<-hist(dat[which(dat$estimator=="CPUE_AM"),]$cv,breaks=brks,plot=F)
d2<-hist(na.omit(dat[which(dat$estimator=="Mt_AM"),]$cv),breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
presPlot()
plot(d1$density~d1$mid,xlim=c(0,0.35),type='n',
    xlab="Precision",ylab="Probability")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("abund-cv.tif",type="tif")   
dev.off()


## ABUNDANCE: PERFORMANCE
brks<- seq(0,1, by=0.005)
d1<-hist(dat[which(dat$gear=="GN14" & dat$estimator=="CPUE_AM"),]$performance,breaks=brks,plot=F)
d2<-hist(dat[which(dat$gear=="GN14" & dat$estimator=="Mt_AM"),]$performance,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
presPlot()
plot(d1$density~d2$mid,xlim=c(0,1),type='n',
    xlab="Performance",ylab="Probability")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("abund-performance.tif",type="tif")   
dev.off()



dat<- as.data.frame(subset(dat_t,gear=="GN14")   )


## TREND: BIAS
brks<- seq(min(na.omit(dat$bias))-0.1,max(na.omit(dat$bias))+0.1, by=0.1)
d1<-hist(dat[which(dat$estimator=="CPUE"),]$bias,breaks=brks,plot=F)
d2<-hist(dat[which(dat$estimator=="Mt_AM"),]$bias,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
presPlot()
plot(d2$density~d2$mid,xlim=c(-1,1),type='n',
    xlab="Bias",ylab="Probability")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("trend-bias.tif",type="tif")   
dev.off()

## TREND: PRECISION
brks<- seq(0,max(na.omit(dat$cv)), by=0.005)
d1<-hist(dat[which(dat$estimator=="CPUE"),]$cv,breaks=brks,plot=F)
d2<-hist(dat[which(dat$estimator=="Mt_AM"),]$cv,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
presPlot()
plot(d2$density~d2$mid,xlim=c(0,0.35),type='n',
    xlab="Precision",ylab="Probability")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("trend-cv.tif",type="tif")   
dev.off()


## TREND: PERFORMANCE
brks<- seq(0,1, by=0.005)
d1<-hist(xx[which(dat$estimator=="CPUE"),]$performance,breaks=brks,plot=F)
d2<-hist(xx[which(dat$estimator=="Mt_AM"),]$performance,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
presPlot()
plot(d1$density~d1$mid,xlim=c(0.9,1),type='n',
    xlab="Performance",ylab="Probability")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("trend-performance.tif",type="tif")   
dev.off()

#######################################################################
## SCALE OUPUTS
### ABUNDANCE
#######################################################################
library(data.table) 
dat_a<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/abund_table.csv")   
dat_t<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/trnd_table.csv")   
library(reshape2)
library(plyr)
xx<- ddply(dat_a, .(estimator,gear,ref_id),summarize,
    performance=mean(perform,na.rm=TRUE),
    bias = mean(bias,na.rm=TRUE),
    cv=mean(cv,na.rm=TRUE))
dat<- as.data.frame(subset(xx,gear=="GN14")   )
dat<- as.data.frame(subset(dat,gear=="GN14")    )
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
dat$bias_sc<- utility(dat$bias,absolute=TRUE)
dat$cv_sc<- utility(dat$cv)
dat$performance_sc<- utility(dat$performance,best="high")



presPlot()
nf <- layout(matrix(c(1,2,3,4),
    4,1,byrow = TRUE), 
    heights=c(3,1,1,1), 
    widths=c(1), 
    respect=FALSE)
# MAIN
xlimbias<- c(-5500,300)
xlimcv<- c(0,0.35)
xlimperformance<-c(0,1)
par(mar=c(1,2,1,1))
plot(bias_sc~bias,dat,type='n',
    xaxt='n',las=1)
par(new=TRUE)
x<- seq(xlimbias[1],
    xlimbias[2],
    length.out=1000)
plot(x,utility(x,absolute=TRUE),type='l',lwd=3,
    xaxt='n',yaxt='n',ylim=c(0,1),
    xlim=xlimbias)
par(new=TRUE)
x<- seq(range(na.omit(dat$cv))[1],
    range(na.omit(dat$cv))[2],
    length.out=1000)
plot(x,utility(x,absolute=FALSE),type='l',lwd=3,
    xaxt='n',yaxt='n',ylim=c(0,1),
    xlim=xlimcv,col="green")
par(new=TRUE)
x<- seq(range(na.omit(dat$performance))[1],
    range(na.omit(dat$performance))[2],
    length.out=1000)
plot(x,utility(x,absolute=FALSE,best="high"),type='l',lwd=3,
    xaxt='n',yaxt='n',ylim=c(0,1),
    xlim=xlimperformance,col="blue")    

    
# SUB 1
par(mar=c(1,2,1,1))
## ABUNDANCE: BIAS
brks<- seq(min(na.omit(dat$bias)),max(na.omit(dat$bias)), by=100)
d1<-hist(dat[which(dat$estimator=="CPUE_AM"),]$bias,breaks=brks,plot=F)
d2<-hist(dat[which(dat$gear=="GN14" & dat$estimator=="Mt_AM"),]$bias,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
plot(d2$density~d2$mid,xlim=xlimbias,type='n',
    xlab="Bias",ylab="",las=1)
polygon(x=c(d1$mid,rev(d1$mid)),
    y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),
    y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))

    
## ABUNDANCE: PRECISION
par(mar=c(1,2,1,1))
brks<- seq(0,max(na.omit(dat$cv)+0.1), by=0.005)
d1<-hist(dat[which(dat$estimator=="CPUE_AM"),]$cv,breaks=brks,plot=F)
d2<-hist(na.omit(dat[which(dat$estimator=="Mt_AM"),]$cv),breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
plot(d1$density~d1$mid,xlim=xlimcv,type='n',
    xlab="Precision",ylab="Probability",las=1,xaxt='n')
axis(side=1,at=axTicks(1),col="green",col.axis="green")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))

## ABUNDANCE: PERFORMANCE
par(mar=c(1,2,1,1))
brks<- seq(0,1, by=0.005)
d1<-hist(dat[which(dat$gear=="GN14" & dat$estimator=="CPUE_AM"),]$performance,breaks=brks,plot=F)
d2<-hist(dat[which(dat$gear=="GN14" & dat$estimator=="Mt_AM"),]$performance,breaks=brks,plot=F)
d1$density<-d1$density/sum(d1$density)
d2$density<-d2$density/sum(d2$density)
plot(d1$density~d2$mid,xlim=xlimperformance,type='n',
    xlab="Performance",ylab="Probability",las=1,xaxt='n')
axis(side=1,at=axTicks(1),col="blue",col.axis="blue")
polygon(x=c(d1$mid,rev(d1$mid)),y=c(d1$density,rep(0,length(d1$density))),
    col=rgb(0,0,1,0.65))
polygon(x=c(d2$mid,rev(d2$mid)),y=c(d2$density,rep(0,length(d2$density))),
    col=rgb(1,0,0,0.65))
savePlot("combine-abundance.tif",type="tif")   
dev.off()
    
# TREND 
dat<- as.data.frame(subset(dat_t,gear=="GN14")   )



# COMBINED UTITLITIES
library(data.table) 
dat_a<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/abund_table.csv")   
dat_t<-fread("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/output/trnd_table.csv")   
qvals<- range(c(dat_t[which(dat_t$gear=="GN14"),]$mean_q_input,dat_a[which(dat_a$gear=="GN14"),]$mean_q_input))
brks<- seq(qvals[1],qvals[2],length.out=4)
dat_a$q_bin<- cut(dat_a$mean_q_input,breaks=brks,labels=c("low","moderate","high"))
dat_t$q_bin<- cut(dat_t$mean_q_input,breaks=brks,labels=c("low","moderate","high"))


library(reshape2)
library(plyr)
xx<- ddply(dat_a, .(estimator,gear,ref_id),summarize,
    performance=mean(perform,na.rm=TRUE),
    bias = mean(bias,na.rm=TRUE),
    cv=mean(cv,na.rm=TRUE))
   


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
