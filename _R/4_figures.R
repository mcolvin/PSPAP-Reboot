figures_study_area<- function(n,...){

if(n==1){
    # ENTIRE STUDY AREA
	plot(manunits,col="black",lwd=1)
	plot(bends,col="black",lwd=2,add=TRUE)	
	plot(reservoirs,add=TRUE,col="grey",border="grey",lwd=2.5)		
	points(-104.54,47.28,pch=19,cex=1)          # INTAKE
	points(-106.422049, 48.001600,pch=19, cex=1)# FORT PECK DAM	
	points(-101.411601,47.498528,pch=19,cex=1)  # GARRISON DAM		
	points(-100.402227, 44.451274,pch=19,cex=1) # OAHE DAM	
	points(-99.448768,44.049215,pch=19,cex=1)   # BIG BEND DAM	
	points(-98.562062,43.059483,pch=19,cex=1)   # FORT RANDALL DAM
	points(-97.485244,42.860696,pch=19,cex=1)   # GAVINS POINT DAM
	map.scale(y=42,ratio=FALSE)
	arrows(-109.5,43,-109.5,45,lwd=3,length=0.15)
	text(-109.5,42.5,"N",cex=1.2)
	par(xpd=TRUE)	
	segments(-90.120752, 38.813236, -90.120752, 50,lwd=2)
	segments(-97.485244,42.860696,-97.485244,50,lwd=2) # GAVINS POINT DAM
	segments(-103.5,48.001600,-103.5,50,lwd=2)	
	segments(-106.422049,48.001600,-106.422049,50,lwd=2)

	text(mean(c(-90.120752,-97.485244)),50,"Lower Missouri \n River \n",
		col="black",cex=0.8)
	text(mean(c(-103.5,-97.485244)),50,"Segmented \n reach \n",
		col="black",cex=0.8)	
	text(mean(c(-103.5,-106.422049)),50,"Upper \n Missouri \n River",
		col="black",cex=0.8)
	text(-86.5,35, "Mississippi River \n and Atchafalaya \n Basin")
		
	par(srt=-45)
	text(-96.5,40, "Missouri River")
	par(srt=0)		
		
	# insert US 
	par(new=TRUE,oma=c(0,6,12,6))
	plot(us)	
	plot(missouri,col="lightgrey", border="lightgrey",add=TRUE)
	plot(manunits,col="black",lwd=2,add=TRUE)
	plot(us,add=TRUE)	
	}	
	
if(n==2)
	{# US LEVEL STUDY AREA
	plot(us)	
	plot(watershed,col="lightgrey", border="lightgrey",add=TRUE)
	plot(manunits,col="black",lwd=2,add=TRUE)
	plot(us,add=TRUE)	
	}	

if(n==3)
	{
	plot(manunits[manunits$Name!= "Coastal Plains",],col="black",lwd=1)
	plot(bends[bends$B_SEGME==1,],col="black",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==2,],col="grey",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==3,],col="black",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==4,],col="grey",lwd=6,add=TRUE)	
	#plot(bends[bends$B_SEGME==5,],col="black",lwd=6,add=TRUE)	
	#plot(bends[bends$B_SEGME==6,],col="grey",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==7,],col="black",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==8,],col="grey",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==9,],col="black",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==10,],col="grey",lwd=6,add=TRUE)	
	#plot(bends[bends$B_SEGME==11,],col="black",lwd=6,add=TRUE) Kansas River	
	#plot(bends[bends$B_SEGME==12,],col="red",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==13,],col="black",lwd=6,add=TRUE)	
	plot(bends[bends$B_SEGME==14,],col="grey",lwd=6,add=TRUE)	
    
    
    
    
    
	plot(reservoirs,add=TRUE,col="lightblue",border="lightblue",lwd=2.5)		
	points(-104.54,47.28,pch=19,cex=1,col="red")          # INTAKE
	points(-106.422049, 48.001600,pch=19,cex=1,col="red")# FORT PECK DAM	
	points(-101.411601,47.498528,pch=19,cex=1,col="red")  # GARRISON DAM		
	points(-100.402227, 44.451274,pch=19,cex=1,col="red") # OAHE DAM	
	points(-99.448768,44.049215,pch=19,cex=1,col="red")   # BIG BEND DAM	
	points(-98.562062,43.059483,pch=19,cex=1,col="red")   # FORT RANDALL DAM
	points(-97.485244,42.860696,pch=19,cex=1,col="red")   # GAVINS POINT DAM
	text(-95,33, "Mississippi River \n and Atchafalaya \n Basin")
		
	par(srt=-45)
	text(-98.5,41.5, "Missouri River")
	par(srt=0)	

    # PLOT HATCHERIES
	points(y~x,hatcheries,pch=15,cex=1.3, subset=Hatchery!="Gavins Point National Fish Hatchery")
    
    labs<-c("Blind Pony \n Hatchery",
        "Gavins Point\n National Fish Hatchery",
        "Garrison Dam\n National Fish Hatchery",
        "Neosho National\n Fish Hatchery", 
        "Miles City State \nFish Hatchery",
        "Bozeman Fish \nTechnology Center" )    

   
	#text(hatcheries$x, hatcheries$y, 
	#	labs,cex=0.4,pos=c(1,1,3,4,1,4))
    
  
	}

if(n==4)
    {

    }
	
}


source("_R/6-analysis-effort.r")

figures_effort<- function(n, dat=NULL){
 if(n==1)
        {
        par(mfrow=c(2,1),mar=c(2,2,0,0),oma=c(2,2,1,1))
        dat$tmp<-1 # TO SUM FOR COUNTS
        plot_summary<- dcast(dat, basin+gear+gear_id+yr~"freq",
            value.var="tmp",sum)       
        ## LOWER BASIN
        plot_dat<- subset(plot_summary,basin=="LB")
        plot_dat<- plot_dat[order(plot_dat$gear_id, plot_dat$yr),]
        plot(gear_id~yr, plot_dat,
            type='n',las=1)
        indx<- unique(plot_dat$gear_id)
        for(i in indx)
            {
            points(gear_id~yr, plot_dat,subset=gear_id==i,type='b',pch=19,cex=0.3)
            }
            
        ## UPPER BASIN
        plot_dat<- subset(plot_summary,basin=="UB")
        plot_dat<- plot_dat[order(plot_dat$gear_id, plot_dat$yr),]
        plot(gear_id~yr, plot_dat,
            type='n',las=1)
        indx<- unique(plot_dat$gear_id)
        for(i in indx)
            {
            points(gear_id~yr, plot_dat,subset=gear_id==i,type='b',pch=19,cex=0.3)
            }
        mtext(side=2,"Gear Id",outer=TRUE, line=0,cex=1.3)
        mtext(side=1,"Year",outer=TRUE, line=0,cex=1.3)
        }


if(n==2)
  {
    ########### figures from effort analysis
    ########### need to modify and clean up
    # LOOK AT EFFORT BY GEAR
    par(mfrow=c(1,2),mar=c(4,4,3,2))
    datLB<-subset(dat, standard_gear=="yes" & basin=="LB")
    LBgears<-unlist(lapply(unlist(levels(datLB$gear)), function(x) 
    {
      lg<-subset(datLB, gear==x)
      if(nrow(lg)!=0) return(x)
    }))
    datLBgear<-subset(datLB, gear==LBgears[x]) #NEED TO DEFINE x
    #Make Sure There's Only One Gear ID
    if(length(levels(as.factor(datLBgear$gear_id)))>1) print("Warning: Multiple Gear IDs for this Gear")
    #Make Plots    
    hist(datLBgear$effort,  xlab="Effort (in minutes)", main=paste("LB ", LBgears[x], " Effort"))
    boxplot(datLBgear$effort, ylab="Effort (in minutes)", main=paste("LB ", LBgears[x], " Effort"))
    #barplot(datLBgear$effort,  ylab="Effort (in minutes)", main=paste("LB ", LBgears[x], " Effort"))
  }

if(n==3)
  {
    datUBgear<-subset(datUB, gear==UBgears[x]) #NEED TO DEFINE datUB, UBgears, and x
    #Make Sure There's Only One Gear ID
    if(length(levels(as.factor(datUBgear$gear_id)))>1) print("Warning: Multiple Gear IDs for this Gear")
    #Make Plots    
    hist(datUBgear$effort, xlab="Effort (in minutes)", main=paste("UB ", UBgears[x], " Effort"))
    boxplot(datUBgear$effort, ylab="Effort (in minutes)", main=paste("UB ", UBgears[x], " Effort"))
    #barplot(datUBgear$effort,  ylab="Effort (in minutes)", main=paste("UB ", UBgears[x], " Effort"))
    }


    #LOOK AT DISTRIBUTION FIT
    fitfigsLB<-function(x)
    {
      s<-dfitfunLB(x)[1]
      r<-dfitfunLB(x)[2]
      datLBgear<-subset(datLB, gear==LBgears[x])
      #Make QQ and Density Plots
      par(mfrow=c(1,2),mar=c(4,4,3,2))
      theo<-rgamma(n=length(datLBgear)*4, shape=s, rate=r)
      qqplot(theo,datLBgear$effort, xlab="Theoretical", ylab=paste(LBgears[x],"Data"), main="QQ-Plot")
      abline(0,1)
      plot(density(datLBgear$effort), xlab="Effort", ylab="Data", main="PDF")
      px<-seq(min(datLBgear$effort), max(datLBgear$effort),floor((max(datLBgear$effort)-min(datLBgear$effort))/25))
      py<-dgamma(px, shape=s, rate=r)
      points(px,py, xlab="Effort", ylab="Normal Function", main="PDF")
    }
    fitfigsLB(18)

    fitfigsUB<-function(x)
    {
      s<-dfitfunUB(x)[1]
      r<-dfitfunUB(x)[2]
      datUBgear<-subset(datUB, gear==UBgears[x])
      #Make QQ and Density Plots
      par(mfrow=c(1,2),mar=c(4,4,3,2))
      theo<-rgamma(n=length(datUBgear)*4, shape=s, rate=r)
      qqplot(theo,datUBgear$effort, xlab="Theoretical", ylab="Data", main="QQ-Plot")
      abline(0,1)
      plot(density(datUBgear$effort), xlab="Effort", ylab=paste(LBgears[x],"Data"), main="PDF")
      px<-seq(min(datUBgear$effort), max(datUBgear$effort),floor((max(datUBgear$effort)-min(datUBgear$effort))/25))
      py<-dgamma(px, shape=s, rate=r)
      plot(px,py, add=TRUE, col="red", type="l")
      curve(dgamma, shape=s, rate=r, min(datUBgear$effort),max(datUBgear$effort), add=TRUE)
    }
    
    # # Fit parameters (to avoid errors, set lower bounds to zero)
    # 
    # fit.params <- fitdistr(x, "gamma", lower = c(0, 0))
    
    # Plot using density points
    den<-density(datUBgear$effort)
    dat<-data.frame(x = den$x, y = den$y, type="Data")
    dat2<-data.frame(x = dat$x, y=dgamma(dat$x,s, r), type="Gamma Fit")
    dat<-rbind(dat, dat2)
    ggplot(data = dat, aes(x = x,y = y,color=factor(type))) + 
      scale_colour_manual(values=c("black","red"))+
      geom_point(size = 1) +     
      #geom_line(aes(x=dat$x, y=dgamma(dat$x,s, r)), color="red", size = 1) + 
      theme_classic()+
      xlab("Effort (minutes)") +
      ylab("Density") +
      ggtitle("Trotline (TLC1) Effort Distribution")+
      theme(plot.title = element_text(hjust = 0.5), 
            legend.title = element_blank())+
      guides(colour = guide_legend(override.aes = list(size=3)))
    
    
    fitfigsUB(5)



    }

# PERFORMANCE METRIC RESULTS BY MONITORING DESIGN
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]

#CPUE
#Gears & Samp Type Occ 1
boxplot(exp_bias~samp_type, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & gear=="GN14"),
        at = 0:1*4.5 + 1, xlim = c(0, 8.5), 
        ylim = range(dat[which(dat$estimator=="CPUE" & dat$occasions=="1"),]$exp_bias),
        xaxt = "n",
        main="CPUE, 1 Occasion",
        ylab="Bias")
boxplot(exp_bias~samp_type, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & gear=="TLC1"),
        at = 0:1*4.5 + 2, xlim = c(0, 8.5), xaxt = "n", add=TRUE)
boxplot(exp_bias~samp_type, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & gear=="TN"),
        at = 0:1*4.5 + 3, xlim = c(0, 8.5), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:1*4.5 + 2, labels = c("Fixed", "Random"), tick = FALSE)

#Gears & Samp Type Occ 1 Reversed
levels(dat$gear)<-c(rep("GN14", 6), rep("TLC1",2), "TN")
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="f"),
        at = 0:2*4 + 1, xlim = c(0, 11), 
        ylim = range(dat[which(dat$estimator=="CPUE" & dat$occasions=="1"),]$exp_bias), 
        xaxt = "n",
        main="CPUE, 1 Occasion",
        ylab="Bias")
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="r"),
        at = 0:2*4 + 2, xlim = c(0, 11), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:2*4 + 1.5, labels = c("GN14", "TLC1", "TN"), tick = FALSE)

#Gears & Occasions, fixed
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="f"),
        at = 0:2*6 + 1, xlim = c(0, 17), 
        ylim = range(dat[which(dat$estimator=="CPUE" & dat$samp_type=="f"),]$exp_bias), 
        xaxt = "n",
        main="CPUE, Fixed Sampling",
        ylab="Bias")
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="2" & samp_type=="f"),
        at = 0:2*6 + 2, xlim = c(0, 17), xaxt = "n", add=TRUE)
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="3" & samp_type=="f"),
        at = 0:2*6 + 3, xlim = c(0, 17), xaxt = "n", add=TRUE)
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="4" & samp_type=="f"),
        at = 0:2*6 + 4, xlim = c(0, 17), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:2*6 + 2.5, labels = c("GN14", "TLC1", "TN"), tick = FALSE)
#Gears & Occasions, random
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="r"),
        at = 0:2*6 + 1, xlim = c(0, 17), 
        ylim = range(dat[which(dat$estimator=="CPUE" & dat$samp_type=="r"),]$exp_bias), 
        xaxt = "n",
        main="CPUE, Random Sampling",
        ylab="Bias")
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="2" & samp_type=="r"),
        at = 0:2*6 + 2, xlim = c(0, 17), xaxt = "n", add=TRUE)
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="3" & samp_type=="r"),
        at = 0:2*6 + 3, xlim = c(0, 17), xaxt = "n", add=TRUE)
boxplot(exp_bias~gear, data=dat, 
        subset=c(estimator=="CPUE" & occasions=="4" & samp_type=="f"),
        at = 0:2*6 + 4, xlim = c(0, 17), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:2*6 + 2.5, labels = c("GN14", "TLC1", "TN"), tick = FALSE)

#Sampling Type & Occasions, GN14
boxplot(exp_bias~occasions, data=dat, 
        subset=c(estimator=="CPUE" & gear=="GN14" & samp_type=="f"),
        at = 0:3*4 + 1, xlim = c(0, 15), 
        ylim = range(dat[which(dat$estimator=="CPUE" & dat$gear=="GN14"),]$exp_bias), 
        xaxt = "n",
        main="CPUE: Gillnet",
        ylab="Bias")
boxplot(exp_bias~occasions, data=dat, 
        subset=c(estimator=="CPUE" & gear=="GN14" & samp_type=="r"),
        at = 0:3*4 + 2, xlim = c(0, 15), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:3*4 + 1.5, labels = c("1", "2", "3", "4"), tick = FALSE)


## Comparing Estimators
# Estimator and Sampling Type, Occ 4
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
        at = 0:7*8 + 1, xlim = c(0, 59), 
        ylim = range(dat[which(dat$occasions=="4" & dat$gear=="GN14"),]$exp_bias, na.rm=TRUE), 
        xaxt = "n",
        main="Gillnet, 4 occasions",
        ylab="Bias")
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="r"),
        at = 0:7*8 + 2, xlim = c(0, 59), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:7*8 + 1.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#CHOPPED
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
        at = 0:7*8 + 1, xlim = c(0, 59), 
        ylim = c(-0.03,0.03), 
        xaxt = "n",
        main="Gillnet, 4 occasions",
        ylab="Bias")
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="r"),
        at = 0:7*8 + 2, xlim = c(0, 59), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:7*8 + 1.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)

# Estimator and Occasion, Fixed
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="1" & gear=="GN14" & samp_type=="f"),
        at = 0:7*8 + 1, xlim = c(0, 61), 
        ylim = range(dat[which(dat$samp_type=="f" & dat$gear=="GN14"),]$exp_bias, na.rm=TRUE), 
        xaxt = "n",
        main="Gillnet, Fixed Sampling",
        ylab="Bias")
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="2" & gear=="GN14" & samp_type=="f"),
        at = 0:7*8 + 2, xlim = c(0, 61), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="3" & gear=="GN14" & samp_type=="f"),
        at = 0:7*8 + 3, xlim = c(0, 61), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
        at = 0:7*8 + 4, xlim = c(0, 61), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:7*8 + 2.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#CHOPPED
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="1" & gear=="GN14" & samp_type=="f"),
        at = 0:7*6 + 1, xlim = c(0, 47), 
        ylim = c(-0.1,0.1),
        xaxt = "n",
        main="Gillnet, Random Sampling",
        ylab="Bias")
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="2" & gear=="GN14" & samp_type=="f"),
        at = 0:7*6 + 2, xlim = c(0, 47), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="3" & gear=="GN14" & samp_type=="f"),
        at = 0:7*6 + 3, xlim = c(0, 47), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
        at = 0:7*6 + 4, xlim = c(0, 47), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:7*6 + 2.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)


# Estimator and Gear, Fixed
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
        at = 0:7*6 + 1, xlim = c(0, 46), 
        ylim = range(dat[which(dat$samp_type=="f" & dat$occasions=="4"),]$exp_bias, na.rm=TRUE), 
        xaxt = "n",
        main="Fixed Sampling, 4 Occasions",
        ylab="Bias")
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*6 + 2, xlim = c(0, 46), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="TN" & samp_type=="f"),
        at = 0:7*6 + 3, xlim = c(0, 46), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:7*6 + 2, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#CHOPPED
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
        at = 0:7*6 + 1, xlim = c(0, 46), 
        ylim = c(-0.075,0.075),
        xaxt = "n",
        main="Fixed Sampling, 4 Occasions",
        ylab="Bias")
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*6 + 2, xlim = c(0, 46), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat, 
        subset=c(occasions=="4" & gear=="TN" & samp_type=="f"),
        at = 0:7*6 + 3, xlim = c(0, 46), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(1, at = 0:7*6 + 2, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
