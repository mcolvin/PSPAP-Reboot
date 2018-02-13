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

