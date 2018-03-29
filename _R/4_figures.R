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

if(n==4)
  {
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
      curve(px,py, add=TRUE, col="red", type=1, add=TRUE)
      curve(dgamma, shape=s, rate=r, min(datUBgear$effort),max(datUBgear$effort), add=TRUE)
    }
    
    # # Fit parameters (to avoid errors, set lower bounds to zero)
    # 
    # fit.params <- fitdistr(x, "gamma", lower = c(0, 0))
    
    # Plot using density points
    den<-density(datUBgear$effort)
    dat<-data.frame(x = den$x, y = den$y, type="Data")
    dat2<-data.frame(x = dat$x, y=dgamma(dat$x,s, r), type="Gamma Fit")
    dat<-rbind(dat2, dat)
    ggplot(data = dat, aes(x = x,y = y, group=type, linetype=type)) + 
      #scale_colour_manual(values=c("black","black"))+
      #geom_point(size = 1) +     
      geom_line(size=1)+
      #geom_line(aes(x=den$x, y=den$y), color="black", size = 1,linetype=4)+
      #geom_line(aes(x=dat$x, y=dgamma(dat$x,s, r)), color="black", size = 1,linetype=1) + 
      theme_classic()+
      xlab("Effort (minutes per gear deployment)") +
      ylab("Density") +
      #ggtitle("Trotline (TLC1) Effort Distribution")+
      theme(#plot.title = element_text(hjust = 0.5), 
            legend.title = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1))#+
      #guides(colour = guide_legend(override.aes = list(size=3)))
    
    
    efitfigsUB(5)
  }
}

fig_utilities<-function()
{
  # UTILITIES
  ## TREND
  tt<-read.csv( "_output/trnd_min_max.csv")
  tbmin<-min(tt$min_abs_exp_bias)
  tbmax<-max(abs(min(tt$min_exp_bias)),max(tt$max_exp_bias))
  tpmin<-min(tt$min_exp_prec)
  tpmax<-max(tt$max_exp_prec)
  
  tbcutL<-0.0001
  tbcutU<-0.7
  tpcutL<-0.00005
  tpcutU<-0.375
  
  tbmin<tbcutL
  tbmax>tbcutU
  tpmin<tpcutL
  tpmax>tpcutU
  
  
  t_bias_x<-c(tbmin, tbcutL, tbcutU, tbmax)
  t_bias_y<-c(1, 1, 0, 0)
  t_prec_x<-c(tpmin, tpcutL, tpcutU, tpmax)
  t_prec_y<-c(1, 1, 0, 0)
  
  par(mfrow=c(2,2))
  plot(t_bias_x, t_bias_y, typ="l", xlab="Absolute Bias",ylab="",
       ylim=c(0,1), yaxp=c(0,1,4), xlim=c(0,tbmax))
  mtext("Trend", side=2, padj=-5, font=2)
  mtext("Bias", side=3, padj=-1, font=2)
  plot(t_prec_x, t_prec_y, typ="l", xlab="CV",ylab="Utility",
       ylim=c(0,1), yaxp=c(0,1,4), xlim=c(0,tpmax))
  mtext("Precision", side=3, padj=-1, font=2)
  
  ## ABUNDANCE
  at<-read.csv( "_output/basin_abund_min_max.csv")
  abmin<-min(at$min_rel_abs_bias)
  abmax<-max(abs(min(at$min_rel_bias)),max(at$max_rel_bias))
  apmin<-min(at$min_prec)
  apmax<-max(at$max_prec)
  
  abcutL<-0.00001
  abcutU<-1
  apcutL<-0.0001
  apcutU<-0.3
  
  abmin<abcutL
  abmax>abcutU
  apmin<apcutL
  apmax>apcutU
  
  a_bias_x<-c(abmin, abcutL, abcutU, abmax)
  a_bias_y<-c(1, 1, 0, 0)
  a_prec_x<-c(apmin, apcutL, apcutU, apmax)
  a_prec_y<-c(1, 1, 0, 0)
  
  plot(a_bias_x, a_bias_y, typ="l", xlab="Absolute Relative Bias",ylab="",
       ylim=c(0,1), yaxp=c(0,1,4), xlim=c(0,abmax))
  mtext("Abundance", side=2, padj=-5,font=2)
  plot(a_prec_x, a_prec_y, typ="l", xlab="CV",ylab="Utility",
       ylim=c(0,1), yaxp=c(0,1,4), xlim=c(0,apmax))
}


# # PERFORMANCE METRIC RESULTS BY MONITORING DESIGN
# dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
# dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
# 
# #CPUE
# #Gears & Samp Type Occ 1
# boxplot(exp_bias~samp_type, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & gear=="GN14"),
#         at = 0:1*4.5 + 1, xlim = c(0, 8.5), 
#         ylim = range(dat[which(dat$estimator=="CPUE" & dat$occasions=="1"),]$exp_bias),
#         xaxt = "n",
#         main="CPUE, 1 Occasion",
#         ylab="Bias")
# boxplot(exp_bias~samp_type, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & gear=="TLC1"),
#         at = 0:1*4.5 + 2, xlim = c(0, 8.5), xaxt = "n", add=TRUE)
# boxplot(exp_bias~samp_type, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & gear=="TN"),
#         at = 0:1*4.5 + 3, xlim = c(0, 8.5), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:1*4.5 + 2, labels = c("Fixed", "Random"), tick = FALSE)
# 
# #Gears & Samp Type Occ 1 Reversed
# levels(dat$gear)<-c(rep("GN14", 6), rep("TLC1",2), "TN")
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="f"),
#         at = 0:2*4 + 1, xlim = c(0, 11), 
#         ylim = range(dat[which(dat$estimator=="CPUE" & dat$occasions=="1"),]$exp_bias), 
#         xaxt = "n",
#         main="CPUE, 1 Occasion",
#         ylab="Bias")
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="r"),
#         at = 0:2*4 + 2, xlim = c(0, 11), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:2*4 + 1.5, labels = c("GN14", "TLC1", "TN"), tick = FALSE)
# 
# #Gears & Occasions, fixed
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="f"),
#         at = 0:2*6 + 1, xlim = c(0, 17), 
#         ylim = range(dat[which(dat$estimator=="CPUE" & dat$samp_type=="f"),]$exp_bias), 
#         xaxt = "n",
#         main="CPUE, Fixed Sampling",
#         ylab="Bias")
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="2" & samp_type=="f"),
#         at = 0:2*6 + 2, xlim = c(0, 17), xaxt = "n", add=TRUE)
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="3" & samp_type=="f"),
#         at = 0:2*6 + 3, xlim = c(0, 17), xaxt = "n", add=TRUE)
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="4" & samp_type=="f"),
#         at = 0:2*6 + 4, xlim = c(0, 17), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:2*6 + 2.5, labels = c("GN14", "TLC1", "TN"), tick = FALSE)
# #Gears & Occasions, random
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="1" & samp_type=="r"),
#         at = 0:2*6 + 1, xlim = c(0, 17), 
#         ylim = range(dat[which(dat$estimator=="CPUE" & dat$samp_type=="r"),]$exp_bias), 
#         xaxt = "n",
#         main="CPUE, Random Sampling",
#         ylab="Bias")
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="2" & samp_type=="r"),
#         at = 0:2*6 + 2, xlim = c(0, 17), xaxt = "n", add=TRUE)
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="3" & samp_type=="r"),
#         at = 0:2*6 + 3, xlim = c(0, 17), xaxt = "n", add=TRUE)
# boxplot(exp_bias~gear, data=dat, 
#         subset=c(estimator=="CPUE" & occasions=="4" & samp_type=="f"),
#         at = 0:2*6 + 4, xlim = c(0, 17), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:2*6 + 2.5, labels = c("GN14", "TLC1", "TN"), tick = FALSE)
# 
# #Sampling Type & Occasions, GN14
# boxplot(exp_bias~occasions, data=dat, 
#         subset=c(estimator=="CPUE" & gear=="GN14" & samp_type=="f"),
#         at = 0:3*4 + 1, xlim = c(0, 15), 
#         ylim = range(dat[which(dat$estimator=="CPUE" & dat$gear=="GN14"),]$exp_bias), 
#         xaxt = "n",
#         main="CPUE: Gillnet",
#         ylab="Bias")
# boxplot(exp_bias~occasions, data=dat, 
#         subset=c(estimator=="CPUE" & gear=="GN14" & samp_type=="r"),
#         at = 0:3*4 + 2, xlim = c(0, 15), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:3*4 + 1.5, labels = c("1", "2", "3", "4"), tick = FALSE)
# 
# 
# ## Comparing Estimators
# # Estimator and Sampling Type, Occ 4
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*8 + 1, xlim = c(0, 59), 
#         ylim = range(dat[which(dat$occasions=="4" & dat$gear=="GN14"),]$exp_bias, na.rm=TRUE), 
#         xaxt = "n",
#         main="Gillnet, 4 occasions",
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="r"),
#         at = 0:7*8 + 2, xlim = c(0, 59), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*8 + 1.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
# #CHOPPED
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*8 + 1, xlim = c(0, 59), 
#         ylim = c(-0.03,0.03), 
#         xaxt = "n",
#         main="Gillnet, 4 occasions",
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="r"),
#         at = 0:7*8 + 2, xlim = c(0, 59), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*8 + 1.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
# 
# # Estimator and Occasion, Fixed
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="1" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*8 + 1, xlim = c(0, 61), 
#         ylim = range(dat[which(dat$samp_type=="f" & dat$gear=="GN14"),]$exp_bias, na.rm=TRUE), 
#         xaxt = "n",
#         main="Gillnet, Fixed Sampling",
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="2" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*8 + 2, xlim = c(0, 61), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="3" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*8 + 3, xlim = c(0, 61), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*8 + 4, xlim = c(0, 61), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*8 + 2.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
# #CHOPPED
# g="TN"
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="1" & gear==g & samp_type=="f"),
#         at = 0:7*6 + 1, xlim = c(0, 47), 
#         ylim = c(-0.1,0.1),
#         xaxt = "n",
#         main=paste0(g, ", Fixed Sampling"),
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="2" & gear==g & samp_type=="f"),
#         at = 0:7*6 + 2, xlim = c(0, 47), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="3" & gear==g & samp_type=="f"),
#         at = 0:7*6 + 3, xlim = c(0, 47), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear==g & samp_type=="f"),
#         at = 0:7*6 + 4, xlim = c(0, 47), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*6 + 2.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
# 
# 
# Estimator and Gear, Fixed
# boxplot(exp_bias~estimator, data=dat,
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*6 + 1, xlim = c(0, 46),
#         ylim = range(dat[which(dat$samp_type=="f" & dat$occasions=="4"),]$exp_bias, na.rm=TRUE),
#         xaxt = "n",
#         main="Fixed Sampling, 4 Occasions",
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat,
#         subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
#         at = 0:7*6 + 2, xlim = c(0, 46), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat,
#         subset=c(occasions=="4" & gear=="TN" & samp_type=="f"),
#         at = 0:7*6 + 3, xlim = c(0, 46), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*6 + 2, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
# #CHOPPED
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="GN14" & samp_type=="f"),
#         at = 0:7*6 + 1, xlim = c(0, 46), 
#         ylim = c(-0.075,0.075),
#         xaxt = "n",
#         main="Fixed Sampling, 4 Occasions",
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
#         at = 0:7*6 + 2, xlim = c(0, 46), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions=="4" & gear=="TN" & samp_type=="f"),
#         at = 0:7*6 + 3, xlim = c(0, 46), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*6 + 2, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
# #TREND CHECKS
# occ="4"
# samp="r"
# samp.text<-"Random"
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions==occ & gear=="GN14" & samp_type==samp),
#         at = 0:7*6 + 1, xlim = c(0, 46), 
#         ylim = c(-0.075,0.075),
#         xaxt = "n",
#         main = paste0(samp.text, " Sampling, ", occ, " Occasions"),
#         ylab="Bias")
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions==occ & gear=="TLC1" & samp_type==samp),
#         at = 0:7*6 + 2, xlim = c(0, 46), xaxt = "n", add=TRUE)
# boxplot(exp_bias~estimator, data=dat, 
#         subset=c(occasions==occ & gear=="TN" & samp_type==samp),
#         at = 0:7*6 + 3, xlim = c(0, 46), xaxt = "n", add=TRUE)
# abline(h=0, lty=2, col="red")
# axis(1, at = 0:7*6 + 2, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)

results_boxplots<-function(n)
{
  if(n==1)
  {
#######################
# PERFORMANCE METRICS #
#######################
par(mfrow=c(2,2),
    oma = c(4,-0.1,3,0) + 0.1,
    mar = c(1,4,1,1) + 0.1,
    las=1)

    # P2
    dat<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
    dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
    dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
    #dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
    dat$estimator<-factor(as.character(dat$estimator), 
                          levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                   "Mt_AM",  "Mt_WM",  "CRDMS"))
    dat<-dat[order(dat$estimator),]
    rlim<-0.5
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
            at = 0:6*15 + 1, xlim = c(0, 102),
            col="Gray",
            ylab="Bias Metric",
            #range=rlim,
            #ylim=range(dat[which(dat$gear=="TLC1"),]$rel_bias, na.rm=TRUE),
            ylim=c(-1,1),
            xaxt = "n"#,
            #main="Trotlines",
            #ylab="Abundance Relative Bias",
            )
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
            at = 0:6*15 + 2, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
            at = 0:6*15 + 4, xlim = c(0, 102),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
            at = 0:6*15 + 5, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
            at = 0:6*15 + 7, xlim = c(0, 102),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
            at = 0:6*15 + 8, xlim = c(0, 102),  xaxt = "n", add=TRUE)#, range=rlim)
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
            at = 0:6*15 + 10, xlim = c(0, 102),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
    boxplot(rel_bias~estimator, data=dat,
            subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
            at = 0:6*15 + 11, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
    abline(h=0, lty=2, col="red")
    axis(3, at = 0:6*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
    axis(3, at = -4.25, labels = "Estimator:", tick = FALSE)
    axis(1, at = 0:7*15 + 1.5, labels = rep("",8), tick = TRUE)
    axis(1, at = 0:7*15 + 4.5, labels = rep("",8), tick = TRUE)
    axis(1, at = 0:7*15 + 7.5, labels = rep("",8), tick = TRUE)
    axis(1, at = 0:7*15 + 10.5, labels = rep("",8), tick = TRUE)
    legend("topright", inset=.015, title="Sampling Type",
           c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
    #mtext("Occasions", 1, padj=3.5)
    #mtext("Estimator", 3, padj=-3)
    #mtext("Trotlines", 3, padj=-4, font=2)
    mtext("Abundance", 3, padj=-3.5, font=2)
    
    
# P1
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat<-dat[order(dat$estimator),]

boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 1, xlim = c(0, 117),
        col="Gray",
        #range=0.5,
        #ylim = range(dat[which(dat$gear=="TLC1"),]$exp_bias, na.rm=TRUE),
        ylim=c(-0.05, 0.05),
        xaxt = "n"#,
        #main="Trotlines",
        #ylab="Trend Bias"
        )
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 2, xlim = c(0, 96), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 4, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 5, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 7, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 8, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 10, xlim = c(0, 117), xaxt = "n", col="Gray",add=TRUE)
boxplot(exp_bias~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 11, xlim = c(0, 117), xaxt = "n", add=TRUE)
abline(h=0, lty=2, col="red")
axis(3, at = 0:7*15 + c(5.5, 5.5, 5.5,5.4,5.65,5.8, 5.8, 5.8), labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#axis(3, at = -4.5, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("",8), tick = TRUE)
#legend("bottomleft", inset=.005, title="Sampling Type",
#       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
#mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
#mtext("Trotlines", 3, padj=-4, font=2)
mtext("Trend", 3, padj=-3.5, font=2)



# P4
dat<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat$estimator<-factor(as.character(dat$estimator), 
                      levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                               "Mt_AM",  "Mt_WM",  "CRDMS"))
dat<-dat[order(dat$estimator),]

boxplot(prec~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 1, xlim = c(0, 102),
        col="Gray",
        #range=rlim,
        #ylim=range(dat[which(dat$gear=="TLC1"),]$prec, na.rm=TRUE),
        ylim=c(0,0.3),
        xaxt = "n",
        #main="Trotlines",
        #ylab="Abundance Precision (CV)",
        ylab="Precision(CV)")
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 2, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 4, xlim = c(0, 102),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 5, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 7, xlim = c(0, 102),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 8, xlim = c(0, 102),  xaxt = "n", add=TRUE)#, range=rlim)
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 10, xlim = c(0, 102),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
boxplot(prec~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 11, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
#abline(h=0, lty=2, col="red")
#axis(3, at = 0:6*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
#legend(42,0.3, inset=.01, title="Sampling Type",
#       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
#mtext("Trotlines", 3, padj=-4, font=2)


# P3
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat<-dat[order(dat$estimator),]

boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 1, xlim = c(0, 117),
        col="Gray",
        #range=rlim,
        #ylim=range(dat[which(dat$gear=="TLC1"),]$exp_precision, na.rm=TRUE),
        ylim=c(0,0.028),
        xaxt = "n"#,
        #main="Trotlines",
        #ylab="Trend Precision (CV)"
        )
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 2, xlim = c(0, 96), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 4, xlim = c(0, 117),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 5, xlim = c(0, 117), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 7, xlim = c(0, 117),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 8, xlim = c(0, 117),  xaxt = "n", add=TRUE)#, range=rlim)
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 10, xlim = c(0, 117),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
boxplot(exp_precision~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 11, xlim = c(0, 117), xaxt = "n", add=TRUE)#, range=rlim)
#abline(h=0, lty=2, col="red")
#axis(3, at = 0:7*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
#legend("topleft", inset=.01, title="Sampling Type",
#       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
#mtext("Trotlines", 3, padj=-4, font=2)
}

if(n==2)
  {
#########################
# PERFORMANCE UTILITIES #
#########################
  par(mfrow=c(2,2),
      oma = c(4,-0.1,3,0) + 0.1,
      mar = c(1,4,1,1) + 0.1,
      las=1)
  # P2
  dat<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
  dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
  dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
  #dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
  dat$estimator<-factor(as.character(dat$estimator), 
                        levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                 "Mt_AM",  "Mt_WM",  "CRDMS"))
  dat<-dat[order(dat$estimator),]
  rlim<-0.5
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
          at = 0:6*15 + 1, xlim = c(0, 102),
          col="Gray",
          #range=rlim,
          ylim=range(dat[which(dat$gear=="TLC1"),]$abund_bias_utility, na.rm=TRUE),
          #ylim=c(-1,0.6),
          xaxt = "n",
          #main="Trotlines",
          #ylab="Abundance Bias Utility")
          ylab="Bias Utility")
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
          at = 0:6*15 + 2, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
          at = 0:6*15 + 4, xlim = c(0, 102),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
          at = 0:6*15 + 5, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
          at = 0:6*15 + 7, xlim = c(0, 102),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
          at = 0:6*15 + 8, xlim = c(0, 102),  xaxt = "n", add=TRUE)#, range=rlim)
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
          at = 0:6*15 + 10, xlim = c(0, 102),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
  boxplot(abund_bias_utility~estimator, data=dat,
          subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
          at = 0:6*15 + 11, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
  #abline(h=0, lty=2, col="red")
  axis(3, at = 0:6*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
  #axis(3, at = -4, labels = "Estimator:", tick = FALSE)
  axis(1, at = 0:7*15 + 1.5, labels = rep("",8), tick = TRUE)
  axis(1, at = 0:7*15 + 4.5, labels = rep("",8), tick = TRUE)
  axis(1, at = 0:7*15 + 7.5, labels = rep("",8), tick = TRUE)
  axis(1, at = 0:7*15 + 10.5, labels = rep("",8), tick = TRUE)
  #legend("bottomright", inset=.01, title="Sampling Type",
  #       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
  #mtext("Occasions", 1, padj=3.5)
  #mtext("Estimator", 3, padj=-3)
  #mtext("Trotlines", 3, padj=-4, font=2)
  mtext("Abundance", 3, padj=-4, font=2)
  
  
# P1
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat$estimator<-factor(as.character(dat$estimator), 
                      levels=c("CPUE","M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                               "Mt_AM",  "Mt_WM",  "CRDMS"))
dat<-dat[order(dat$estimator),]

boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 1, xlim = c(0, 117),
        col="Gray",
        #range=0.5,
        #ylim=range(dat[which(dat$gear=="TLC1"),]$trend_bias_utility, na.rm=TRUE),
        ylim=c(0.8, 1),
        xaxt = "n")#,
        #main="Trotlines",
        #ylab="Trend Bias Utility")
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 2, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 4, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 5, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 7, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 8, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 10, xlim = c(0, 117), xaxt = "n", col="Gray",add=TRUE)
boxplot(trend_bias_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 11, xlim = c(0, 117), xaxt = "n", add=TRUE)
#abline(h=0, lty=2, col="red")
axis(3, at = 0:7*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("",8), tick = TRUE)
#legend("bottomleft", inset=.005, title="Sampling Type",
#       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
#mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
#mtext("Trotlines", 3, padj=-4, font=2)
mtext("Trend", 3, padj=-4, font=2)


# P4
dat<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat$estimator<-factor(as.character(dat$estimator), 
                      levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                               "Mt_AM",  "Mt_WM",  "CRDMS"))
dat<-dat[order(dat$estimator),]

boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 1, xlim = c(0, 102),
        col="Gray",
        #range=rlim,
        ylim=range(dat[which(dat$gear=="TLC1"),]$abund_prec_utility, na.rm=TRUE),
        #ylim=c(0,0.2),
        xaxt = "n",
        #main="Trotlines",
        #ylab="Abundance Precision Utility"
        ylab="Precision Utility")
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 2, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 4, xlim = c(0, 102),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 5, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 7, xlim = c(0, 102),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 8, xlim = c(0, 102),  xaxt = "n", add=TRUE)#, range=rlim)
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 10, xlim = c(0, 102),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
boxplot(abund_prec_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 11, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
#abline(h=0, lty=2, col="red")
#axis(3, at = 0:6*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
legend(33,0.2, inset=.01, title="Sampling Type",
       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
#mtext("Trotlines", 3, padj=-4, font=2)



# P3
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat<-dat[order(dat$estimator),]
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 1, xlim = c(0, 117),
        col="Gray",
        #range=rlim,
        #ylim=range(dat[which(dat$gear=="TLC1"),]$trend_precision_utility, na.rm=TRUE),
        ylim=c(0.85,1),
        xaxt = "n")#,
        #main="Trotlines",
        #ylab="Trend Precision Utility")
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 2, xlim = c(0, 117), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 4, xlim = c(0, 117),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 5, xlim = c(0, 117), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 7, xlim = c(0, 117),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 8, xlim = c(0, 117),  xaxt = "n", add=TRUE)#, range=rlim)
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 10, xlim = c(0, 117),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
boxplot(trend_precision_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 11, xlim = c(0, 117), xaxt = "n", add=TRUE)#, range=rlim)
#abline(h=0, lty=2, col="red")
#axis(3, at = 0:7*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
#axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
#legend("bottomleft", inset=.01, title="Sampling Type",
#       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
#mtext("Trotlines", 3, padj=-4, font=2)
}



if(n==3)
{
#############################
# TREND/ABUNDANCE UTILITIES #
#############################
par(mfrow=c(2,1))
# P1
dat<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat$estimator<-factor(as.character(dat$estimator), 
                      levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                               "Mt_AM",  "Mt_WM",  "CRDMS"))
dat<-dat[order(dat$estimator),]
dat$abund_utility<-(0.5*dat$abund_bias_utility+0.5*dat$abund_prec_utility)

boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 1, xlim = c(0, 102),
        col="Gray",
        #range=rlim,
        ylim=range(dat[which(dat$gear=="TLC1"),]$abund_utility, na.rm=TRUE),
        #ylim=c(-1,0.6),
        xaxt = "n",
        #main="Trotlines",
        ylab="Abundance Utility")
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 2, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 4, xlim = c(0, 102),xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 5, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 7, xlim = c(0, 102),  xaxt = "n",col="Gray", add=TRUE)#, range=rlim)
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 8, xlim = c(0, 102),  xaxt = "n", add=TRUE)#, range=rlim)
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 10, xlim = c(0, 102),  xaxt = "n", col="Gray",add=TRUE)#, range=rlim)
boxplot(abund_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 11, xlim = c(0, 102), xaxt = "n", add=TRUE)#, range=rlim)
#abline(h=0, lty=2, col="red")
axis(3, at = 0:6*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
#legend("bottomleft", inset=.01, title="Sampling Type",
#       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
mtext("Trotlines", 3, padj=-4, font=2)

# P2
dat<-readRDS("D:/_output/4-tables/trnd_table.rds")
dat<-dat[which(dat$gear %in% c("GN14", "TLC1", "TN")),]
dat<-dat[-which(dat$estimator=="CRDMS" & dat$occasions==4),]
#dat<-dat[which(dat$catch_id==3 | dat$catch_id==4),]
dat$estimator<-factor(as.character(dat$estimator), 
                      levels=c("CPUE","M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                               "Mt_AM",  "Mt_WM",  "CRDMS"))
dat<-dat[order(dat$estimator),]
dat$trend_utility<-(0.5*dat$trend_bias_utility+0.5*dat$trend_precision_utility)

boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 1, xlim = c(0, 117),
        col="Gray",
        #range=0.5,
        #ylim=range(dat[which(dat$gear=="TLC1"),]$trend_utility, na.rm=TRUE),
        ylim=c(0.9, 1),
        xaxt = "n",
        #main="Trotlines",
        ylab="Trend Utility")
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 2, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 4, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 5, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 7, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 8, xlim = c(0, 117), xaxt = "n", add=TRUE)
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:7*15 + 10, xlim = c(0, 117), xaxt = "n", col="Gray",add=TRUE)
boxplot(trend_utility~estimator, data=dat,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:7*15 + 11, xlim = c(0, 117), xaxt = "n", add=TRUE)
#abline(h=0, lty=2, col="red")
axis(3, at = 0:7*15 + 5.5, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
legend("bottomleft", inset=.005, title="Sampling Type",
       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
mtext("Trotlines", 3, padj=-4, font=2)
}
if(n==4)
{
#######################
#  OVERALL UTILITIES  #
#######################  
  datT<-readRDS("D:/_output/4-tables/trnd_table.rds")
  datT<-datT[which(datT$gear %in% c("GN14", "TLC1", "TN")),]
  datT<-datT[-which(datT$estimator=="CRDMS" & datT$occasions==4),]
  #datT<-datT[which(datT$catch_id==3 | datT$catch_id==4),]
  datT$estimator<-factor(as.character(datT$estimator), 
                        levels=c("CPUE","M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                 "Mt_AM",  "Mt_WM",  "CRDMS"))
  datT<-datT[order(datT$estimator),]
  datT$trend_utility<-(0.5*datT$trend_bias_utility+0.5*datT$trend_precision_utility)
  datT<-datT[,c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator",
                "trend_utility")]
  datT[which(is.na(datT$trend_utility)),]$trend_utility<-0
  datT_CPUE<-datT[which(datT$estimator=="CPUE"),]
  datT_Abund<-datT[which(datT$estimator!="CPUE"),]
  
  datA<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
  datA<-datA[which(datA$gear %in% c("GN14", "TLC1", "TN")),]
  datA<-datA[-which(datA$estimator=="CRDMS" & datA$occasions==4),]
  #datA<-datA[which(datA$catch_id==3 | datA$catch_id==4),]
  datA$estimator<-factor(as.character(datA$estimator), 
                        levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                 "Mt_AM",  "Mt_WM",  "CRDMS"))
  datA<-datA[order(datA$estimator),]
  datA$abund_utility<-(0.5*datA$abund_bias_utility+0.5*datA$abund_prec_utility)
  datA<-datA[,c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator", 
                "abund_utility")]
  datA[which(is.na(datA$abund_utility)),]$abund_utility<-0
  datA_MKA_WM<-datA[which(datA$estimator=="MKA_WM"),
                    c("pop_id", "catch_id","gear", "samp_type", "occasions", 
                      "abund_utility")]
  
  dat<-merge(datT_Abund, datA, 
             by=c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator"), 
             all=TRUE)
  dat_CPUE<-merge(datT_CPUE, datA_MKA_WM,
                  by=c("pop_id", "catch_id","gear", "samp_type", "occasions"), 
                  all=TRUE)
  dat<-rbind(dat_CPUE, dat)
  dat$utility<-(0.5*dat$trend_utility+0.5*dat$abund_utility)
  dat$estimator<-factor(as.character(dat$estimator), 
                         levels=c("CPUE", "M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
                                  "Mt_AM",  "Mt_WM",  "CRDMS"))
  dat<-dat[order(datA$estimator),]
 
# ALL 
  par(mfrow=c(1,1))
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
          at = 0:7*15 + 1, xlim = c(0, 117),
          col="Gray",
          #range=0.5,
          #ylim=range(dat[which(dat$gear=="TLC1"),]$utility, na.rm=TRUE),
          ylim=c(0.55, 1),
          xaxt = "n",
          #main="Trotlines",
          ylab="Combined Abundance & Trend Utility")
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
          at = 0:7*15 + 2, xlim = c(0, 117), xaxt = "n", add=TRUE)
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
          at = 0:7*15 + 4, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
          at = 0:7*15 + 5, xlim = c(0, 117), xaxt = "n", add=TRUE)
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
          at = 0:7*15 + 7, xlim = c(0, 117), xaxt = "n",col="Gray", add=TRUE)
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
          at = 0:7*15 + 8, xlim = c(0, 117), xaxt = "n", add=TRUE)
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
          at = 0:7*15 + 10, xlim = c(0, 117), xaxt = "n", col="Gray",add=TRUE)
  boxplot(utility~estimator, data=dat,
          subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
          at = 0:7*15 + 11, xlim = c(0, 117), xaxt = "n", add=TRUE)
  #abline(h=0, lty=2, col="red")
  axis(3, at = 0:7*15 + 6, labels = unique(dat[order(dat$estimator),]$estimator), tick = FALSE)
  axis(3, at = -4, labels = "Estimator:", tick = FALSE)
  axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
  axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
  axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
  axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
  legend("bottomleft", inset=.005, title="Sampling Type",
         c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
  mtext("Occasions", 1, padj=3.5)
  #mtext("Estimator", 3, padj=-3)
  mtext("Trotlines", 3, padj=-4, font=2)
  
#CPUE 
names(datT_CPUE)[6]<-"trend_estimator"
names(datA)[6]<-"abund_estimator"
dat_CPUE_Abund<-merge(datT_CPUE, datA, 
                      by=c("pop_id", "catch_id","gear", "samp_type", "occasions"),
                      all=TRUE)
dat_CPUE_Abund$utility<-(0.5*dat_CPUE_Abund$trend_utility+0.5*dat_CPUE_Abund$abund_utility)
dat_CPUE_Abund$estimator<-paste0(dat_CPUE_Abund$trend_estimator, "+", 
                                 dat_CPUE_Abund$abund_estimator)
dat_CPUE_Abund$estimator<-factor(as.character(dat_CPUE_Abund$estimator), 
                                 levels=c("CPUE+M0_AM", "CPUE+M0_WM", "CPUE+MKA_AM",
                                          "CPUE+MKA_WM","CPUE+Mt_AM",  "CPUE+Mt_WM",
                                          "CPUE+CRDMS"))
dat_CPUE_Abund<-dat_CPUE_Abund[order(dat_CPUE_Abund$estimator),]

boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 1, xlim = c(0, 102),
        col="Gray",
        #range=0.5,
        ylim=range(dat_CPUE_Abund[which(dat_CPUE_Abund$gear=="TLC1"),]$utility, na.rm=TRUE),
        #ylim=c(0.55, 1),
        xaxt = "n",
        #main="Trotlines",
        ylab="Combined Abundance & Trend Utility")
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="1" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 2, xlim = c(0, 102), xaxt = "n", add=TRUE)
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 4, xlim = c(0, 102), xaxt = "n",col="Gray", add=TRUE)
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="2" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 5, xlim = c(0, 102), xaxt = "n", add=TRUE)
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 7, xlim = c(0, 102), xaxt = "n",col="Gray", add=TRUE)
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="3" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 8, xlim = c(0, 102), xaxt = "n", add=TRUE)
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="f"),
        at = 0:6*15 + 10, xlim = c(0, 102), xaxt = "n", col="Gray",add=TRUE)
boxplot(utility~estimator, data=dat_CPUE_Abund,
        subset=c(occasions=="4" & gear=="TLC1" & samp_type=="r"),
        at = 0:6*15 + 11, xlim = c(0, 102), xaxt = "n", add=TRUE)
#abline(h=0, lty=2, col="red")
axis(3, at = 0:6*15 + 6, labels = unique(dat_CPUE_Abund[order(dat_CPUE_Abund$estimator),]$estimator), tick = FALSE)
axis(3, at = -4, labels = "Estimator:", tick = FALSE)
axis(1, at = 0:7*15 + 1.5, labels = rep("1",8), tick = TRUE)
axis(1, at = 0:7*15 + 4.5, labels = rep("2",8), tick = TRUE)
axis(1, at = 0:7*15 + 7.5, labels = rep("3",8), tick = TRUE)
axis(1, at = 0:7*15 + 10.5, labels = rep("4",8), tick = TRUE)
legend(40,0.53, inset=.005, title="Sampling Type",
       c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
mtext("Occasions", 1, padj=3.5)
#mtext("Estimator", 3, padj=-3)
mtext("Trotlines", 3, padj=-4, font=2)
}

if(n==5){  
#############################
#  MEDIAN UTILITY VS. COST  #
#############################  
  # datT<-readRDS("D:/_output/4-tables/trnd_table.rds")
  # datT<-datT[which(datT$gear %in% c("GN14", "TLC1", "TN")),]
  # datT<-datT[-which(datT$estimator=="CRDMS" & datT$occasions==4),]
  # #datT<-datT[which(datT$catch_id==3 | datT$catch_id==4),]
  # datT$estimator<-factor(as.character(datT$estimator), 
  #                        levels=c("CPUE","M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
  #                                 "Mt_AM",  "Mt_WM",  "CRDMS"))
  # datT<-datT[order(datT$estimator),]
  # datT$trend_utility<-(0.5*datT$trend_bias_utility+0.5*datT$trend_precision_utility)
  # datT<-datT[,c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator",
  #               "trend_utility")]
  # datT[which(is.na(datT$trend_utility)),]$trend_utility<-0
  # datT_CPUE<-datT[which(datT$estimator=="CPUE"),]
  # datT_Abund<-datT[which(datT$estimator!="CPUE"),]
  # 
  # datA<-readRDS("D:/_output/4-tables/BasinData/Replicates Averaged Over Year & Basin/Replicates_Basin_Abund.rds")
  # datA<-datA[which(datA$gear %in% c("GN14", "TLC1", "TN")),]
  # datA<-datA[-which(datA$estimator=="CRDMS" & datA$occasions==4),]
  # #datA<-datA[which(datA$catch_id==3 | datA$catch_id==4),]
  # datA$estimator<-factor(as.character(datA$estimator), 
  #                        levels=c("M0_AM", "M0_WM", "MKA_AM", "MKA_WM",
  #                                 "Mt_AM",  "Mt_WM",  "CRDMS"))
  # datA<-datA[order(datA$estimator),]
  # datA$abund_utility<-(0.5*datA$abund_bias_utility+0.5*datA$abund_prec_utility)
  # datA<-datA[,c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator", 
  #               "abund_utility")]
  # datA[which(is.na(datA$abund_utility)),]$abund_utility<-0
  # 
  # dat<-merge(datT_Abund, datA, 
  #            by=c("pop_id", "catch_id","gear", "samp_type", "occasions","estimator"), 
  #            all=TRUE)
  # 
  # names(datT_CPUE)[6]<-"trend_estimator"
  # names(datA)[6]<-"abund_estimator"
  # dat_CPUE<-merge(datT_CPUE, datA, 
  #                 by=c("pop_id", "catch_id","gear", "samp_type", "occasions"),
  #                 all=TRUE)
  # dat_CPUE$estimator<-paste0(dat_CPUE$trend_estimator, "+", 
  #                                  dat_CPUE$abund_estimator)
  # dat_CPUE[which(dat_CPUE$estimator=="CPUE+MKA_WM"), ]$estimator<-"CPUE"
  # dat_CPUE<-dat_CPUE[,names(dat)]
  # dat<-rbind(dat, dat_CPUE)
  # 
  # dat$utility<-(0.5*dat$trend_utility+0.5*dat$abund_utility)
  # U_dat<-ddply(dat, .(gear, estimator, samp_type, occasions), summarize,
  #                 median_utility=median(utility),
  #                 mean_utility=mean(utility),
  #                 sd_utility=sd(utility))
  # cost<-data.frame(occasions=1:4, 
  #                  expected_cost=c(0.3011479,0.6022957, 0.9034436, 1.20459))
  # comp<-merge(U_dat, cost, by=("occasions"), all.x=TRUE)
  # #min(comp$median_utility)
  # plot(comp$expected_cost, comp$median_utility, xlab="Cost (millions of dollars)", ylab="Median Utility",
  #      ylim=c(0.7,1), main="Trotlines")
  # plot(comp$expected_cost, comp$median_utility, xlab="Cost (millions of dollars)", ylab="Median Utility",
  #      ylim=c(0,1), main="Trotlines")
  results<-read.csv("_output/overall_cost_and_utility.csv")
  comp<-results[results$gear=="TLC1",]
  comp$val<-NA
  comp[which(comp$estimator=="CPUE"),]$val<-1
  comp[which(comp$estimator=="MKA_AM" | comp$estimator=="CPUE+MKA_AM"),]$val<-0
  comp[which(comp$estimator=="MKA_WM" | comp$estimator=="CPUE+MKA_WM"),]$val<-15
  comp[which(comp$estimator=="M0_AM" | comp$estimator=="CPUE+M0_AM"),]$val<-2
  comp[which(comp$estimator=="M0_WM" | comp$estimator=="CPUE+M0_WM"),]$val<-17
  comp[which(comp$estimator=="Mt_AM" | comp$estimator=="CPUE+Mt_AM"),]$val<-5
  comp[which(comp$estimator=="Mt_WM" | comp$estimator=="CPUE+Mt_WM"),]$val<-18
  comp[which(comp$estimator=="CRDMS" | comp$estimator=="CPUE+CRDMS"),]$val<-8
  
  plot(comp$expected_cost, comp$U_median, xlab="Cost (millions of dollars)", ylab="Median Utility",
       ylim=c(0.28,0.85), main="Trotlines", type="p", pch=comp$val)
  legend(0.26,0.865, title="Estimator",
         c("CPUE","MKA_AM", "MKA_WM","M0_AM", "M0_WM","Mt_AM", "Mt_WM", "CRDMS"), 
         pch=c(1, 0, 15, 2, 17, 5, 18, 8))
  #plot(comp$expected_cost, comp$U_median, xlab="Cost (millions of dollars)", ylab="Median Utility",
  #     ylim=c(0,1), xlim=c(0.2, 1.6), main="Trotlines", asp=1, type="p", pch=comp$val)

  zoom<-comp[which(comp$expected_cost==unique(comp$expected_cost)[5]),]
  zoom<-zoom[order(zoom$U_median),]
  plot(zoom$expected_cost, zoom$U_median, xlab="Cost (millions of dollars)", ylab="Median Utility",
       ylim=c(0.777,0.799),xlim=c(0.65, 1.15),main="Trotlines", type="p", pch=zoom$val)
  legend(0.64,0.7993, title="Estimator",
         c("CPUE","MKA_AM", "MKA_WM","M0_AM", "M0_WM","Mt_AM", "Mt_WM", "CRDMS"), 
         pch=c(1, 0, 15, 2, 17, 5, 18, 8))
  }  
if(n==6)
{
  ## FUNDAMENTAL OBJECTIVE 1
  outcomes<- read.csv("_output/age1-detection-cpt.csv")
  outcomes<-outcomes[,c(3, 5:9, 11, 12, 14)]
  ### REPORT SAYS RECRUITMENT BETWEEN 1-1000 SO:
  outcomes<-outcomes[outcomes$recruitmentLevelLabs!="1000-5000",]
  outcomes$samp_type<-ifelse(outcomes$design=="Fixed", "f", "r")
  outcomes<-outcomes[,c(10,2:9)]
  
  outcomes$ntrawlsLabs<-factor(as.character(outcomes$ntrawlsLabs), 
                        levels=c("2", "3", "4", "5-9",
                                 "10-19",  "20-29",  "30-39", "40-49"))
  outcomes<-outcomes[order(outcomes$ntrawlsLabs),]
  
  boxplot(detected_prob~ntrawlsLabs, data=outcomes,
          subset=c(samp_type=="f"),
          at = 0:7*4 + 1, xlim = c(0, 31),
          col="Gray",
          #range=0.5,
          ylim=range(outcomes$detected_prob),
          #xaxt = "n",
          #main="Trotlines",
          ylab="Recruitment Detection Utility")
  boxplot(detected_prob~ntrawlsLabs, data=outcomes,
          subset=c(samp_type=="r"),
          at = 0:7*4 + 2, xlim = c(0, 31), xaxt = "n", add=TRUE)
  # legend("bottomleft", inset=.005, title="Sampling Type",
  #        c("Fixed","Random"), fill=c("Gray", "White"), horiz=TRUE, cex=0.8)
  mtext("Number of Trawls", 1, padj=3.5)
}

}
