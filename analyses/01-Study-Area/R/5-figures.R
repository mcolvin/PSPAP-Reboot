figures<- function(n){

if(n==1){
    # ENTIRE STUDY AREA
	plot(manunits,col="black",lwd=1)
	plot(umo,col="black",lwd=2,add=TRUE)	
	plot(wb,add=TRUE,col="grey",border="grey",lwd=2.5)		
	plot(lmo,col="black",add=TRUE,lwd=2)
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
	plot(states_dissolve)	
	plot(missouri,col="lightgrey", border="lightgrey",add=TRUE)
	plot(manunits,col="black",lwd=2,add=TRUE)
	plot(states_dissolve,add=TRUE)	
	}	
	
	
	if(n==2)
	{
	
	#  UPPER MO RIVER
	plot(umo)
	plot(wb,add=TRUE, col="black",border="black")
	points(-104.54,47.28,pch=19,cex=2)#INTAKE
	
	# LOWER MO RIVER
	plot(lmo_miss,xlim=c(-100,-90))
	plot(lmo,lwd=3,add=TRUE)
	plot(wb,add=TRUE, col="black",border="black")
	points(-97.485244,42.860696,pch=19,cex=2)#GAVINS POINT DAM
	
	
	#plot(states_dissolve)
	}
if(n==3)
	{
	plot(manunits,col="black",lwd=1)
	plot(umo,col="black",lwd=2,add=TRUE)	
	plot(wb,add=TRUE,col="grey",border="grey",lwd=2.5)		
	plot(lmo,col="black",add=TRUE,lwd=2)
	points(-104.54,47.28,pch=19,cex=1)          # INTAKE
	points(-106.422049, 48.001600,pch=19, cex=1)# FORT PECK DAM	
	points(-101.411601,47.498528,pch=19,cex=1)  # GARRISON DAM		
	points(-100.402227, 44.451274,pch=19,cex=1) # OAHE DAM	
	points(-99.448768,44.049215,pch=19,cex=1)   # BIG BEND DAM	
	points(-98.562062,43.059483,pch=19,cex=1)   # FORT RANDALL DAM
	points(-97.485244,42.860696,pch=19,cex=1)   # GAVINS POINT DAM
	#map.scale(y=42,ratio=FALSE)
	#arrows(-109.5,43,-109.5,45,lwd=3,length=0.15)
	#text(-109.5,42.5,"N",cex=1.2)
#	par(xpd=TRUE)	
#	segments(-90.120752, 38.813236, -90.120752, 50,lwd=2)
#	segments(-97.485244,42.860696,-97.485244,50,lwd=2) # GAVINS POINT DAM
#	segments(-103.5,48.001600,-103.5,50,lwd=2)	
#	segments(-106.422049,48.001600,-106.422049,50,lwd=2)

#	text(mean(c(-90.120752,-97.485244)),50,"Lower Missouri \n River \n",
#		col="black",cex=0.8)
#	text(mean(c(-103.5,-97.485244)),50,"Segmented \n reach \n",
#		col="black",cex=0.8)	
#	text(mean(c(-103.5,-106.422049)),50,"Upper \n Missouri \n River",
#		col="black",cex=0.8)
	text(-86.5,33, "Mississippi River \n and Atchafalaya \n Basin")
		
	par(srt=-45)
	text(-98.5,41.5, "Missouri River")
	par(srt=0)	

    # PLOT HATCHERIES
	points(y~x,hatcheries,pch=15,cex=1.8)
	text(hatcheries$x, hatcheries$y, 
		hatcheries$Hatchery,cex=0.6,pos=c(1,1,4,4,1,1))



		}

if(n==4)
    {
    plot(majorstreams[majorstreams$NAME%in%c("Mississippi River","Missouri River"),],
		axes=TRUE)
	plot(majorstreams, col="red",add=TRUE)
		axes=TRUE)
	plot(wb2[wb2$Name%in%c("Lewis and Clark Lake"),],col='red',add=TRUE)
	plot(wb2[wb2$Name%in%c("Lewis and Clark Lake"),],col='red',axes=T)
	
	
	ind<-grep("Miss", unique(majorstreams$NAME))
	plot(majorstreams[ind,])
	sort(unique(majorstreams$NAME))[7000:8000]
    
    }
	
	}

