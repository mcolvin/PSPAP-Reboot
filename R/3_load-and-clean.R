
# READ IN BEND DATA FOR RPMA 2 (UPPER) AND 4 (LOWER)

bends<- read.csv("./dat/bend-data.csv")

# make data.frame column names lower case
names(bends)<- tolower(names(bends))
bends<- subset(bends,b_segment %in% c(1,2,3,4,7,8,9,10,13,14))



# READ IN EFFORT DATA FROM 01-PSPAP-Background Analysis

effort<- read.table("./dat/effort_dat.csv")

#Report Effort for GN14,GN18,GN41, GN81, MF, OT16, TLC1, TLC2, TN for both LB and UB
effort<-rbind(effort[1:8,],effort[12:13,],effort[2,], effort[14,], effort[4,],effort[15:18,],effort[20,])
effort[11,1]<-"UB"
effort[13,1]<-"UB"
effort[11,4:11]<-as.integer(rep(0,8))
effort[13,4:11]<-as.integer(rep(0,8))


# READ IN DENSITY DATA
dens<-read.csv("./dat/fish_density.csv")
# REMOVE WU AND HOLAN DATA
dens<-rbind(dens[1:4,],dens[7:nrow(dens),])
# MAKE COLUMN NAMES LOWER CASE
names(dens)<-tolower(names(dens))


############## EFFORT ANALYSIS #####################

## CODE TO COMMUNICATE WITH LOCAL PSPAP DATABASE 
#com3<- odbcConnectAccess2007("C:/Users/mcolvin/Google Drive/Pallid-Sturgeon/analysis-effort/pallids.accdb")
# com3<- odbcConnectAccess2007("C:/Users/sreynolds/Google Drive/Pallid-Sturgeon/analysis-effort/pallids.accdb")
dat<-sqlFetch(com3, "Gear-Specific-Effort")
## CONVERT TO CHARACTER
dat$STARTTIME<- as.character(dat$STARTTIME)
dat$STOPTIME<- as.character(dat$STOPTIME)
## FILL NON TIME FORMATS WITH NA
dat[-(grep(":",dat$STARTTIME)),]$STARTTIME<- NA
dat[-(grep(":",dat$STOPTIME)),]$STOPTIME<- NA   
## DROP NAs
dat<- subset(dat, !(is.na(STARTTIME)));dim(dat)
dat<- subset(dat, !(is.na(STOPTIME)));dim(dat)
dat<- subset(dat, !(is.na(SETDATE)))   ;dim(dat)
## MAKE SURE ALL IS UPPER CASE
dat$STARTTIME<- toupper(dat$STARTTIME)
dat$STOPTIME<-toupper(dat$STOPTIME)

## LOAD GEAR DATA
gear_dat<-sqlFetch(com3, "Gear-Meta-Data")
names(gear_dat)[2]<-"gear_type"
## SUBSET OUT DUPLICATE GEARS 
# POT02, HN, SHN,TN11, MOT02, OT04, OT02
gear_dat<- gear_dat[-which(duplicated(gear_dat$gear_code)==TRUE),]
## MERGE GEAR TYPE WITH EFFORT DATA   
dat<- merge(dat,gear_dat[,c(2,3,4,5,7,10)],by.x="GEAR",
            by.y="gear_code",all.x=TRUE)


### UPDATE STOP DATE FOR OVERNIGHTS
dat$STOPDATE<- dat$SETDATE+60*60*24*dat$overnight

## CONVERT ALL TO HOUR FRACTION TIME
## START TIME
dat$start_time<-unlist(lapply(1:nrow(dat),function(x)
{
  pp<- NA
  xx<-unlist(
    strsplit(
      dat[x,]$STARTTIME,":"
    ))
  if(length(grep("PM",xx))==0|{length(grep("PM",xx))>0 & xx[1]==12}) # HANDLE AM, NON PM, AND 12PM FORMATS
  {
    pp<-paste(as.character(dat[x,]$SETDATE)," ",as.numeric(xx[1]),":", 
              as.numeric(xx[2]),":00",sep="") 
    pp<-strptime(pp, "%Y-%m-%d %H:%M:%S")  
  }
  if(length(grep("PM",xx))>0 & xx[1]!=12) # HANDLE REMAINING PM FORMATS
  {
    pp<-paste(as.character(dat[x,]$SETDATE)," ",as.numeric(xx[1])+12,":", 
              as.numeric(xx[2]),":00",sep="") 
    pp<-strptime(pp, "%Y-%m-%d %H:%M:%S")  
  }
  return(as.character(pp))
}))

## STOP TIME
# xx<-strsplit(dat$STOPTIME,":")
# hr<- unlist(lapply(xx, `[[`, 1))
# mins<- unlist(lapply(xx, `[[`, 2))
# apply(cbind(hr,mins),1,paste,collapse=":")
dat$stop_time<-unlist(lapply(1:nrow(dat),function(x)
{
  pp<- NA
  xx<-unlist(
    strsplit(
      dat[x,]$STOPTIME,":"
    ))
  if(length(grep("PM",xx))==0|{length(grep("PM",xx))>0 & xx[1]==12}) # HANDLE AM, NON PM FORMATS, AND 12PM
  {
    pp<-paste(as.character(dat[x,]$STOPDATE)," ",as.numeric(xx[1]),":", 
              as.numeric(xx[2]),":00",sep="") 
    pp<-strptime(pp, "%Y-%m-%d %H:%M:%S")  
  }
  if(length(grep("PM",xx))>0 & xx[1]!=12) # HANDLE REMAINING PM FORMATS
  {
    pp<-paste(as.character(dat[x,]$STOPDATE)," ",as.numeric(xx[1])+12,":", 
              as.numeric(xx[2]),":00",sep="") 
    pp<-strptime(pp, "%Y-%m-%d %H:%M:%S")  
  }
  return(as.character(pp))
})) 
dat$stop_time<- strptime(dat$stop_time,"%Y-%m-%d %H:%M:%S")
dat$start_time<- strptime(dat$start_time,"%Y-%m-%d %H:%M:%S")

## CALCULATE EFFORT
dat$effort<-as.numeric(dat$stop_time-dat$start_time)/60 # EFFORT IN MINUTES
dat<-subset(dat,effort>0)
names(dat)<-tolower(names(dat))

##FIX TYPO IN COLUMN NAME
colnames(dat)[14]<-"standard_gear"


######################### STUDY AREA ###########

if(studyArea==TRUE)# CAN BE LONG... 
    {
    # MISSOURI RIVER WATERSHED

    ## READ IN US HUCS
    #hucs<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages", "hucs00p020")
    ## SUBSET OUT MO RIVER HUC	
    #missouri<- subset(hucs, hucs@data$HUC2==10)
    ## EXTRACT COORDINATES FOR EACH HUC CENTROID
    #lps<- coordinates(missouri)
    ## MAKE AN ID TO DISSOLVE HUCS WITH
    #ID<- rep(1, nrow(lps))
    ## MAKE A MO RIVER WATERSHED FOR PLOTING
    #missouri_ws <- unionSpatialPolygons(missouri,ID)
    ## COERCE TO SPATIALPOLYGONDATAFRAME
    #ppp<-as(missouri_ws, "SpatialPolygonsDataFrame")
    #writeOGR(ppp, 
    #    "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    #    "missouri_ws", driver="ESRI Shapefile")

    # LOAD PROCESSED US POLYGON
    watershed<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
        "missouri_ws")	

    # LOAD AND CLEAN COVERAGE OF US STATES

    ## STATES
    #states<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages", "statep010")
    ## SUBSET CONTIGUOUS US STATES
    #states<- subset(states, !(states@data$STATE %in% c("Alaska", "Hawaii", "Puerto Rico",
    #	"U.S. Virgin Islands")))
    ## REMOVE SOME ISLANDS
    #states<- subset(states,!(AREA %in% c(1.166,0.994,0.790,11.527,2.739,0.443,0.065,0.211)))
    ## EXTRACT COORDINES FOR EACH STATE CENTROID
    #lps<- coordinates(states)
    ## MAKE AN ID TO DISSOLVE HUCS WITH
    #ID<- rep(1, nrow(lps))
    # DISSOLVE STATES FOR OUTLINE OF U.S.
    #states_dissolve <- unionSpatialPolygons(states,ID)	
    ## COERCE TO SPATIALPOLYGONDATAFRAME
    #ppp<-as(states_dissolve, "SpatialPolygonsDataFrame")
    #writeOGR(ppp, 
    #    "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    #    "states_dissolve", driver="ESRI Shapefile")

    # LOAD PROCESSED US POLYGON
    us<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
        "states_dissolve")	


    # LOAD DATA PALLID STURGEON MANAGEMENT UNITS
    manunits<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/analysis/GIS", 
        "PallidSturgeon_ManagementUnits")
        
    # HATCHERY DATA
    chan<-odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
    hatcheries<- sqlFetch(chan, "Hatchery_latlongs")

    # WATER BODIES 

    ## LOAD DATA
    #wb<- readOGR("C:/Users/mcolvin/My Documents/projects/gis coverages/National Coverages", 
    #    "wtrbdyp010")
    ## SUBSET OUT MISSOURI RIVER RESERVOIRS
    #reservoirs<- subset(wb, Name %in%c("Lake Sakakawea",
    #    "Lake Francis Case","Lake Oahe", "Lake Sharpe", "Fort Peck Lake",
    #    "Lewis and Clark Lake"))
    ## COERCE TO SPATIALPOLYGONDATAFRAME
    #ppp<-as(reservoirs, "SpatialPolygonsDataFrame")
    #writeOGR(ppp, 
    #    "C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
    #    "reservoirs", driver="ESRI Shapefile")




    ## LOAD PROCESSED MAJOR WATERBODIES
    reservoirs<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
        "reservoirs")    
        
    ## LOAD BEND DATA 
    bends<- readOGR("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/GIS", 
        "bends_sp_ll")
    }