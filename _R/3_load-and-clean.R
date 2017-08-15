## SOME CODE IS NOT NECESSARY FOR SPECIFIC 
## ANALYSES AND THEREFORE IS WRAPPED IN AN 
## IF STATEMENT SUCH THAT IF AN PARTICULARLY
## TIME CONSUMING LOAD AND CLEAN IS REQUIRED
## IT CAN BE CALLED BY SETTING AN OBJECT TO
## TRUE. IF THE OBJECT DOES NOT EXIST IT 
## THROWS AN ERROR IN THE IF STATEMENT AND 
## THE CODE BELOW CHECKS TO SEE IF THE OBJECT
## EXISTS AND IF NOT CREATS IT AND SETS IT TO
## FALSE SO LONG DATA PROCESSESSING IS NOT 
## PERFORMED.
##  1. studyArea-suppresses reading in GIS files
##  2. effort-suppresses the processing of effort 
if(exists("studyArea")==FALSE){studyArea<-FALSE}
if(exists("effort_data")==FALSE){effort_data<-FALSE}



## PC NAME FOR AUTOMATING FILE DIRECTORY FOR CRUNCH OR NOT...
pcname<- Sys.info()[['nodename']]     


#######################################################################
# READ IN BEND DATA FOR RPMA 2 (UPPER) AND 4 (LOWER)
# USED FOR MOVEMENT ANALYSIS
#######################################################################
bends<- read.csv("./_dat/bend-data.csv")

# make data.frame column names lower case
names(bends)<- tolower(names(bends))
bends<- subset(bends,b_segment %in% c(1,2,3,4,7,8,9,10,13,14))
bends<-bends[-157,]
## bend_start_rkm needs to be adjusted either upstream or downstream
## of segment 9 bend 65

bends<-bends[order(bends$lower_river_mile),]
# MAKE A PAIRWISE DISTANCE MATRICES FOR EACH RPMA
bends$center<- bends$bend_start_rkm+ bends$length.rkm/2
bends$b_id<- 1
## ADD WITHIN BEND ID, INCREASING ORDER MOVING UPSTREAM
bends$b_id[which(bends$rpma==2)]<- 1:length(which(bends$rpma==2))
bends$b_id[which(bends$rpma==4)]<- 1:length(which(bends$rpma==4))
## PAIRWISE DISTANCES AND DIRECTION FOR RPMA2
sp<- list()
tmp<-subset(bends,rpma==2)
tmp<- tmp[order(tmp$lower_river_mile),]
sp$dis$rpma2<- -1*abs(outer(tmp$center,tmp$center,"-"))
sp$direct$rpma2<- sp$dis$rpma2
sp$direct$rpma2[lower.tri(sp$direct$rpma2)]<--1
sp$direct$rpma2[upper.tri(sp$direct$rpma2)]<- 1

## PAIRWISE DISTANCES AND DIRECTION FOR RPMA4
tmp<-subset(bends,rpma==4)
tmp<- tmp[order(tmp$lower_river_mile),]
sp$dis$rpma4<- -1*abs(outer(tmp$center,tmp$center,"-"))
sp$direct$rpma4<- sp$dis$rpma4
sp$direct$rpma4[lower.tri(sp$direct$rpma4)]<--1
sp$direct$rpma4[upper.tri(sp$direct$rpma4)]<- 1





#######################################################################
# READ IN EFFORT DATA FROM 01-PSPAP-Background Analysis
## THIS DATA HAS ALREADY BEEN PROCESSED AND IS
## AN OUTPUT FROM THE EFFORT ANALYSIS
#######################################################################
effort<- read.table("_output/effort_dat.csv")
effort$rpma<- ifelse(effort$basin=="UB",2,4)

# READ IN DENSITY DATA
dens<-read.csv("./_dat/fish_density.csv")
# REMOVE WU AND HOLAN DATA
dens<-rbind(dens[1:4,],dens[7:nrow(dens),])
# MAKE COLUMN NAMES LOWER CASE
names(dens)<-tolower(names(dens))
# GENERATE INITIAL DENSITY DATA BY SEGMENT
## FIND THE MOST CURRENT MEAN DENSITY BY SEGMENT
init_dens<-ddply(dens, .(rpma,segments,fish_type), summarize,
                 expected_dens=density_mean[which.max(as.numeric(year))])
## COMBINE HATCHERY AND WILD DENSITIES TO GET OVERALL SEGMENT DENSITY
init_dens<-aggregate(expected_dens~rpma+segments, init_dens, sum)
## EXPAND TO HAVE ONE ROW FOR EACH SEGMENT
init_dens<-init_dens[rep(seq_len(nrow(init_dens)), times=c(4,3,3)),]
init_dens$segments<-c(1:4, 7:9, 10, 13, 14)
colnames(init_dens)[2]<-"b_segment"




############## EFFORT ANALYSIS #####################
if(effort_data==TRUE)# long time to run, set to true in Rmd to run
    {
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
    dat<- subset(dat, !(is.na(STARTTIME)))
    dat<- subset(dat, !(is.na(STOPTIME)))
    dat<- subset(dat, !(is.na(SETDATE))) 
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

    dat$gearC<-as.factor(unlist(lapply(dat$gear, function(x) 
        {
        ifelse(any(common==x), 
            x<-paste0(x,"*"),
            ifelse((dat$basin=="LB" && any(LBcommon==x))||(dat$basin=="UB" && x=="TLO1"),x<-paste0(x,"*"),
                unlist(levels(dat$gear))[x]))
        }
    )))
    saveRDS(dat,"output/effort-data.Rds")
    }

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