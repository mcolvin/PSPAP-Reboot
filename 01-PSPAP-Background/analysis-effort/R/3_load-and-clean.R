

# CHECK TO SEE IF PROCESSED
# EFFORT DATA EXISTS
checksum<- "FAIL" 
fn<- dir("./output")
## LOAD PREPROCESSED DATA IF EXISTS
## IT TAKES A LONG TIME TO PROCESS THE DATA
## SO IT HAS BEEN PROCESSED AND SAVED TO AN RDS
## FILE TO QUICKLY IMPORT. THE HASH IS AN MD5 CHECK SUM
## (I.E., A FINGERPRINT) TO COMPARE THE IMPORTED DATA 
## TO THE CHECK SUM. 
if("dat.RDS" %in% fn)
    {
    # LOAD PROCESSED DATA
    dat<-readRDS("./output/dat.RDS")
    hash1<-readRDS("./output/hash.RDS")
    checksum<- ifelse(hash1==digest(dat,algo="md5", serialize=TRUE),"PASS","FAIL")
    }

# IMPORT EFFORT DATA
if(!("dat.RDS" %in% fn) | 
    checksum=="FAIL")
    {
    ## CODE TO COMMUNICATE WITH LOCAL PSPAP DATABASE 
    com3<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
    dat<-sqlFetch(com3, "Gear-Specific-Effort")
    ## CONVERT TO CHARACTER
    dat$STARTTIME<- as.character(dat$STARTTIME)
    dat$STOPTIME<- as.character(dat$STOPTIME)

    ## FILL NON TIME FORMATS WITH NA
    dat[-(grep(":",dat$STARTTIME)),]$STARTTIME<- NA
    dat[-(grep(":",dat$STOPTIME)),]$STOPTIME<- NA
    dat$STARTTIME<- toupper(dat$STARTTIME)
    dat$STOPTIME<-toupper(dat$STOPTIME)

    ## NEW START/STOP FIELDS
    ## FOR START AS FRACTIONAL HOUR
    dat$start_time<- NA
    dat$stop_time<- NA

    ## CONVERT ALL TO HOUR FRACTION TIME
    ## START TIME
    dat$start_time<-unlist(lapply(1:nrow(dat),function(x)
        {
        xx<-unlist(
            strsplit(
                dat[x,]$STARTTIME,":"
                ))
        hr<-as.numeric(xx[1])+ 
            as.numeric(xx[2])/60
        return(hr)
        }))

    ## STOP TIME
    dat$stop_time<-unlist(lapply(1:nrow(dat),function(x)
        {
        xx<-unlist(
            strsplit(
                dat[x,]$STOPTIME,":"
                ))
        hr<-as.numeric(xx[1])+ 
            as.numeric(xx[2])/60
        return(hr)
        }))  
        
    ## FORMAT PM
    ## CONVERT HR:MIN:SEC TO HOURS
    indx<-grep("PM",dat$STARTTIME)
    dat[indx,]$start_time<-dat[indx,]$start_time+12

    indx<-grep("PM",dat$STOPTIME)
    dat[indx,]$stop_time<-dat[indx,]$stop_time+12

    ## CALCULATE EFFORT
    dat$effort<-dat$stop_time-dat$start_time

    ## LOAD GEAR DATA
    gear_dat<-sqlFetch(com3, "Gear-Meta-Data")
    names(gear_dat)[2]<-"gear_type"
    ## MERGE GEAR TYPE WITH EFFORT DATA
    dat<- merge(dat,gear_dat[,c(2,3,4,6,9)],by.x="GEAR",
        by.y="gear_code",all.x=TRUE)
    names(dat)<-tolower(names(dat))
    saveRDS(dat,"./output/dat.RDS")
    hash<-digest(dat,algo="md5", serialize=TRUE)# md5 check sum, fingerprint data
    saveRDS(hash,"./output/hash.RDS")# save fingerprint to check later 
    }
	

  