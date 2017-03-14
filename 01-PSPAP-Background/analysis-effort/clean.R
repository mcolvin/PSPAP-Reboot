

if(hash!=hash_imp)
    {
    # CONVERT TO CHARACTER
    dat$STARTTIME<- as.character(dat$STARTTIME)
    dat$STOPTIME<- as.character(dat$STOPTIME)



    # FILL NON TIME FORMATS WITH NA
    dat[-(grep(":",dat$STARTTIME)),]$STARTTIME<- NA
    dat[-(grep(":",dat$STOPTIME)),]$STOPTIME<- NA
    dat$STARTTIME<- toupper(dat$STARTTIME)
    dat$STOPTIME<-toupper(dat$STOPTIME)

    # NEW START/STOP FIELDS
    # FOR START AS FRACTIONAL HOUR
    dat$start_time<- NA
    dat$stop_time<- NA

    # CONVERT ALL TO HOUR FRACTION TIME
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
        })  )  
        
    # FORMAT PM
    ## CONVERT HR:MIN:SEC TO HOURS
    indx<-grep("PM",dat$STARTTIME)
    dat[indx,]$start_time<-dat[indx,]$start_time+12

    indx<-grep("PM",dat$STOPTIME)
    dat[indx,]$stop_time<-dat[indx,]$stop_time+12

    
    
    ## CALCULATE EFFORT
    dat$effort<-dat$stop_time-dat$start_time
    
    # set night
    Gill net
    Mini fyke net
    Trammel net??
    Trot lines - over night but segs 1-4 8hrs or less
    Hoop nets
    Setlines/banklines
    
    cbind(table(subset(dat,effort<0)$GEAR),
    table(subset(dat,effort>0)$GEAR))
    
    subset(dat,effort>0 & GEAR =="TN")
    
    hash<-digest(dat,algo="md5", serialize=TRUE)
    saveRDS(dat,"./output/dat.RDS")
    saveRDS(hash,"./output/hash.RDS")

    }

  
  