figures<- function(n,...)
    {
  
    if(n==1)
        {
        S<- list(...)  
        pdat<- subset(dat, Segment==S) 
        pdat$GEAR<- factor(pdat$GEAR)
        pdat$BEND<- factor(pdat$BEND)
        XX<- dcast(pdat, Segment+SEASON+GEAR+BEND~year,
            value.var="BEND_INT",unique,
            fill=-99)
        XX[XX==-99]<-NA
        
        tmp<- reshape(XX,
            varying = names(XX) [5:ncol(XX)],
            v.names = "sampled",
            timevar= "year",
            times = names(XX) [5:ncol(XX)],
            direction = "long")
        tmp$year<- as.numeric(tmp$year)
        
        
        season<- unique(XX$SEASON)
        bend<- unique(XX$BEND)           
        xyplot(sampled~year|GEAR,tmp,group=BEND,type='b')
        }
    
    }