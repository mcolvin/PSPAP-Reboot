tables<- function(n)
    {
    if(n=="f1")
        {# TABLE OF GEAR USE OVER TIME BY BASIN FOR FIGURE 1
        dat$tmp<-1 # TO SUM FOR COUNTS
        tmp<- dcast(dat, basin+gear+gear_id+yr~"freq",value.var="tmp",sum)
        return(tmp)
        }
    if(n==1)
        {
        dat$tmp<-1 # TO SUM FOR COUNTS
        tmp<- dcast(dat, basin+gear+gear_id~yr,value.var="tmp",sum)
        return(tmp)      
        }
    }