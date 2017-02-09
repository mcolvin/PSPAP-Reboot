tables<-function(n)
    {
    if(n==1)
        {
        # SEGMENTS WITHIN BASIN - UPPER BASIN SEGMENTS 1,2,3,4, 157 BENDS
        XX<-dcast(dat,RPMA + SEGMENT_ID+BEND~"tmp",value.var="tmp",sum)
        XX$tmp<-1
                
        XXX<- ddply(XX, .(RPMA,SEGMENT_ID),summarize,
            nbends=sum(tmp),
            bend_start = min(BEND),
            bend_stop= max(BEND))
        return(XXX)
        }
    if(n==2)
        {
        
        
        }
    
    
    
    }