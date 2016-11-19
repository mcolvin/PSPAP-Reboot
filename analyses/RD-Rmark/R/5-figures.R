

figures<-function(n)
    {
    if(n==1)
        {
        plot(derived$estimate,
            pch=19,ylim=c(min(derived$lcl),max(derived$ucl)))
        segments(c(1:length(derived$ucl)),derived$lcl, c(1:length(derived$lcl)),derived$ucl )
               
        }
    }
    
    
