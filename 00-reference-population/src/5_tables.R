tables<- function(n){

if(n==1)
    {
    # BEND SUMMARY TABLE
    ## THIS TABLE SUMMARIZES THE SAMPLING UNITS
    ## FOR BENDS IN RPMAN 2 (UPPER) AND 4 (LOWER)
    tmp<-ddply(bends,.(basin,b_segment),summarize,
        n_bends=length(id),
        min_rkm=min(length.rkm),
        mean_rkm=min(length.rkm),
        maximum_rkm=min(length.rkm))     
    return(format(tmp,digits=2))
    }
if(n==2)
  {
  tmp<-ddply(dens,.(rpma, segments, fish_type),summarize,
            min_dens=min(d_lower, na.rm=TRUE),
            mean_dens=mean(density_mean, na.rm=TRUE),
            max_dens=max(d_upper, na.rm=TRUE),
            min_mean=min(density_mean, na.rm=TRUE),
            max_mean=max(density_mean, na.rm=TRUE), 
            max_year=max(as.numeric(year)))
  tmp$min_dens<-mapply(min, tmp$min_dens, tmp$min_mean, na.rm=TRUE)
  tmp$max_dens<-mapply(max, tmp$max_dens, tmp$max_mean, na.rm=TRUE)
  tmp<-tmp[,1:6]
  tmp$recent_mean<-c(91.6, 0.3, 5.5, 0.9, 32.3, 5.7)
  tmp$recent_year<-c(2013, 2008, 2013,2013, 2010, 2010)
  return(format(tmp,digits=1))
  }

}