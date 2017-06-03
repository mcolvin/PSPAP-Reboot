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
            mean_dens=density_mean[which.max(as.numeric(year))],
            max_dens=max(d_upper, na.rm=TRUE),
            max_year=max(as.numeric(year)),
            min_mean=min(density_mean, na.rm=TRUE),
            max_mean=max(density_mean, na.rm=TRUE))
  tmp$min_dens<-mapply(min, tmp$min_dens, tmp$min_mean, na.rm=TRUE)
  tmp$max_dens<-mapply(max, tmp$max_dens, tmp$max_mean, na.rm=TRUE)
  tmp$segs<-1
  tmp$segs[which(tmp$segments=="8")]<-8
  tmp$segs[which(tmp$segments=="10")]<-10
  tmp<-tmp[order(tmp$segs),]
  tmp$segments<-ifelse(tmp$segments=="1, 2, 3, 4, LY", "1-4", ifelse(tmp$segments=="8", "7-9", "   10, 13, 14"))
  tmp<-tmp[,1:7]
  tmp$ref<-c("[1]", "[2]-[4]", "[5]", "[5]", "[6]", "[6]")
  return(tmp)
  }

}