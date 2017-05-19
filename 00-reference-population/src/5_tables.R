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

}