tables<- function(n){

if(n==1)
    {
    # BEND SUMMARY TABLE
    tmp<-ddply(bends,.(basin,b_segment),summarize,
        n_bends=length(id),
        min_rkm=min(length.rkm),
        mean_rkm=min(length.rkm),
        maximum_rkm=min(length.rkm))     
    return(format(tmp,digits=2))

}

}