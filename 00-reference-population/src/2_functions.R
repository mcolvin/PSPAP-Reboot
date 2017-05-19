
# FUNCTION TO DISTRIBUTE FISH AMONG SEGEMENTS AND THEN BENDS
reference_population<- function(segs=c(1,2,3,4,7,8,9,11,10,13,14),
    bends=NULL,
    fish_density=1,
    nyears=10,
    phi=0.95)
    {
    # this function allocates fish to bends within a segment
    # probabilistically given bend weights
    
    # inputs
    ## segment: segment [1,2,3,4,7,8,9,10,13,14]
    ## 
    fish_density: density of fish within segment; fish/rkm
    ## type: input for fish type [hatchery, natural]
    
    # outputs
    ## out: a matrix where each row is a bend and 
    ##   each column represents a year; number
    
    # assumptions
    ## no movement
    ## no recruitment
    ## survival homogenous for individuals
    
    ## ERROR HANDLING
    if(dim(phi)[1]!=length(segs) &
        dim(phi)[1]!=nyears)
        {return(print("Survival(phi) needs to be a matrix \n
        with rows equal to the number of segments \n
        and the same number of columns as years-1 to simulate"))}

    # GET BEND INFORMATION
    tmp<- subset(bends, b_segment%in% segs)
    tmp<- tmp[order(tmp$b_segment, tmp$bend_num),]
    bends_in_segs<-aggregate(bend_num~b_segment,tmp,length) 
    phi_indx<-rep(1:nrow(bends_in_segs),bends_in_segs$bend_num)
    # MATRIX OF ABUNDANCES TO RETURN
    out<- matrix(0,nrow(tmp), nyears)
     
    # INITIAL ABUNDANCES
    ## PULL NUMBER FROM A POISSON AFTER ADJUSTING
    ## DENSITY FOR BEND SIZE
    out[,1]<- rpois(nrow(out),
        lambda=fish_density*tmp$length.rkm)
    # SIMULATE DYNAMICS
    ## SURVIVAL
    for(i in 2:nyears)
        {
        # SET UP VECTOR OF SEGMENT
        # SPECIFIC SURVIVALS        
        phi_t<- phi[phi_indx,i-1]
        out[,i]<- rbinom(nrow(out),
            size=out[,i-1],
            prob=phi_t)
        }
    return(out)# return relevant stuff
    }
    
    
   












