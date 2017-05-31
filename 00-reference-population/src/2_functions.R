
# FUNCTION TO DISTRIBUTE FISH AMONG SEGEMENTS AND THEN BENDS
reference_population<- function(segs=c(1,2,3,4,7,8,9,10,13,14),
    bends=NULL,
    fish_density=1,
    nyears=10,
    phi=0.95)
    {
    # this function allocates fish to bends within a segment
    # probabilistically given bend weights
    
    # inputs
    ## segment: segment [1,2,3,4,7,8,9,10,13,14]
    ##     fish_density: density of fish within segment; fish/rkm
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
        dim(phi)[1]!=nyears) #should this be dim(phi)[2]!=nyears?
        {return(print("Survival(phi) needs to be a matrix \n
        with rows equal to the number of segments \n
        and the same number of columns as years-1 to simulate"))}

    # GET BEND INFORMATION
    tmp<- subset(bends, b_segment %in% segs)
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



# FUNCTION TO DETERMINE CATCH COUNTS IN A BEND FOR ALL STANDARD, COMMON GEARS
catch_counts<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                        bends=NULL,
                        n=NULL,
                        effort=NULL)
{ 
  # this function calculates the number of fish caught in
  # each bend within a segment probabilistically given bend
  # abundance and a gamma distribution for effort
  
  # a formula for catchability (q) still needs to be worked in
  # currently q is constant at 0.0002
  
  # inputs
  ## segs: segment [1,2,3,4,7,8,9,10,13,14]
  ## bends: bend data that lists each bend within segment
  ## n: a matrix of abundance in a particular bend (row) in a 
  ##    particular year (column)
  ## effort: a dataframe summarizing effort (in minutes) for 
  ##    standard gears over the duration of the PSPAP 
  
  # outputs
  ## out: an array of numerical values representing catch
  ##    out[i,j,k] is the catch for bend i in year j with gear k
  ##    k=1 GN14
  ##    k=2 GN18
  ##    k=3 GN41
  ##    k=4 GN81
  ##    k=5 MF
  ##    k=6 OT16
  ##    k=7 TLC1
  ##    k=8 TLC2
  ##    k=9 TN
  
  # assumptions--unless these are somehow factored into q, we have:
  ## no movement
  ## no recruitment
  ## no mortality throughout the year;
  ##  this occurs at very end when moving to the next year
  
  # FIND BASIN ATTRIBUTE FOR EACH DATA POINT
    # ORDER DATA AS IN REFERENCE POPULATION FUNCTION
    tmp<- subset(bends, b_segment %in% segs)
    tmp<- tmp[order(tmp$b_segment, tmp$bend_num),]
    # FIND WHERE IN THE DATA THE BASIN CHANGES FROM UB TO LB
    indx<-min(which(tmp$b_segment %in% c(7,8,9,11,10,13,14)))-1
    if(indx==Inf) indx<-nrow(n)
  
  # DIVIDE UP EFFORT BY BASIN
    eftLB<-subset(effort,basin=="LB")
    eftUB<-subset(effort,basin=="UB")
    # QUICK ERROR CHECK
    if(nrow(eftLB)!=nrow(eftUB)) 
      {return(print("LB gears differ from UB gears.  Effort \n
                may not have been cleaned up."))}
  
  # DETERMINE CATCHABILITY (0<q<1/f)
    q=0.0004
  
  # DETERMINE CATCH
    out<-array(0,c(dim(n),nrow(effort)/2,2))
  
    C<-function(q,N,f){q*N*f}
  
    for(j in 1:ncol(n))
      {
        fUB<-mapply(rgamma, n=indx,shape=eftUB$gamma_shape, rate=eftUB$gamma_rate)
        fLB<-mapply(rgamma, n=(nrow(n)-indx),shape=eftLB$gamma_shape, rate=eftLB$gamma_rate)
        f<-rbind(fUB,fLB)
        out[,j,,1]<-round(mapply(C,q=q,N=n[,j], f=f))
        out[,j,,2]<-f
      }

    return(out) 
}
    
  
# FUNCTION TO DETERMINE WHICH BENDS WITHIN A SEGMENT TO SAMPLE
bend_samples<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                       bends=NULL,
                       n=NULL)
{
  # GET BEND INFORMATION
  tmp<-subset(bends, b_segment %in% segs)
  tmp<-tmp[order(tmp$b_segment, tmp$bend_num),]
  bends_in_segs<-aggregate(bend_num~b_segment,tmp,length)
  bends_in_segs$start<-1
  for(i in 1:nrow(bends_in_segs)) 
    {
      bends_in_segs$stop[i]<-sum(bends_in_segs$bend_num[1:i])
      if(i>1) bends_in_segs$start[i]<-bends_in_segs$stop[i-1]+1
    }
  # SAMPLE NUMBERS IN TABLE A1 IN 
  #   PSPAP_Vol_1.8.FEB 2017_Welker_Drobish_Williams.pdf
  bends_in_segs$samp_num<-c(0, 12, 21, 12, 12, 15, 20, 10, 11, 14)
  # SAMPLE SIZE THAT MEETS A MINIMUM OF 25.2% OF BENDS IN SEGMENT
  bends_in_segs$one_fourth<-ceiling(.252*bends_in_segs$bend_num)
  # COMPARISON (STARRED IF PERCENTAGE IS LARGER THAN TABLE VALUE)
  bends_in_segs$flag<-ifelse(bends_in_segs$one_fourth>bends_in_segs$samp_num, "*", " ")
  
  # DETERMINE WHICH BENDS IN A SEGMENT TO SAMPLE
  sampled<-matrix(0,nrow(n), ncol(n))
  for(j in 1:ncol(n))
    {
      sample_bends<-NULL
      for(k in 1:nrow(bends_in_segs)) 
        {
          sample_bends<-c(sample_bends,sample(c(bends_in_segs$start[k]:bends_in_segs$stop[k]), bends_in_segs$samp_num[k]))
        }
      for(i in 1:nrow(n))
        {
          sampled[i,j]<-ifelse(any(sample_bends==i), 1, 0)
        }
    } 
  
  #FIND WHERE IN THE DATA THE BASIN CHANGES FROM UB TO LB (FOR RPMA INFO)
  indx<-min(which(tmp$b_segment %in% c(7,8,9,11,10,13,14)))-1
  if(indx==Inf) indx<-nrow(n)
  
  # PUT OUTPUT IN LONG FORM
  RPMA<-rep(c(rep(2,indx),rep(4,nrow(n)-indx)),ncol(n))
  SEGMENT<-rep(tmp$b_segment, ncol(n))
  BEND<-rep(tmp$bend_num, ncol(n))
  B_ABUNDANCE<-c(n[,1:ncol(n)])
  SAMPLED<-c(sampled[,1:ncol(n)])
  YEAR<-c(mapply(rep, 0:(ncol(n)-1), MoreArgs=list(times=nrow(n))))
  out<-data.frame(RPMA, SEGMENT, BEND, YEAR, B_ABUNDANCE, SAMPLED)
  
  #ADD SEGMENT ABUNDANCE
  s_abun<-aggregate(out$B_ABUNDANCE, by=list(SEGMENT,YEAR),sum)
  s_abun$bend_num<-rep(bends_in_segs$bend_num, nrow(bends_in_segs))
  out$S_ABUNDANCE<-unlist(mapply(rep, x=s_abun$x, times=s_abun$bend_num))

  #ADD RPMA ABUNDANCE
  bends_in_segs$RPMA<-ifelse(bends_in_segs$b_segment==1|bends_in_segs$b_segment==2|bends_in_segs$b_segment==3|bends_in_segs$b_segment==4, 2, 4)
  r_abun<-aggregate(out$B_ABUNDANCE, by=list(out$RPMA,out$YEAR),sum)
  r_abun$bend_num<-rep(c(indx,nrow(n)-indx),nrow(r_abun)/2)
  out$R_ABUNDANCE<-unlist(mapply(rep, x=r_abun$x, times=r_abun$bend_num))
  
  #NAMES TO LOWERCASE
  names(out)<- tolower(names(out))
  return(out)
  }

samp_dat<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                   bends=NULL,
                   fish_density=1,
                   nyears=10,
                   phi=0.95,
                   effort=NULL)
{
  #OBTAIN SIMPULATED REFERENCE POPULATION
  sim_pop<-reference_population(segs=segs,
                                bends=bends,# BENDS DATAFRAME
                                fish_density=10, # FISH DENSITY PER RKM
                                nyears=nyears, #NUMBER OF YEARS TO PROJECT
                                phi=phi)
  #DETERMINE WHICH BENDS TO SAMPLE
  sim_samp<-bend_samples(segs=segs,bends=bends,n=sim_pop)
  
  #ADD CATCH AND EFFORT FOR TROTLINE TLC1 (k=7)
  sim_catch<-catch_counts(segs=segs,
                          bends=bends,
                          n=sim_pop,
                          effort=effort)
  sim_effort<-sim_catch[,,7,2]
  EFFORT<-c(sim_effort[,1:ncol(sim_effort)])
  EFFORT<-ifelse(sim_samp$sampled==1,EFFORT,NA)
  sim_catch<-sim_catch[,,7,1]
  CATCH<-c(sim_catch[,1:ncol(sim_catch)])
  CATCH<-ifelse(sim_samp$sampled==1,CATCH,NA)
  
  out<-data.frame(sim_samp[,1:6], EFFORT, CATCH, sim_samp[,7:8])
  names(out)<- tolower(names(out))
  return(out)
}