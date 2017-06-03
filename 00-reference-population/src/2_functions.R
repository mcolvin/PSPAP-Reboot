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
    if(dim(phi)[1]!=length(segs) |
        dim(phi)[2]!=nyears-1)
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
    out<-list(out=out, bendMeta=tmp)
    return(out)# return relevant stuff
}






# FUNCTION TO DETERMINE CATCH COUNTS IN A BEND FOR ALL STANDARD, COMMON GEARS
catch_counts<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                        bends=NULL,
                        abund=NULL,
                        gears=c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN"),
                        catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.0002, 0.00004, 0.00004, 0.0002),
                        deployments=rep(8,9),
                        effort=NULL)
{ 
  # this function calculates the number of fish caught in
  # each bend within a segment probabilistically given bend
  # abundance and a gamma distribution for effort
  
  # a formula for catchability (q) still needs to be worked in
  # currently q is a vector of catchabilities by gear
  # Is the environment different enough that we need by basin as well?
  
  # inputs
  ## segs: segment [1,2,3,4,7,8,9,10,13,14]
  ## bends: bend data that lists each bend within segment
  ## n: a matrix of abundance in a particular bend (row) in a 
  ##    particular year (column)
  ## gears: a vector of sampling gears
  ## q: a vector of catchabilities by gear
  ##    must either match the vector of gears given or be of 
  ##    length 9 with q[k] equaling catchability of gear k where
  ##        k=1 GN14
  ##        k=2 GN18
  ##        k=3 GN41
  ##        k=4 GN81
  ##        k=5 MF
  ##        k=6 OT16
  ##        k=7 TLC1
  ##        k=8 TLC2
  ##        k=9 TN
  ## d: a vector of the number of deployments by gear
  ##    must either match the vector of gears given or be of
  ##    length 9 where d[k] is the number of deployments for 
  ##    gear k, as indicated above
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
  
  # ERROR HANDLING
  if(any(!(gears %in% effort$gear)))
  {return(print("Gears must be a vector subset of the gears: GN14, \n 
                 GN18, GN41, GN81, MF, OT16, TLC1, TLC2, TN"))}
  if(length(catchability)!=9 & length(catchability)!=length(gears))
  {return(print("Catchability needs to be a vector of length 9 (catchabilities for all possible standard
                  gears; see function description for gears and gear order) or a vector with
                  length equal to the length of the vector of gears (catchabilities for each 
                  given gear in the same order given)."))}
  if(length(deployments)!=9 & length(deployments)!=length(gears))
  {return(print("Deployments needs to be a vector of length 9 \n 
                   (number of deployments for all possible standard gears; \n 
                    see function description for gears and gear order)
                    or a vector with length equal to the length of \n
                    the vector of gears (deployment numbers for each \n
                    given gear in the same order given)."))}
  
  # REORDER GEARS TO MATCH EFFORT AND ELIMINATE ANY REPEATS
  g<-gears[order(unique(gears))]
  
  # FIND BASIN ATTRIBUTE FOR EACH DATA POINT
    # ORDER DATA AS IN REFERENCE POPULATION FUNCTION
    tmp<- subset(bends, b_segment %in% segs)
    tmp<- tmp[order(tmp$b_segment, tmp$bend_num),]
    # FIND WHERE IN THE DATA THE BASIN CHANGES FROM UB TO LB
    indx<-min(which(tmp$b_segment %in% c(7,8,9,11,10,13,14)))-1
    if(indx==Inf) indx<-nrow(abund)
  
  # DIVIDE UP EFFORT BY BASIN
    eft<-effort[effort$gear %in% gears,]
    eftLB<-subset(eft,basin=="LB")
    eftUB<-subset(eft,basin=="UB")
    # QUICK ERROR CHECK
    if(nrow(eftLB)!=nrow(eftUB)) 
      {return(print("LB gears differ from UB gears.  Effort \n
                may not have been cleaned up."))}
  
  # DETERMINE CATCHABILITY BY GEAR (0<q<1/f)
    if(length(catchability)==length(gears)) q<-catchability[order(unique(gears))] else q<-catchability[which(effort$gear[1:9] %in% gears)]
    q<-matrix(unlist(lapply(q, rep, times=nrow(abund))),c(nrow(abund),length(g)))
    
  # DETERMINE DEPLOYMENTS BY GEAR
    if(length(deployments)==length(gears)) d<-deployments[order(unique(gears))] else d<-deployments[which(effort$gear[1:9] %in% gears)]
    d<-matrix(unlist(lapply(d, rep, times=nrow(abund))),c(nrow(abund),length(g)))
  
  # DETERMINE CATCH
    out_a<-array(0,c(dim(abund),length(g),2))
  
    C<-function(q,N,f){q*N*f}
  
    for(j in 1:ncol(abund))
      {
        fUB<-mapply(rgamma, n=indx,shape=eftUB$gamma_shape, rate=eftUB$gamma_rate)
        fLB<-mapply(rgamma, n=(nrow(abund)-indx),shape=eftLB$gamma_shape, rate=eftLB$gamma_rate)
        f<-rbind(fUB,fLB)
        f<-d*f
        out_a[,j,,1]<-round(mapply(C,q=q,N=abund[,j], f=f))
        out_a[,j,,2]<-f
    }
    out<-list()
    for(k in 1:length(g))
    {
      out$effort[[g[k]]]<-out_a[,,k,2]
      out$catch[[g[k]]]<-out_a[,,k,1]
      # out[[g[k]]]$gear<-g[k]
      # out[[g[k]]]$catch<-out_a[,,k,1]
      # out[[g[k]]]$effort<-out_a[,,k,2] 
    }
    return(out) 
}
  




# FUNCTION TO DETERMINE WHICH BENDS WITHIN A SEGMENT TO SAMPLE
bend_samples<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                       bends=NULL,
                       abund=NULL)
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
  sampled<-matrix(0,nrow(abund), ncol(abund))
  for(j in 1:ncol(abund))
    {
      sample_bends<-NULL
      for(k in 1:nrow(bends_in_segs)) 
        {
          sample_bends<-c(sample_bends,sample(c(bends_in_segs$start[k]:bends_in_segs$stop[k]), bends_in_segs$samp_num[k]))
        }
      for(i in 1:nrow(abund))
        {
          sampled[i,j]<-ifelse(any(sample_bends==i), 1, 0)
        }
    } 
  
  #FIND WHERE IN THE DATA THE BASIN CHANGES FROM UB TO LB (FOR RPMA INFO)
  indx<-min(which(tmp$b_segment %in% c(7,8,9,11,10,13,14)))-1
  if(indx==Inf) indx<-nrow(abund)
  
  # PUT OUTPUT IN LONG FORM
  RPMA<-rep(c(rep(2,indx),rep(4,nrow(abund)-indx)),ncol(abund))
  SEGMENT<-rep(tmp$b_segment, ncol(abund))
  BEND<-rep(tmp$bend_num, ncol(abund))
  B_ABUNDANCE<-c(abund[,1:ncol(abund)])
  SAMPLED<-c(sampled[,1:ncol(abund)])
  YEAR<-c(mapply(rep, 0:(ncol(abund)-1), MoreArgs=list(times=nrow(abund))))
  out<-data.frame(RPMA, SEGMENT, BEND, YEAR, B_ABUNDANCE, SAMPLED)
  
  #ADD SEGMENT ABUNDANCE
  s_abund<-aggregate(out$B_ABUNDANCE, by=list(SEGMENT,YEAR),sum)
  s_abund$bend_num<-rep(bends_in_segs$bend_num, nrow(bends_in_segs))
  out$S_ABUNDANCE<-unlist(mapply(rep, x=s_abund$x, times=s_abund$bend_num))

  #ADD RPMA ABUNDANCE
  bends_in_segs$RPMA<-ifelse(bends_in_segs$b_segment %in% c(1,2,3,4), 2, 4)
  r_abund<-aggregate(out$B_ABUNDANCE, by=list(out$RPMA,out$YEAR),sum)
  r_abund$bend_num<-rep(c(indx,nrow(abund)-indx),nrow(r_abund)/2)
  out$R_ABUNDANCE<-unlist(mapply(rep, x=r_abund$x, times=r_abund$bend_num))
  
  #NAMES TO LOWERCASE
  names(out)<- tolower(names(out))
  return(out)
  }







samp_dat<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                   bends=NULL,
                   abund=NULL,
                   gears=c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN"),
                   catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.0002, 0.00004, 0.00004, 0.0002),
                   deployments=rep(8,9),
                   effort=NULL)
{
  #DETERMINE WHICH BENDS TO SAMPLE
  sim_samp<-bend_samples(segs=segs,bends=bends,abund=abund)
  
  #EXPAND SIM_SAMP TO INCLUDE RESULTS FOR REACH GEAR
  sim_samp<-rbind(sim_samp,sim_samp,sim_samp,sim_samp,sim_samp,sim_samp,sim_samp,sim_samp,sim_samp)
  g<-gears[order(unique(gears))]
  sim_samp<-sim_samp[1:(length(g)*nrow(abund)*ncol(abund)),]
  
  
  #ADD CATCH AND EFFORT FOR GEARS
  sim_catch<-catch_counts(segs=segs,
                          bends=bends,
                          abund=abund,
                          gears=gears,
                          catchability=catchability,
                          deployments=deployments,
                          effort=effort)
  
  GEAR<-unlist(lapply(g,rep,nrow(abund)*ncol(abund)))
  EFFORT<-unlist(sim_catch$effort, use.names = FALSE)
  EFFORT<-ifelse(sim_samp$sampled==1,EFFORT,NA)
  CATCH<-unlist(sim_catch$catch, use.names = FALSE)
  CATCH<-ifelse(sim_samp$sampled==1,CATCH,NA)
  #CATCH<-ifelse(EFFORT==0, NA, CATCH)
  #EFFORT<-ifelse(EFFORT==0, NA, EFFORT)
  CPUE<- CATCH/EFFORT
  
  out<-data.frame(sim_samp[,1:6], GEAR, EFFORT, CATCH, CPUE, sim_samp[,7:8])
  names(out)<- tolower(names(out))
  return(out)
}









# GETTING TREND

get.trnd<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                   bends=NULL, #UPLOADED
                   abund=NULL, #RUN REFERENCE_POPULATION FUNCTION TO GET
                   gears=c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN"),
                   catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.0002, 0.00004, 0.00004, 0.0002), #BY GEAR,
                   deployments=rep(8,9), #BY GEAR,
                   effort=NULL) #UPLOADED
{
  # GET CATCH SIMULATION DATA
  sim_dat<-samp_dat(segs=segs,
                    bends=bends,# BENDS DATAFRAME
                    abund=abund,
                    gears=gears,
                    catchability=catchability,
                    deployments=deployments,
                    effort=effort)
  
  # GET AVERAGE SEGMENT CPUE BY YEAR
  tmp<- aggregate(cpue~year+segment+gear,sim_dat,mean)
  tmp$segment<- as.factor(tmp$segment)
  tmp$lncpue<- log(tmp$cpue)
  
  # FIT LINEAR MODEL FOR TREND FOR EACH GEAR
  out<-lapply(gears,function(g)
    {
      fit<- lm(lncpue~segment+year, tmp, subset=gear==g)
      tmp2<- data.frame( 
        # THE GOODIES
        ## GEAR
        gear=g,
        ## TREND ESTIMATE
        trnd=coef(fit)['year'],
        ## STANDARD ERROR FOR TREND ESTIMATE
        se=summary(fit)$coefficients['year',2],
        ## PVALUE FOR TREND ESTIMATE
        pval=summary(fit)$coefficients['year',4]
        )
      }
    )
  # OUTPUT THE GOODIES
  return(out)
}
 
 