# FUNCTIONS IN THIS SCRIPT
## EFFORT DISTRIBUTIONS
###  1. dfitfunLB
###  2. dfitfunuB
## RELATIVE ABUNDANCE (CPUE)
###  3. reference_population
###  4. catch_counts
###  5. bend_samples
###  6. samp_dat
###  7. get.trnd
## CAPTURE RECAPTURE FUNCTIONS
###  8. sim_ch
###  9. estimate
### 10. plot_metrics

# EFFORT DISTRIBUTIONS
## FIT DISTRIBUTIONS TO EFFORT DATA 
dfitfun<-function(x,dat,basin,gears)
  {
    datBgear<-subset(dat, basin==basin & gear==gears[x])
    dfit<-fitdistr(datBgear$effort, "gamma")
    #Define Shape and Rate Based on Distribution Fitting
    s<-as.numeric(unlist(dfit)[1])
    r<-as.numeric(unlist(dfit)[2])
    return(c(s,r))
  }


## 1.  
dfitfunLB<-function(x)
    {
    datLBgear<-subset(datLB, gear==LBgears[x])
    dfit<-fitdistr(datLBgear$effort, "gamma")
    #Define Shape and Rate Based on Distribution Fitting
    s<-as.numeric(unlist(dfit)[1])
    r<-as.numeric(unlist(dfit)[2])
    return(c(s,r))
    }

## 2.
dfitfunUB<-function(x)
    {
    datUBgear<-subset(datUB, gear==UBgears[x])
    dfit<-fitdistr(datUBgear$effort, "gamma")
    #Define Shape and Rate Based on Distribution Fitting
    s<-as.numeric(unlist(dfit)[1])
    r<-as.numeric(unlist(dfit)[2])
    return(c(s,r))
    }



# RELATIVE ABUNDANCE (CPUE)
## 3. FUNCTION TO DISTRIBUTE FISH AMONG SEGEMENTS
## AND THEN BENDS
reference_population<- function(inputs,...)
    {
    segs<-inputs$segs
    bends<-inputs$bends
    fish_density<-inputs$fish_density
    nyears<-inputs$nyears
    phi<-inputs$phi
    initial_length<-inputs$initial_length
    mv_beta0<-inputs$mv_beta0
    mv_beta1<-inputs$mv_beta1
    dis<- inputs$dis
    #direct<- inputs$direct #UNUSED
    
    # this function allocates fish to bends within a segment
    # probabilistically given bend weights and determines the survival
    # of fish within bend over nyears given survival probabilities
    
    # inputs
    ## segment: segment [1,2,3,4,7,8,9,10,13,14]
    ## fish_density: initial density data from load-and-clean; fish/rkm
    ## type: input for fish type [hatchery, natural]
    ## bends: bend data from load-and-clean
    ## nyears: number of years to simulate the population for
    ## phi: a matrix of survival probabilities by segment (rows) and 
    ##    year (cols)
    ## lower: a list of growth values for the lower basin:
    ##    $ln_Linf_mu: mean ln(Linf) value for growth equation
    ##    $ln_k_mu: mean ln(k) value for growth equation
    ##    $vcv: variance and covariances for Linf and k on a natural log scale
    ## upper: a list of growth values for the upper basin:
    ##    $ln_Linf_mu: mean ln(Linf) value for growth equation
    ##    $ln_k_mu: mean ln(k) value for growth equation
    ##    $vcv: variance and covariances for Linf and k on a natural log scale
    ## initial_length: functions to simulate initial length given an
    ##    empirical distribution of segment specific lengths
    ## mv_beta0:
    ## mv_beta1:
    ## dis:
    
    # outputs
    ## out: a list of 3 objects:
    ##  $out: a matrix where each row is a bend and 
    ##    each column represents a year; number
    ##  $bendMeta: a dataframe including the information in "bends"
    ##    with expected initial segment density (from init_dens), 
    ##    initial bend abundance, and segment index columns added
    ##  $individual_meta: a dataframe including the information in "bendMeta"
    ##    but expanded to include one row for each individual fish
    ##  $Z: a matrix with entries for individual survival status (0=Dead,
    ##    1=Alive) where each row represents a fish living and each column
    ##    represents a year 
    ##  $BND: a matrix where each row is an individual fish (matching up with
    ##    the entries from "individual_meta"), each column is a year, and each
    ##    entry is a number from 1 to 471 which indicates the bend the fish was
    ##    living in during the given year; NAs are entered for dead fish
    ##  $l: a matrix where each row is an individual fish (matching up with
    ##    the entries from "individual_meta"), each column is a year, and each
    ##    entry is the length of the fish during the given year; NAs are entered
    ##    for dead fish
    ##  $inputs: a list of the inputs used to create the reference population

    
    # assumptions
    ## no movement within years
    ## no recruitment
    ## survival homogenous for individuals
    
    ## ERROR HANDLING
    ### PHI
    if(dim(phi)[1]!=length(segs) |
        dim(phi)[2]!=nyears-1)
        {return(print("Survival(phi) needs to be a matrix \n
        with rows equal to the number of segments \n
        and the same number of columns as years-1 to simulate"))}
    ### INITIAL FISH DENSITY
    if(nrow(fish_density)!=length(segs))
      {return(print("Initial fish density (fish_density) needs to be \n
        a dataframe of densities by segment with number of \n
        rows equal to the number of segments."))}
    ### GROWTH
    if(dim(inputs$lower$vcv)[1]!=2|dim(inputs$lower$vcv)[2]!=2)
      {return("The lower variance covariance \n
        matrix needs to be 2x2 square")}
    if(dim(inputs$upper$vcv)[1]!=2|dim(inputs$upper$vcv)[2]!=2)
      {return("The upper variance covariance \n
        matrix needs to be 2x2 square")}
    #if(dim(vbgf_vcv)[1]!=2|dim(vbgf_vcv)[2]!=2){return("The variance covariance \n
    #    matrix needs to be 2x2 square")}
    #if(dim(vbgf_vcv)[3]!=10){return("There needs to be 10 2x2 variance covariance \n
    #    matrices for each segment")}
    #if(length(Linf)!=10){return("Linf needs to be a vector of 10 values for each segment")}
    #if(length(k)!=10){return("k needs to be a vector of 10 values for each segment")}
    ## END: ERROR HANDLING   
    
    
    # GET BEND INFORMATION
    tmp<- subset(bends, b_segment %in% segs)
    tmp<- tmp[order(tmp$id),]## CRITICAL
    bends_in_segs<-aggregate(bend_num~b_segment,tmp,length) 
    bends_in_segs$phi_indx<-1:nrow(bends_in_segs)
    
    tmp<-merge(tmp, bends_in_segs[,-2],by="b_segment",all.x=TRUE)
    
    # ADD INITIAL DENSITIES TO TMP
    tmp<-merge(tmp, fish_density,by=c("rpma", "b_segment"),all.x=TRUE)
  
    # INITIAL ABUNDANCES
    ## PULL NUMBER FROM A POISSON AFTER ADJUSTING
    ## DENSITY FOR BEND SIZE
    tmp$N_ini<-rpois(nrow(tmp),
        lambda=tmp$expected_dens*tmp$length.rkm)
    tmp<-tmp[order(tmp$id),] #CRITICAL
    ## ASSIGN A BEND TO EACH INVIDUAL 
    ### EXPAND BENDS FOR EACH FISH
    individual_meta<- as.data.frame(
        lapply(tmp,function(x) rep(x,tmp$N_ini)))
    ### ADD INDIVIDUAL FISH ID
    individual_meta$fish_id<-1:nrow(individual_meta)
    ## ASSIGN GROWTH PARAMETERS TO EACH INDIVIDUAL
    ln_Linf<-ifelse(segs %in% c(1:4), inputs$upper$ln_Linf_mu,
                    inputs$lower$ln_Linf_mu) # a vector of mean Linf values, one entry for each segment
    ln_k<-ifelse(segs %in% c(1:4), inputs$upper$ln_k_mu, inputs$lower$ln_k_mu)
      # a vector of mean k values, one entry for each segment
    ln_B<-array(0,dim=c(2,2,length(segs)))
    for(i in 1:length(segs))
        {
        if(segs[i] %in% c(1:4)) 
          {
          ln_B[,,i]<-eigen(inputs$upper$vcv)$vectors%*%
            matrix(c(sqrt(eigen(inputs$upper$vcv)$values[1]),0,0,
                     sqrt(eigen(inputs$upper$vcv)$values[2])),2,2)
          }
        if(segs[i] %in% c(7:10,13,14))
          {
          ln_B[,,i]<-eigen(inputs$lower$vcv)$vectors%*%
            matrix(c(sqrt(eigen(inputs$lower$vcv)$values[1]),0,0,
                   sqrt(eigen(inputs$lower$vcv)$values[2])),2,2)
          }
        } # an array of variance and covariances for Linf and k
            # each matrix is for a segment
    ln_vals<-lapply(1:nrow(individual_meta),function(m)
    {#DRAWN FROM MIDDLE 80% OF BIVARIATE NORMAL (ELLIPSE)
      z1<-rtruncnorm(1, -sqrt(-2*log(0.2)), sqrt(-2*log(0.2)), mean=0, sd=1)
      z2<-rtruncnorm(n=1, a=-sqrt(-2*log(0.2)-z1^2), b=sqrt(-2*log(0.2)-z1^2), 
                     mean=0, sd=1)
      X<-t(ln_B[,,individual_meta$phi_indx[m]]%*%c(z1,z2)+
             c(ln_Linf[individual_meta$phi_indx[m]],ln_k[individual_meta$phi_indx[m]]))
      return(X)
    })
    ln_vals<-do.call("rbind",ln_vals)
    individual_meta$Linf<-exp(ln_vals[,1]) 
    individual_meta$k<-exp(ln_vals[,2])
    individual_meta$yr_ini<-0
    
    ## Z: INDIVIDUAL SURVIVAL MATRIX WHERE EACH ROW IS A SINGLE FISH 
    ### IN THE GIVEN BEND AND EACH COLUMN IS A YEAR (0=Dead, 1=Alive)  
    Z<-matrix(0,nrow=nrow(individual_meta),ncol=nyears)  
    Z[,1]<-1
    
    ## l: LENGTH FOR INDIVIDUALS
    l<-matrix(0,nrow=nrow(individual_meta),ncol=nyears)
    for(i in unique(individual_meta$b_segment))
        {
        ## MAKE A QUICK FUNCTION OF THE INVERSE CUMULATIVE DISTRIBUTION
        l_ini<-approxfun(
                initial_length[initial_length$segment==i,]$quantile,
                initial_length[initial_length$segment==i,]$val,
                rule=2)
        indx<- which(individual_meta$b_segment==i)
        l[indx,1]<-l_ini(runif(length(indx)))
        }
    ### FIX ANY LENGTHS > THAN LINF
    l[,1]<- ifelse(l[,1]>= individual_meta$Linf,individual_meta$Linf*0.95,l[,1])    
    
    ## BND: BEND LOCATION FOR INDIVIDUALS 
    BND<-matrix(0,nrow=nrow(individual_meta),ncol=nyears)  
    BND[,1]<- individual_meta$id
   
    ###############################################################
    ## POPULATION DYNAMICS
    ## 1. SUVIVAL
    ## 2. GROWTH
    ## 3. MOVEMENT
    ## 4. RECRUITMENT
    ###############################################################
    bends2segs<-ddply(individual_meta, .(id), summarize,
                      rpma=mean(rpma),
                      b_id=mean(b_id),
                      phi_indx=mean(phi_indx))
    #ADD IN BENDS WHICH INITIALLY HAD 0 FISH
    bends2segs<-merge(data.frame(id=tmp$id[which(tmp$N_ini==0)],
                                 rpma=tmp$rpma[which(tmp$N_ini==0)],
                                 b_id=tmp$b_id[which(tmp$N_ini==0)],
                                 phi_indx=tmp$phi_indx[which(tmp$N_ini==0)]),
                      bends2segs,
                      by=c("id","rpma","b_id","phi_indx"),all=TRUE)
    
    
    # BEGIN POPULATION SIMULATION 
    ## 4. RECRUITMENT
    ### HOW MANY RECRUITS AGE-1 
    r_freq_upper<-rbinom(nyears,1,1/inputs$upper$r_freq)
    r_freq_lower<-rbinom(nyears,1,1/inputs$lower$r_freq)
    recruits_upper<- r_freq_upper*rpois(nyears,exp(inputs$upper$r_beta0))
    recruits_lower<- r_freq_lower*rpois(nyears,exp(inputs$lower$r_beta0))
    r_dat<-data.frame(rpma=c(rep(2,nyears),rep(4,nyears)), year=rep(1:nyears,2),
                      r_year=c(r_freq_upper,r_freq_lower), 
                      age_0=c(recruits_upper,recruits_lower))

    for(i in 2:nyears)
        {# loop over each year
        ### RECRUIT TO POPULATION
        if(sum(recruits_upper[i-1],recruits_lower[i-1])>0)
            {
            ### ASSIGN A LENGTH AND GROWTH PARAMETERS
            new_recruits<- data.frame(
                rpma=c(rep(2,recruits_upper[i-1]),rep(4,recruits_lower[i-1])))
            ### ASSIGN A SEGMENT AND BEND
            recruit_loc<- lapply(1:nrow(new_recruits),function(x)
                {
                bend<- sample(tmp[which(tmp$rpma==new_recruits$rpma[x]),]$b_id,1)
                  #MAKE PROBABILITY BASED ON BEND LENGTH, LOCATION, IRC HABITAT AVAILABILITY ETC???
                segment<- tmp[which(tmp$b_id==bend & tmp$rpma==new_recruits$rpma[x]),]$b_segment
                phi_indx<- tmp[which(tmp$b_id==bend & tmp$rpma==new_recruits$rpma[x]),]$phi_indx
                id<- tmp[which(tmp$b_id==bend & tmp$rpma==new_recruits$rpma[x]),]$id
                return(list(b_id=bend,b_segment=segment,phi_indx=phi_indx, id=id))
                })
            recruit_loc<-as.data.frame(do.call("rbind",recruit_loc))
            new_recruits$b_segment<- unlist(recruit_loc$b_segment)
            new_recruits$b_id<- unlist(recruit_loc$b_id)
            new_recruits$phi_indx<- unlist(recruit_loc$phi_indx)
            new_recruits$id<-unlist(recruit_loc$id)
            
            ### GROWTH PARAMETES FOR NEW RECRUITS
            ln_vals<-lapply(1:nrow(individual_meta),function(m)
            {#DRAWN FROM MIDDLE 80% OF BIVARIATE NORMAL (ELLIPSE)
              z1<-rtruncnorm(1, -sqrt(-2*log(0.2)), sqrt(-2*log(0.2)), mean=0, sd=1)
              z2<-rtruncnorm(n=1, a=-sqrt(-2*log(0.2)-z1^2), b=sqrt(-2*log(0.2)-z1^2), 
                             mean=0, sd=1)
              X<-t(ln_B[,,individual_meta$phi_indx[m]]%*%c(z1,z2)+
                     c(ln_Linf[individual_meta$phi_indx[m]],ln_k[individual_meta$phi_indx[m]]))
              return(X)
            })
            ln_vals<-do.call("rbind",ln_vals)
            new_recruits$Linf<-exp(ln_vals[,1])
            new_recruits$k<-exp(ln_vals[,2]) 
            new_recruits$fish_id<-(max(individual_meta$fish_id)+1):(max(individual_meta$fish_id)+sum(recruits_upper[i-1],recruits_lower[i-1]))
            new_recruits$yr_ini<-i-1
            individual_meta<-rbind.fill(individual_meta,new_recruits)      
                  
            ## UPDATE MATRICES: Z, L, BND
            ### MATRICES TO APPEND TO OTHER
            BND_recruits<-l_recruits<-Z_recruits<- matrix(0,nrow=sum(recruits_upper[i-1],recruits_lower[i-1]),ncol=nyears)
            
            # ALIVE OR NOT
            Z_recruits[,i-1]<-1 # had to be alive in previous year to recruit
            Z<-rbind(Z,Z_recruits)
            
            # LENGTH AT AGE 0
            l_recruits[,i-1]<-200 ## 250mm calibrates to ~ 325 mm at age-1
            l<-rbind(l,l_recruits)
              ## ADD VARIATION IN AGE 0 SIZE???
            
            ## BEND
            BND_recruits[,i-1]<-new_recruits$id
            BND<-rbind(BND,BND_recruits)
            }
        
            for(m in 1:nrow(Z))
                {# loop over individuals
                #INDEX FOR LOCATION OF FISH IN PREVIOUS TIME STEP  
                seg_indx<-ifelse(Z[m,i-1]>0,bends2segs$phi_indx[which(
                  bends2segs$id==BND[m,i-1])],1)# Using 1 when FALSE is 
                                                # arbitrary and only a placeholder
                                                # since fish is dead at this point
                b_indx<-ifelse(Z[m,i-1]>0,bends2segs$b_id[which(
                  bends2segs$id==BND[m,i-1])],1)# Using 1 when FALSE is 
                                                # arbitrary and only a placeholder
                                                # since fish is dead at this point
                ## 1. SURVIVAL
                Z[m,i]<- rbinom(1,
                    size=1,
                    prob=phi[seg_indx,i-1]*Z[m,i-1])
                ## 2. FABENS MODEL FOR GROWTH (VBGF)
                l[m,i]<-(l[m,i-1] + (individual_meta$Linf[m]-l[m,i-1])*(1-exp(-individual_meta$k[m])))*Z[m,i]
                    # 0 growth if dead 
                ## 3. MOVEMENT MODEL
                if(individual_meta$rpma[m]==2 & Z[m,i]>0)
                    {
                    y<- exp(mv_beta0[1]+
                        mv_beta1[1]*dis$rpma2[b_indx,])
                    y[which(dis$rpma2[b_indx,]==0)]<-1
                    p<- y/sum(y)
                    BND[m,i]<- sample(x=subset(tmp,rpma==2)$id,
                        size=1,
                        prob=p)
                    } # end if 
                 if(individual_meta$rpma[m]==4 & Z[m,i]>0)# needs to be alive
                    {
                    y<- exp(mv_beta0[2]+
                        mv_beta1[2]*dis$rpma4[b_indx,])
                    y[which(dis$rpma4[b_indx,]==0)]<-1
                    p<- y/sum(y)
                    BND[m,i]<- sample(x=subset(tmp,rpma==4)$id,
                        size=1,
                        prob=p)
                    } # end if
                }
        } # END POPOULATION SIMULATION
    
    ## AGE 0's IN FINAL YEAR
    if(sum(recruits_upper[nyears],recruits_lower[nyears])>0)
    {
      ### ASSIGN A LENGTH AND GROWTH PARAMETERS
      new_recruits<- data.frame(
        rpma=c(rep(2,recruits_upper[nyears]),rep(4,recruits_lower[nyears])))
      ### ASSIGN A SEGMENT AND BEND
      recruit_loc<- lapply(1:nrow(new_recruits),function(x)
      {
        bend<- sample(tmp[which(tmp$rpma==new_recruits$rpma[x]),]$b_id,1)
        #MAKE PROBABILITY BASED ON BEND LENGTH, LOCATION, IRC HABITAT AVAILABILITY ETC???
        segment<- tmp[which(tmp$b_id==bend & tmp$rpma==new_recruits$rpma[x]),]$b_segment
        phi_indx<- tmp[which(tmp$b_id==bend & tmp$rpma==new_recruits$rpma[x]),]$phi_indx
        id<- tmp[which(tmp$b_id==bend & tmp$rpma==new_recruits$rpma[x]),]$id
        return(list(b_id=bend,b_segment=segment,phi_indx=phi_indx, id=id))
      })
      recruit_loc<-as.data.frame(do.call("rbind",recruit_loc))
      new_recruits$b_segment<- unlist(recruit_loc$b_segment)
      new_recruits$b_id<- unlist(recruit_loc$b_id)
      new_recruits$phi_indx<- unlist(recruit_loc$phi_indx)
      new_recruits$id<-unlist(recruit_loc$id)
      
      ### GROWTH PARAMETES FOR NEW RECRUITS
      ln_vals<-lapply(1:nrow(individual_meta),function(m)
      {#DRAWN FROM MIDDLE 80% OF BIVARIATE NORMAL (ELLIPSE)
        z1<-rtruncnorm(1, -sqrt(-2*log(0.2)), sqrt(-2*log(0.2)), mean=0, sd=1)
        z2<-rtruncnorm(n=1, a=-sqrt(-2*log(0.2)-z1^2), b=sqrt(-2*log(0.2)-z1^2), 
                       mean=0, sd=1)
        X<-t(ln_B[,,individual_meta$phi_indx[m]]%*%c(z1,z2)+
               c(ln_Linf[individual_meta$phi_indx[m]],ln_k[individual_meta$phi_indx[m]]))
        return(X)
      })
      ln_vals<-do.call("rbind",ln_vals)
      new_recruits$Linf<-exp(ln_vals[,1])
      new_recruits$k<-exp(ln_vals[,2]) 
      new_recruits$fish_id<-(max(individual_meta$fish_id)+1):(max(individual_meta$fish_id)+sum(recruits_upper[nyears],recruits_lower[nyears]))
      new_recruits$yr_ini<-nyears
      individual_meta<-rbind.fill(individual_meta,new_recruits)      
      
      ## UPDATE MATRICES: Z, L, BND
      ### MATRICES TO APPEND TO OTHER
      BND_recruits<-l_recruits<-Z_recruits<- matrix(0,nrow=sum(recruits_upper[nyears],recruits_lower[nyears]),ncol=nyears)
      
      # ALIVE OR NOT
      Z_recruits[,nyears]<-1 # had to be alive in previous year to recruit
      Z<-rbind(Z,Z_recruits)
      
      # LENGTH AT AGE 0
      l_recruits[,nyears]<-200 ## 250mm calibrates to ~ 325 mm at age-1
      l<-rbind(l,l_recruits)
      ## ADD VARIATION IN AGE 0 SIZE???
      
      ## BEND
      BND_recruits[,nyears]<-new_recruits$id
      BND<-rbind(BND,BND_recruits)
    }
    
    ## PROCESS POPULATION AND RETURN    
    l[l==0]<-NA
    BND[BND==0]<-NA
    # MATRIX OF BEND LEVEL ABUNDANCES TO RETURN
    out<-aggregate(Z[,1],
                by=list(id=BND[,1]),
                   sum)
    names(out)[2]<-"yr_1"
    for(i in 2:nyears)
        {
        app<-aggregate(Z[,i],
            by=list(id=BND[,i]),
            sum)
        names(app)[2]<-paste("yr",i,sep="_")
        out<-merge(out,app, by="id", all=TRUE)
        }
    ## IN CASE THERE WERE BENDS WITH NO FISH EVERY YEAR
    out<-merge(out,data.frame(id=tmp$id), by="id", all=TRUE)
    out[is.na(out)]<-0  # NAs for no fish in bend
    if(nrow(out)!=nrow(tmp))
        {
        return(print("ERROR IN BEND ABUNDANCE MERGE"))
        } #ERROR HANDLING FOR DOUBLE CHECKING
    out<-out[order(out$id),]
    #if(is.null(Linf)){l<-0}
    out<-list(out=as.matrix(out[,-c(1)]), 
        bendMeta=tmp,
        individual_meta=individual_meta,
        Z=Z,
        BND=BND,
        l=l,
        r_dat=r_dat,
        inputs=inputs)
    return(out)# return relevant stuff
    }



## 4. FUNCTION TO DETERMINE WHICH BENDS WITHIN A SEGMENT 
## TO SAMPLE
bend_samples<-function(sim_pop=NULL,
    samp_type=NULL)
    {
    # this function determines which bends within segments are to be
    # sampled each year with the number of bends sampled within a segment
    # given by Table A1 in PSPAP_Vol_1.8.FEB 2017_Welker_Drobish_Williams.pdf
  
    # inputs
    ## sim_pop: a simulated population using the reference_populations 
    ##  function having components:
    ##    $out: a matrix of bend abundance data (rows=bends; cols=years)
  
    # outputs
    ## a list of 2 elements:
    ##  $bendLong: a data.frame expanded from sim_pop$bendMeta to include one 
    ##    observation per bend per year; new columns include:
    ##      $sampled: a column of 0's (not sampled) and 1's (sampled) that
    ##        tell if the given bend should be sampled in the given year
    ##      $s_abund and $r_abund: columns of segment-level and RPMA-level
    ##        abundances, respectively 
    ##  $sampled: a matrix of 0's (not sampled) and 1's (sampled) where
    ##    each row is a bend and each column is a year
  
    # ERROR HANDLING
    if(samp_type!="r" & samp_type!="f")
    {return(print("samp_type needs to be one of two characters: \n
        r, which randomly selects bends for each year of sampling or \n
        f, which randomly selects a single set of bends and fixes them
                to be sampled every year \n"))}
  
    # GET BEND INFORMATION
    tmp<-sim_pop$bendMeta
    tmp<-tmp[order(tmp$id),] #CRITICAL
    bends_in_segs<-ddply(tmp, .(b_segment), summarize, 
                         bend_num=length(bend_num), 
                         start=min(id), 
                         stop=max(id))
    # SAMPLE NUMBERS IN TABLE A1 IN 
    #   PSPAP_Vol_1.8.FEB 2017_Welker_Drobish_Williams.pdf
    bends_in_segs$samp_num<-c(0, 12, 21, 12, 12, 15, 20, 10, 11, 14)
  
    # DETERMINE WHICH BENDS IN A SEGMENT TO SAMPLE
    abund<-sim_pop$out
    sampled<-matrix(0,nrow=nrow(abund), ncol=ncol(abund))
    if(samp_type=="r")
        {
        for(j in 1:ncol(abund))
            {
            sample_bends<-NULL
            for(k in 1:nrow(bends_in_segs)) 
                {
                sample_bends<-c(sample_bends,
                        sample(c(bends_in_segs$start[k]:bends_in_segs$stop[k]), 
                               bends_in_segs$samp_num[k], replace=FALSE))
                }
            for(i in 1:nrow(abund))
                {
                sampled[i,j]<-ifelse(any(sample_bends==i), 1, 0)
                }
            } 
        }
    if(samp_type=="f")
        {
        sample_bends<-NULL
        for(k in 1:nrow(bends_in_segs)) 
            {
            sample_bends<-c(sample_bends,
                      sample(c(bends_in_segs$start[k]:bends_in_segs$stop[k]), 
                             bends_in_segs$samp_num[k], replace=FALSE))
            }
        for(i in 1:nrow(abund))
            {
            sampled[i,]<-rep(ifelse(any(sample_bends==i), 1, 0),ncol(abund))
            }
        } 
  
    # RETURN SAMPLES BENDS (MATRIX FORM)
    return(sampled)
    }




## 5. FUNCTION TO DETERMINE EFFORT & CATCHABILITY OF EACH DEPLOYMENT AND 
##    RESULTING CAPTURE HISTORIES OF FISH IN SAMPLED BENDS
catch_data<-function(sim_pop=NULL,inputs,...)
    {
    samp_type=inputs$samp_type
    gears=inputs$gears
    catchability=inputs$catchability
    B0_sd=inputs$B0_sd
    deployments=inputs$deployments
    effort=inputs$effort
    occasions=inputs$occasions    
    
    
    # USE SIM_POP TO DEFINE VARIABLES
    tmp<-sim_pop$bendMeta
    tmp<-tmp[order(tmp$id),] #CRITICAL
    b_abund<-sim_pop$out
    individual_meta<-sim_pop$individual_meta
    individual_meta<-individual_meta[order(individual_meta$fish_id),]
    l<-sim_pop$l
    BND<-sim_pop$BND
    r_dat<-sim_pop$r_dat
    inputs<-c(sim_pop$inputs,inputs)
    sampled<-bend_samples(sim_pop=sim_pop,samp_type=samp_type)

    # LOG-ODDS CATCHABILITY BY GEAR
    B0<- log(catchability/(1-catchability))

    # EFFORT AND CATCHABILITY DATA FOR SAMPLED BENDS
    b_samp<-lapply(1:ncol(sampled),function(yr)
        { 
        ## PULL OUT SAMPLED BENDS
        samp_indx<-which(sampled[,yr]==1)
        ## CREATE TABLE OF SAMPLED BENDS
        tmp1<-tmp[samp_indx,]
        tmp1<-tmp1[,c("b_segment","bend_num","id")]
        ## ADD YEAR SAMPLED
        tmp1$year<-yr
          
        ## FIND EFFORT AND CATCHABILITY FOR EACH GEAR, BEND, 
        ## OCCASION, & DEPLOYMENT
        gear_dat<-lapply(gears,function(g,d=deployments,out=tmp1)
            {
            ### ADD GEAR TO TABLE
            out$gear<-g
            ### EXPAND TABLE FOR DEPLOYMENTS AND OCCASIONS
            k<-which(gears==g)
            out<-out[rep(seq_len(nrow(out)), each=d[k]*occasions),]
            out$deployment<-rep(1:d[k],times=occasions*length(samp_indx))
            out$occasion<-rep(1:occasions, each=8, times=length(samp_indx))

            ### ADD EFFORT
            f<-sapply(samp_indx, function(x)
                {
                indx<-which(effort$gear==g & 
                      effort$rpma==tmp$rpma[x])
                  #### ERROR HANDLING FOR GEARS THAT ARE NOT USED 
                  #### IN A RPMA: NO EFFORT AND f=0
                  if(length(indx)==0)
                  {
                    f_reps<-rep(0,d[k]*occasions)
                  }
                  if(length(indx)>0)
                  {
                    f_reps<-rgamma(n=d[k]*occasions,
                                     shape=effort$gamma_shape[indx], 
                                     rate=effort$gamma_rate[indx])
                  }
                  return(f_reps)
                })
        out$f<-c(f)
        
        ### ADD CATCHABILITY
        out$q<-plogis(B0[k]+rnorm(n=d[k]*occasions*length(samp_indx),
            mean=0,sd=B0_sd[k]))

        ### RETURN EXPANDED DATAFRAME (SINGLE GEAR)
        return(out)
        })
      
        ## COMBINE INTO A SINGLE DATAFRAME
        gear_dat<-do.call(rbind, gear_dat)
          
        ## RETURN DATAFRAME (SINGLE YEAR)
        return(gear_dat)
        })
  
    # COMBINE INTO A SINGLE DATAFRAME
    b_samp<-do.call(rbind, b_samp) 
  
    # ADD INDIVIDUAL CPs
    b_samp$p<-b_samp$q*b_samp$f

    # CREATE CAPTURE HISTORIES FOR EACH GEAR
    # LONG TO RUN, OPTIMIZE CODE AT SOME POINT
    ## RUN IN PARALLEL
    library(parallel)
    ### USE ALL CORES
    numCores<-detectCores()
    ### INITIATE CLUSTER
    cl<-makeCluster(numCores)
    ### MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
    clusterExport(cl, c("sampled", "individual_meta","l","BND","tmp",
                        "gears","occasions","b_samp"),
                  envir=environment())
    ch<-parLapply(cl,1:ncol(sampled),function(yr)
        { 
        ## PULL OUT SAMPLED BENDS
        samp_indx<-which(sampled[,yr]==1)
        bend_ch<-lapply(samp_indx, function(x)
            {
            indx<- which(BND[,yr]==x)   ## LINK FOR MOVEMENT; ONLY GIVES LIVE FISH
            if(length(indx)==0) {occ_ch<-NULL}
            if(length(indx)>0)
                {
                ## CREATE TABLE OF SAMPLED BENDS
                tmp1<-tmp[x,c("b_segment","bend_num","id")]
                ## ADD YEAR SAMPLED
                tmp1$year<-yr
                ## EXPAND FOR INDIVIDUALS
                tmp1<-merge(tmp1,data.frame(fish_id=indx),all=TRUE)
                ## ADD INDIVIDUAL LENGTHS
                tmp1$length<-l[indx,yr]
                ## EXPAND FOR GEARS
                tmp1<-tmp1[rep(seq_len(nrow(tmp1)), each=length(gears)),]
                tmp1$gear<-rep(gears, times=length(indx))
                ## FIND CH FOR EACH OCCASION, GEAR, AND INDIVIDUAL
                occ_ch<-lapply(1:occasions,function(occ,out=tmp1)
                    { 
                    ### EXPAND DATAFRAME TO INCLUDE OCCASION
                    out$occasion<-occ
                    ### FIND OCCASION LEVEL CP FOR GIVEN BEND AND YEAR
                    dat<-subset(b_samp, year==yr & id==x & occasion==occ)
                    P<-aggregate(p~gear,dat,sum)
                    #P<-aggregate(pnot~gear,dat,prod)
                    #P$p<-1-P$pnot
                    ### CAP CPs AT 1
                    P$p<-ifelse(P$p>1,1,P$p)
                    ### CH
                    ch_reps<-matrix(rbinom(length(indx)*length(gears), size=1,
                        prob=rep(P$p,each=length(indx))),
                        nrow=length(indx),
                        ncol=length(gears))
                    out$ch<-c(t(ch_reps))
                    out<-subset(out,ch==1)
                    return(out)
                    })
                occ_ch<-do.call(rbind,occ_ch)
                }
            return(occ_ch)
            })
        bend_ch<-do.call(rbind,bend_ch)
        })
    ### CLOSE CLUSTERS
    stopCluster(cl)
    ch<-do.call(rbind,ch)
    
    # PROCESS THE DATA
    tmp1<-aggregate(length.rkm~b_segment, tmp,sum)
    names(tmp1)[2]<-"seg_rkm"
    tmp1<-tmp1[rep(seq_len(nrow(tmp1)),ncol(b_abund)),]
    tmp1$year<-rep(1:ncol(b_abund), each=length(unique(tmp$b_segment)))
    # SEGMENT ABUNDANCE BY YEAR
    s_abund<-sapply(1:max(tmp$phi_indx), function(i)
        {
        r<-which(tmp$phi_indx==i)
        if(length(r)==1) out<-b_abund[r,]
        if(length(r)>1)
            {
            out<-colSums(b_abund[r,]) 
            }
        return(out)
        })
    tmp1<-tmp1[order(tmp1$b_segment,tmp1$year),]
    tmp1$abundance<-c(s_abund)
    # SEGMENT MEAN LENGTH BY YEAR
    s_length<-sapply(1:max(tmp$phi_indx), function(i)
    {
      r<-which(tmp$phi_indx==i) #find ids (bends) in segment
      seg_i<-sapply(1:ncol(BND),function(yr)
        {
          z<-which(BND[,yr] %in% r)
          if(length(z)==0) out<-NA
          if(length(z)!=0) out<-mean(l[z,yr])
          return(out)
        })
      return(seg_i)
    })
    tmp1<-tmp1[order(tmp1$b_segment,tmp1$year),]
    tmp1$mean_length<-c(s_length)
    # SEGMENT AGE-0s (THAT RECRUIT) BY YEAR
    s_recruits<-aggregate(fish_id~yr_ini+b_segment,individual_meta, length, subset=yr_ini!=0)
    colnames(s_recruits)[which(colnames(s_recruits)=="fish_id")]<-"age_0"
    colnames(s_recruits)[which(colnames(s_recruits)=="yr_ini")]<-"year"
    tmp1<-merge(tmp1,s_recruits,by=c("b_segment","year"), all.x=TRUE)
    tmp1[is.na(tmp1$age_0),]$age_0<-0
    # RECRUITMENT DATA BY YEAR
    tmp1$rpma<-ifelse(tmp1$b_segment %in% c(1:4),2,4)
    tmp1<-merge(tmp1,r_dat[,c("rpma", "year","r_year")],by=c("rpma","year"), all.x=TRUE)
    #phi<-matrix(0,nrow=nrow(s_abund),ncol=ncol(s_abund))
    #for(i in 1:(nrow(phi)-1))
    #    {
    #    phi[i,]<-s_abund[i+1,]/s_abund[i,]
    #    }
    #phi[nrow(phi),]<-NA
    #tmp1$phi<-c(phi)
    #tmp1$density<-tmp1$abundance/tmp1$length.rkm
    #tmp1<-tmp1[,c(1,3:6)]
    tmp1<-tmp1[,which(names(tmp1)!="rpma")]
    b_samp<-b_samp[,which(names(b_samp)!="p")]
    ch<-ch[,which(names(ch)!="ch")]
    inputs<-c(sim_pop$inputs, inputs)
    return(list(true_vals=tmp1, 
        samp_dat=b_samp, 
        catch_dat=ch,
        inputs=inputs))
    }
 



## 6. GETTING CPUE ESTIMATES
### USES OCCASION 1 DATA ONLY
MKA.ests<-function(sim_dat=NULL,
                    max_occ=NULL, #Number of occasions to use for estimate
                    ....)
{
  #MASSAGE DATA TO FIT THE NUMBER OF OCCASIONS TO BE USED
  catch<-sim_dat$catch_dat
  occ<-1
  if(!is.null(max_occ)) #modify analysis for less occasions than those simulated, if desired
  {
    if(max_occ>sim_dat$inputs$occasions | max_occ<1)
    {return(print(paste0("max_occ needs to be at least 1 AND less than or equal to ",
                         sim_dat$inputs$occasions)))}
    occ<-1:max_occ
  } 
  catch<-catch[which(catch$occasion %in% occ),]
  
  ## GET TOTAL CATCH FOR EACH SAMPLED BEND
  tmp<-aggregate(fish_id~b_segment+bend_num+year+gear,catch, length)
  names(tmp)[which(names(tmp)=="fish_id")]<-"catch"
  ### ADD SAMPLED BENDS WITH ZERO CATCH
  sampled<-ddply(sim_dat$samp_dat[which(sim_dat$samp_dat$occasion %in% occ),],.(b_segment,bend_num,year,gear),
                 summarize,
                 effort=sum(f))
  tmp<-merge(tmp, sampled, by=c("b_segment","bend_num","year","gear"),all.y=TRUE)
  tmp[which(is.na(tmp$catch)),]$catch<-0
  ### REMOVE UNUSED GEARS (THIS SHOULD BE ONLY GN18 & GN81 IN RPMA 2)
  tmp<-subset(tmp,effort!=0)
  ## ADD BEND RKM
  bends<-sim_dat$inputs$bends
  tmp<- merge(tmp,bends[,c("b_segment","bend_num","length.rkm")], by=c("b_segment","bend_num"))
  ## ADD MINIMUM KNOWN ALIVE (FOR max_occ > 1 THIS DEPENDS ON BEING ABLE TO INDIVIDUALLY IDENTIFY FISH)
  if(max(occ)==1){tmp$alive<-tmp$catch}
  if(max(occ)>1)
  {
    #AVOID DOUBLE COUNTING REPEAT FISH
    gears<-unique(catch$gear)
    catch$ch<-1 #all fish in data were caught
    ch<-lapply(gears,function(g)
    {
      pp<- dcast(catch, 
                 year+b_segment+bend_num+fish_id~occasion,
                 value.var="ch", sum, subset=.(gear==g))
      pp$gear<-g
      return(pp)
    })
    ch<-do.call(rbind,ch)
    out<-aggregate(fish_id~b_segment+bend_num+year+gear,ch,length)
    names(out)[ncol(out)]<-"alive"
    tmp<-merge(tmp,out,by=c("b_segment","bend_num","year","gear"),all.x=TRUE)
    tmp[which(is.na(tmp$alive)),]$alive<-0
  }
  ## ADD ESTIMATOR TYPE, NUMBER OF OCCASIONS USED, AND RENAME COLUMNS
  colnames(tmp)[which(colnames(tmp)=="b_segment")]<-"segment"
  colnames(tmp)[which(colnames(tmp)=="length.rkm")]<-"rkm"
  tmp$occasions<-max(occ)
  tmp$estimator<-"MKA"
  return(tmp)
}




## 7. GETTING M0 & Mt ESTIMATES
M0t.ests<-function(sim_dat=NULL,
                   max_occ=NULL, #Number of occasions to use for estimate.
                   ...)
{
  # ERROR HANDLING
  if(sim_dat$inputs$occasions<2)
  {return(print("Simulated data needs to have at least 2 capture occasions per year."))}
  
  ## MASSAGE DATA INTO SHAPE 
  ## CAPTURE HISTORIES
  catch<-sim_dat$catch_dat
  catch$ch<-1 #all fish on list were captured
  gears<-unique(catch$gear) #identify gears that caught fish
  occ<-as.character(unique(catch$occasion)) #identify number of occasions simulated
  if(!is.null(max_occ)) #modify analysis for less occasions than those simulated, if desired
  {
    if(max_occ>sim_dat$inputs$occasions | max_occ<2)
    {return(print(paste0("max_occ needs to be at least 2 AND less than or equal to ",
                         sim_dat$inputs$occasions)))}
    occ<-as.character(1:max_occ)
    catch<-catch[which(catch$occasion %in% occ),]
  } 
  ch<-lapply(gears,function(g)
  {
    pp<- dcast(catch, 
               year+b_segment+bend_num+fish_id~occasion,
               value.var="ch", sum, subset=.(gear==g))
    pp$gear<-g
    return(pp)
  })
  ch<-do.call(rbind,ch)
  
  ### PULL SAMPLED BENDS
  samps<-ddply(sim_dat$samp_dat[which(sim_dat$samp_dat$occasion %in% occ),],.(b_segment,bend_num,year,gear),
               summarize,
               effort=sum(f))
  ### REMOVE UNUSED GEARS (THIS SHOULD BE ONLY GN18 & GN81 IN RPMA 2)
  samps<-subset(samps,effort!=0)
  ### ADD BEND RKM
  bends<-sim_dat$inputs$bends
  samps<- merge(samps,bends[,c("b_segment","bend_num","length.rkm")], by=c("b_segment","bend_num"))
  
  ## RUN M0/Mt ESTIMATOR IN PARALLEL
  library(parallel)
  ### USE ALL CORES
  num_cores<-detectCores()
  ### INITIATE CLUSTER
  cl<-makeCluster(num_cores)
  ### MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
  clusterExport(cl, c("ch", "samps", "occ"), envir=environment())
  clusterEvalQ(cl, library(Rcapture))
  ### M0/Mt ESTIMATOR
  bend_Np<- parLapply(cl,1:nrow(samps),function(x)
  {
    ## SUBSET BEND AND YEAR CAPTURE DATA
    bend_dat<-subset(ch, 
                     b_segment==samps$b_segment[x] & 
                       bend_num==samps$bend_num[x] & 
                       year==samps$year[x] &
                       gear==samps$gear[x])
    
    ## FIT M0 MODEL TO ESTIMATE ABUNDANCE
    if(nrow(bend_dat)>0){
      tmp<- closedp.t(bend_dat[,occ])## estimate abundance
      warn_codes_M0<-ifelse(tmp$results[1,7]==0,"0",
                         ifelse(grepl("bias",tmp$glm.warn$M0, fixed=TRUE),"b",
                                ifelse(grepl("sigma",tmp$glm.warn$M0, fixed=TRUE),"s",
                                       ifelse(grepl("converge",tmp$glm.warn$M0, fixed=TRUE),"c",
                                              ifelse(grepl("0 occurred",tmp$glm.warn$M0, fixed=TRUE),"z",
                                                     tmp$glm.warn$M0)))))
      warn_codes_Mt<-ifelse(tmp$results[2,7]==0,"0",
                            ifelse(grepl("bias",tmp$glm.warn$Mt, fixed=TRUE),"b",
                                   ifelse(grepl("sigma",tmp$glm.warn$Mt, fixed=TRUE),"s",
                                          ifelse(grepl("converge",tmp$glm.warn$Mt, fixed=TRUE),"c",
                                                 ifelse(grepl("0 occurred",tmp$glm.warn$Mt, fixed=TRUE),"z",
                                                        tmp$glm.warn$Mt)))))
      tmp<- data.frame(## collect up relevant bits for M0 and Mt
        year=samps$year[x],
        segment=samps$b_segment[x],
        bend_num=samps$bend_num[x],
        gear=samps$gear[x],
        effort=samps$effort[x],
        occasions=max(as.numeric(occ)),
        rkm=samps$length.rkm[x],
        samp_size=nrow(bend_dat),
        Nhat_M0=tmp$parameters$M0[1],
        SE_Nhat_M0=tmp$results[1,2],
        p_M0=tmp$parameters$M0[2],
        fit_M0=tmp$results[1,7],
        warn_M0=warn_codes_M0,
        Nhat_Mt=tmp$parameters$Mt[1],
        SE_Nhat_Mt=tmp$results[2,2],
        p_Mt=tmp$parameters$Mt[2],
        fit_Mt=tmp$results[2,7],
        warn_Mt=warn_codes_Mt
      )}
    ## FILL FOR NO DATA
    if(nrow(bend_dat)==0){# no data
      tmp<- data.frame(
        year=samps$year[x],
        segment=samps$b_segment[x],
        bend_num=samps$bend_num[x],
        gear=samps$gear[x],
        effort=samps$effort[x],
        occasions=max(as.numeric(occ)),
        rkm=samps$length.rkm[x],
        samp_size=0,
        Nhat_M0=-99,
        SE_Nhat_M0=-99,
        p_M0=-99,
        fit_M0=-99,
        warn_M0="-99",
        Nhat_Mt=-99,
        SE_Nhat_Mt=-99,
        p_Mt=-99,
        fit_Mt=-99,
        warn_Mt="-99"
      )}
    return(tmp)    
  }) # ABOUT 5 MINUTES ON CRUNCH
  ### CLOSE CLUSTERS
  stopCluster(cl)
  ### CREATE DATA FRAME
  bend_Np<- do.call("rbind", bend_Np)
  ## REORGANIZE DATA FRAME
  tmp0<-bend_Np[,c(1:13)]
  tmp0$estimator<-"M0"
  colnames(tmp0)<-gsub("_M0", "", colnames(tmp0))
  tmpt<-bend_Np[,c(1:8,14:18)]
  tmpt$estimator<-"Mt"
  colnames(tmpt)<-gsub("_Mt", "", colnames(tmpt))
  bend_Np<-rbind(tmp0,tmpt)
  return(bend_Np)
}




## 8. GETTING ABUNDANCE AND TREND ESTIMATES
abund.trnd<-function(samp_type=NULL, 
                     pop_num=NULL, 
                     catch_num=NULL,
                     location=NULL, #Optional: character string describing where "_output" is located when not located in GitHub; Ex. for Crunch use "E:/"  
                     est_type=NULL, #Optional: for when only a subset of available estimate types should be evaluated; character vector ("CPUE", "M0", etc.)
                     ...)       
                     
{ 
  ## ERROR HANDLING
  if(samp_type!="r" & samp_type!="f")
  {return(print("samp_type needs to be one of two characters: \n
                r, which randomly selects bends for each year of sampling or \n
                f, which randomly selects a single set of bends and fixes them
                to be sampled every year \n"))}
  
  # PULL CATCH DATA AND ASSOCIATED ESTIMATES
  id<-paste0(samp_type, "_", pop_num, "-", catch_num)
  sim_dat<-readRDS(file=paste0(location,"_output/2-catch/catch_dat_",id,".rds"))
  est_list<-dir(paste0(location, "_output/3-estimates"), pattern=id)
  if(length(est_type)>0)
  {
    indx<-lapply(est_type, function(i){grep(i,est_list)})
    indx<-unlist(indx)
    est_list<-est_list[indx]
  }
  if(length(est_list)==0)  
  {return(print("There is no estimate data of type 'est_type' for reference population \n
                'pop_num' and catch data run 'catch_num' with sampling 'samp_type' in \n
                 specified location."))}
    
  
  # COMPILE SEGMENT LEVEL BASED TREND AND ABUNDANCE ESTIMATES
  outt<-lapply(est_list,function(x)
    {
    # PULL ESTIMATOR RESULTS
    est<-readRDS(file=paste0(location,"_output/3-estimates/",x))
    # FIND ACTUAL SEGMENT ABUNDANCES
    true<-sim_dat$true_vals
    names(true)[1]<-"segment"
    true_abund<-true[,c("segment","year","abundance")]
  
    # FIND ACTUAL POPULATION TREND BY GEAR
    true$segment<- as.factor(true$segment)
    fit<-lm(log(abundance)~segment+year,true)
    pop_trnd<-unname(coef(fit)['year'])
  
    # FIND SEGMENT LENGTHS
    seg_length<-aggregate(seg_rkm~segment, data=true,mean)
  
    # PULL INPUTS
    gears<-sim_dat$inputs$gears
    occasions<-unique(est$occasions)
    
    outtt<-lapply(occasions, function(y)
    {
      # PULL ESTIMATES
      tmp<-est[which(est$occasions==y),]
      # MINIMUM KNOWN ALIVE ESTIMATES
      if(tmp$estimator[1]=="MKA")
      {
        ## ADD ESTIMATED BEND DENSITY
        tmp$catch_dens<-tmp$alive/tmp$rkm
        ## GET SEGMENT LEVEL ESTIMATES BY YEAR AND GEAR
        ests<-ddply(tmp,.(segment,year,gear),
                    summarize,
                    samp_size=length(catch),
                    mean_dens=mean(catch_dens),
                    sd_dens=sd(catch_dens),
                    catch=sum(catch),
                    effort=sum(effort),
                    alive=sum(alive),
                    samp_rkm=sum(rkm))
        ests$WM_dens<-ests$alive/ests$samp_rkm
        ## ADD SEGMENT LENGTHS, PERFORMANCE, AND ESTIMATOR
        ests<-merge(ests,seg_length, by="segment",all.x=TRUE)
        ests$perform<-1
        ests$estimator<-"MKA"
    
        # ABUNDANCE
        ## ESTIMATE SEGMENT ABUNDANCE
        ests$Nhat_AM<-ests$seg_rkm*ests$mean_dens #arithmetic mean
        ests$Nhat_WM<-ests$seg_rkm*ests$WM_dens #weighted arithmetic mean
        ## ADD TRUE ABUNDANCE
        ests<-merge(ests, true_abund, by=c("segment","year"), all.x=TRUE)
        ## ADD BIAS
        ests$bias_AM<-ests$Nhat_AM-ests$abundance
        ests$bias_WM<-ests$Nhat_WM-ests$abundance
        ## ADD PRECISION
        ### CALCULATE WEIGHTED MEAN SD
        tmp<-merge(tmp, ests[,c("segment","year","gear","WM_dens","samp_rkm")],
                   by=c("segment","year","gear"),all.x=TRUE)
        tmp$diffsq<-(tmp$catch_dens-tmp$WM_dens)^2
        Wsd<-ddply(tmp,.(segment,year,gear),
                   summarize,
                   Wsd_dens=sqrt(sum((rkm/samp_rkm)*diffsq)))
        ests<-merge(ests,Wsd, by=c("segment","year","gear"))
        ### CALCULATE CV
        ests$precision_AM<-ests$sd_dens/abs(ests$Nhat_AM)
        ests$precision_WM<-ests$Wsd_dens/abs(ests$Nhat_WM)
        ## ADD ABUNDANCE EXTRAS
        ### FLAGS
        occ<-1:y
        samp<-sim_dat$samp_dat
        samp<-samp[which(samp$occasion %in% occ),]
        colnames(samp)[which(colnames(samp)=="b_segment")]<-"segment"
        samp$p<-samp$q*samp$f
        P<-aggregate(p~segment+bend_num+year+gear+occasion, samp, sum)
        P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2))
        fl<-ddply(P, .(segment, year, gear), summarize, 
                  flags=length(which(flag!=0))/length(flag))
        ests<-merge(ests,fl, by=c("segment", "year","gear"), all.x=TRUE)
        ### CATCHABILITY
        q_stats<-ddply(samp, .(segment, year, gear),
                       summarize,
                       q_mean_realized=mean(q),
                       q_sd_realized=sd(q))
        ests<-merge(ests, q_stats, by=c("segment","year","gear"), all.x=TRUE)
      
        # TREND
        ## ABUNDANCE TREND
        ### FIT LINEAR MODEL FOR TREND FOR EACH GEAR
        outA<-lapply(gears,function(g)
        {
          ### USE DATA+1 TO AVOID ln(0) 
          fit_AM<-lm(log(Nhat_AM+1)~as.factor(segment)+year, ests, subset=gear==g)
          fit_WM<-lm(log(Nhat_WM+1)~as.factor(segment)+year, ests, subset=gear==g)
          tmp2<- data.frame( 
            # THE GOODIES
            ## GEAR
            gear=g,
            ## ARITHMETIC MEAN
            ### TREND ESTIMATE
            trnd_AM=coef(fit_AM)['year'],
            ### STANDARD ERROR FOR TREND ESTIMATE
            se_AM=summary(fit_AM)$coefficients['year',2],
            ### PVALUE FOR TREND ESTIMATE
            pval_AM=summary(fit_AM)$coefficients['year',4],
            ## WEIGHTED MEAN
            ### TREND ESTIMATE
            trnd_WM=coef(fit_WM)['year'],
            ### STANDARD ERROR FOR TREND ESTIMATE
            se_WM=summary(fit_WM)$coefficients['year',2],
            ### PVALUE FOR TREND ESTIMATE
            pval_WM=summary(fit_WM)$coefficients['year',4],
            ## TOTAL EFFORT
            effort=sum(ests[which(ests$gear==g),]$effort)
            )
          return(tmp2)
        })
        outA<-do.call(rbind,outA)
        ### ADD ESTIMATOR
        outA$estimator<-"MKA"
        ### REFORMAT MKA ABUNDANCE OUTPUTS
        tmpA<-outA[,c(1:4,8,9)]
        tmpA$estimator<-paste0(tmpA$estimator,"_AM")
        colnames(tmpA)<-gsub("_AM", "", colnames(tmpA))
        tmpW<-outA[,c(1,5:9)]
        tmpW$estimator<-paste0(tmpW$estimator,"_WM")
        colnames(tmpW)<-gsub("_WM", "", colnames(tmpW))
        outA<-rbind(tmpA,tmpW)
        
        
        ## CPUE TREND
        ### ADD SEGMENT-LEVEL CPUE AND LN(CPUE)
        #### USE CATCH +1 TO AVOID CPUE=0
        ests$catch1<-ests$catch+1
        ests$cpue1<-ests$catch1/ests$effort
        ests$lncpue1<-log(ests$cpue1)
        ### FIT LINEAR MODEL FOR TREND FOR EACH GEAR
        outC<-lapply(gears,function(g)
        {
          fit<- lm(lncpue1~as.factor(segment)+year, ests, subset=gear==g)
          tmp2<- data.frame( 
            # THE GOODIES
            ## GEAR
            gear=g,
            ## TREND ESTIMATE
            trnd=coef(fit)['year'],
            ## STANDARD ERROR FOR TREND ESTIMATE
            se=summary(fit)$coefficients['year',2],
            ## PVALUE FOR TREND ESTIMATE
            pval=summary(fit)$coefficients['year',4],
            ## TOTAL EFFORT
            effort=sum(ests[which(ests$gear==g),]$effort)
          )
          return(tmp2)
        })
        outC<-do.call(rbind,outC)
        # ADD ESTIMATOR
        outC$estimator<-"CPUE"
        
        ## FULL TREND
        out<-rbind(outA, outC)
        ### ADD POPULATION TREND
        out$pop_trnd<-pop_trnd
        ### CALCULATE TREND BIAS
        out$bias<-out$trnd-out$pop_trnd
        ### CALCULATE TREND PRECISION
        out$precision<-out$se/abs(out$trnd)
        ### ADD TREND EXTRAS
        out$perform<-1
        #### FLAGS
        fl<-ddply(P, .(gear), summarize, flags=length(which(flag!=0))/length(flag))
        out<-merge(out,fl, by="gear", all.x=TRUE)
        #### CATCHABILITY
        q_stats<-ddply(samp, .(gear),
                       summarize,
                       q_mean_realized=mean(q),
                       q_sd_realized=sd(q))
        out<-merge(out, q_stats, by="gear", all.x=TRUE)
        
        # ADD INPUTS
        inputs<-sim_dat$inputs
        in_dat<-data.frame(gear=inputs$gears, q_mean_input=inputs$catchability, 
                           B0_sd_input=inputs$B0_sd, deployments=inputs$deployments,
                           occasions=y, samp_type=inputs$samp_type,pop_id=pop_num,
                           catch_id=catch_num)
        out<-merge(out, in_dat, by="gear", all.x=TRUE)
        ests<-merge(ests, in_dat, by="gear",all.x=TRUE)
        
        # OUTPUT THE GOODIES
        out<-out[, c("gear", "pop_trnd","trnd","bias", "precision", "pval", "perform",
                       "estimator","flags", "effort", "q_mean_realized", "q_sd_realized",
                       "q_mean_input", "B0_sd_input","deployments","occasions", "samp_type",
                       "pop_id", "catch_id")]
        ests<-ests[,c("segment", "year","gear", "abundance","Nhat_AM","bias_AM", 
                      "precision_AM", "Nhat_WM", "bias_WM", "precision_WM","perform",
                      "estimator","flags", "effort", "q_mean_realized","q_sd_realized",
                      "q_mean_input", "B0_sd_input","deployments","occasions","samp_type",
                      "pop_id", "catch_id")]
      }

      # M0 & Mt ESTIMATES
      if(tmp$estimator[1]=="M0"|tmp$estimator[1]=="Mt")
      {
        ## CLEAN UP DATA
        ### MAKE NON-CONVERGED MODELS AND NO FISH MODELS NA
        tmp[tmp$fit!=0,]$Nhat<-NA 
        tmp[tmp$fit!=0,]$SE_Nhat<-NA
        tmp[tmp$fit!=0,]$p<-NA
        tmp[is.na(tmp$Nhat),]$rkm<-NA  #important for WM calculation
        ## ADD ESTIMATED BEND DENSITY
        tmp$dens<-tmp$Nhat/tmp$rkm
        ## GET SEGMENT LEVEL ESTIMATES BY YEAR, GEAR, AND ESTIMATE TYPE
        ests<-ddply(tmp,.(segment,year,gear,estimator),
                    summarize,
                    mean_dens=mean(dens,na.rm = TRUE),
                    n_st=length(which(!is.na(dens))), #no. of bend densities used to calculate segment density estimate
                    v_tmp=sum((1/rkm)^2*SE_Nhat^2,na.rm=TRUE),
                    N_sst=sum(Nhat,na.rm=TRUE), #estimated total abundance of sampled bends w/in segment
                    d_sst=sum(rkm,na.rm=TRUE),  #total length of sampled bends w/in segment
                    v_tmp2=sum(SE_Nhat^2, na.rm=TRUE),
                    b_st=length(dens), #no. of bends sampled
                    perform=length(which(fit==0))/length(fit), #proportion of sampled bends used in analysis
                    effort=sum(effort))
        ests$dens_sst<-ests$N_sst/ests$d_sst #sampled bend w/in segment density
          # To remove segments that had no bends with successful
          # sampling catch data for the given gear:
          # ests<-subset(ests,n_st!=0)
        ## ADD SEGMENT LENGTHS
        ests<-merge(ests,seg_length, by="segment",all.x=TRUE)
      
        # ABUNDANCE
        ## ESTIMATE SEGMENT ABUNDANCE
        ests$Nhat_AM<-ests$seg_rkm*ests$mean_dens
        ests$Nhat_WM<-ifelse(ests$n_st>0,ests$seg_rkm*ests$dens_sst,NA)
        ## ADD TRUE ABUNDANCE
        ests<-merge(ests, true_abund, by=c("segment","year"), all.x=TRUE)
        ## ADD BIAS
        ests$bias_AM<-ests$Nhat_AM-ests$abundance
        ests$bias_WM<-ests$Nhat_WM-ests$abundance
        ## ADD PRECISION
        ### CALCULATE VARIANCES
        ests$var_AM<-(ests$seg_rkm/ests$n_st)^2*ests$v_tmp
        ests$var_WM<-ifelse(ests$n_st>0,(ests$seg_rkm/ests$d_sst)^2*ests$v_tmp2,NA)
        ### CALCULATE CV
        ests$precision_AM<-sqrt(ests$var_AM)/abs(ests$Nhat_AM)
        ests$precision_WM<-sqrt(ests$var_WM)/abs(ests$Nhat_WM)
        ## ADD ABUNDANCE EXTRAS
        ### FLAGS
        occ<-1:y
        samp<-sim_dat$samp_dat[which(sim_dat$samp_dat$occasion %in% occ),]
        colnames(samp)[which(colnames(samp)=="b_segment")]<-"segment"
        samp$p<-samp$q*samp$f
        P<-aggregate(p~segment+bend_num+year+gear+occasion, samp, sum)
        P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2))
        fl<-ddply(P, .(segment, year, gear), summarize, 
                  flags=length(which(flag!=0))/length(flag))
        ests<-merge(ests,fl, by=c("segment", "year","gear"), all.x=TRUE)
        ### CATCHABILITY FOR NUMBER OF OCCASIONS USED
        q_stats<-ddply(samp, .(segment, year, gear),
                       summarize,
                       q_mean_realized=mean(q),
                       q_sd_realized=sd(q))
        ests<-merge(ests, q_stats, by=c("segment","year","gear"), all.x=TRUE)
    
    
        # TREND
        ## FIT LINEAR MODEL FOR TREND FOR EACH GEAR
        outA<-lapply(gears,function(g)
        {
          tmp<-subset(ests, gear==g)
          out2<-lapply(unique(tmp$estimator), function(e)
          {
            tmp<-subset(tmp, estimator==e)
            #perform<-length(which(tmp$n_st!=0))/length(tmp$n_st) #PROBABLY WANT A DIFFERENT MEASUREMENT OF PERFORMANCE HERE...PROPORTION OF SEGMENTS x YEARS USED... BUT LOTS OF BEND DATA MIGHT BE EXCLUDED
            perform<-sum(tmp$n_st)/sum(tmp$b_st) #total no. of bends with usable data/total no. of bends sampled (in an entire year across the entire river, for a particular gear) 
            #perform<-sum(tmp$n_st)/sum(tmp$b_st)*length(which(tmp$n_st!=0))/length(tmp$n_st) #THIS MIGHT BE BEST
            effort=sum(tmp$effort)
            tmp<-subset(tmp,!is.na(Nhat_AM))
            if(perform>0 #enough data
              & length(unique(tmp$segment))>1 # more than one segment
              & length(unique(tmp$year))>=2) # at least two years
            {
              fit_AM<-lm(log(Nhat_AM)~year+as.factor(segment),tmp)
              fit_WM<-lm(log(Nhat_WM)~year+as.factor(segment),tmp)
              tmp2<- data.frame(
                # THE GOODIES
                ## GEAR
                gear=g,
                ## ESTIMATOR
                estimator=e,
                ## ARITHMETIC MEAN
                ### TREND ESTIMATE
                trnd_AM=ifelse(is.na(summary(fit_AM)$coefficients['year',2]),NA,
                                coef(fit_AM)['year']),
                ### STANDARD ERROR FOR TREND ESTIMATE
                se_AM=summary(fit_AM)$coefficients['year',2],
                ### PVALUE FOR TREND ESTIMATE
                pval_AM=summary(fit_AM)$coefficients['year',4],
                ## WEIGHTED ARITHMETIC MEAN
                ### TREND ESTIMATE
                trnd_WM=ifelse(is.na(summary(fit_WM)$coefficients['year',2]),NA,
                                  coef(fit_WM)['year']),
                ### STANDARD ERROR FOR TREND ESTIMATE
                se_WM=summary(fit_WM)$coefficients['year',2],
                ### PVALUE FOR TREND ESTIMATE
                pval_WM=summary(fit_WM)$coefficients['year',4],
                ## PERFORMANCE (FRACTION OF SEGMENT-YEAR DATA USED)
                perform=perform,
                effort=effort
              )
            }
            if(perform==0 #no data
              | length(unique(tmp$segment))<=1 #or only one segment
              | length(unique(tmp$year))<2) #or less than two years
            {
              tmp2<- data.frame(
                gear=g,
                estimator=e,
                trnd_AM=NA,
                se_AM=NA,
                pval_AM=NA,
                trnd_WM=NA,
                se_WM=NA,
                pval_WM=NA,
                perform=0, #NOT ENOUGH DATA TO CALCULATE TREND
                effort=effort
              )
            }
            return(tmp2)
          })
          out2<-do.call("rbind",out2)
          return(out2)
        })
        outA<-do.call(rbind,outA)
        ## REFORMAT M0t TREND OUTPUTS
        tmpA<-outA[,c(1:5,9,10)]
        tmpA$estimator<-paste0(tmpA$estimator,"_AM")
        colnames(tmpA)<-gsub("_AM", "", colnames(tmpA))
        tmpW<-outA[,c(1,2,6:10)]
        tmpW$estimator<-paste0(tmpW$estimator,"_WM")
        colnames(tmpW)<-gsub("_WM", "", colnames(tmpW))
        outA<-rbind(tmpA,tmpW)
        ## ADD POPULATION TREND
        outA$pop_trnd<-pop_trnd
        ## CALCULATE TREND BIAS
        outA$bias<-outA$trnd-outA$pop_trnd
        ## CALCULATE TREND PRECISION
        outA$precision<-outA$se/abs(outA$trnd)
        #######################################
        #  DO WE WANT EXP VERSION OF THIS???  #
        #######################################
        ## ADD TREND EXTRAS
        ### FLAGS
        fl<-ddply(P, .(gear), summarize, flags=length(which(flag!=0))/length(flag))
        outA<-merge(outA,fl, by="gear")
        ### CATCHABILITY
        q_stats<-ddply(samp, .(gear),
                       summarize,
                       q_mean_realized=mean(q),
                       q_sd_realized=sd(q))
        outA<-merge(outA, q_stats, by="gear")
        # ADD INPUTS
        inputs<-sim_dat$inputs
        in_dat<-data.frame(gear=inputs$gears, q_mean_input=inputs$catchability, 
                           B0_sd_input=inputs$B0_sd, deployments=inputs$deployments,
                           occasions=y, samp_type=inputs$samp_type, pop_id=pop_num,
                           catch_id=catch_num)
        outA<-merge(outA, in_dat, by="gear", all.x=TRUE)
        ests<-merge(ests, in_dat, by="gear",all.x=TRUE)
        # OUTPUT THE GOODIES
        ## ABUNDANCE
        ests<-ests[,c("segment", "year","gear", "abundance","Nhat_AM","bias_AM", 
                      "precision_AM", "Nhat_WM", "bias_WM", "precision_WM","perform",
                      "estimator","flags", "effort", "q_mean_realized","q_sd_realized",
                      "q_mean_input", "B0_sd_input","deployments","occasions","samp_type",
                      "pop_id", "catch_id")]
        ## TREND
        out<-outA[,c("gear", "pop_trnd","trnd","bias", "precision", "pval", "perform",
                     "estimator","flags", "effort", "q_mean_realized", "q_sd_realized",
                     "q_mean_input", "B0_sd_input","deployments","occasions", "samp_type",
                     "pop_id", "catch_id")]
      } 
      return(list(trnd=out, abund=ests))
    })
    # ORGANIZE OUTPUT INTO TWO TABLES
    trnd<-do.call(rbind,lapply(outtt, `[[`, 1))
    abund<-do.call(rbind,lapply(outtt, `[[`, 2))
    return(list(trnd=trnd,abund=abund))
  })
  # ORGANIZE OUTPUT INTO TWO TABLES
  trnd<-do.call(rbind,lapply(outt, `[[`, 1))
  abund<-do.call(rbind,lapply(outt, `[[`, 2))
  return(list(trnd=trnd,abund=abund))
}




## 9. GETTING LENGTH ESTIMATES
length.dat<-function(sim_dat=NULL,...)
{
  # FIND ACTUAL SEGMENT-LEVEL MEAN LENGTH
  true_l<-sim_dat$true_vals[,c("b_segment", "year", "mean_length")]

  # PULL LENGTH DATA AND INPUTS
  ## MASSAGE DATA INTO CAPTURE HISTORIES 
  catch<-sim_dat$catch_dat
  catch$ch<-1 #all fish on list were captured
  gears<-unique(catch$gear) #identify gears that caught fish
  occ<-as.character(unique(catch$occasion))
  ch<-lapply(gears,function(g)
  {
    pp<- dcast(catch, 
               year+b_segment+bend_num+length+fish_id~occasion,
               value.var="ch", sum, subset=.(gear==g))
    pp$gear<-g
    return(pp)
  })
  ch<-do.call(rbind,ch)
    
  # CALCULATE MEAN LENGTH
  ## CPUE 
  tmpC<-ddply(subset(ch,`1`==1), .(b_segment, year, gear), summarize,
              lhat=mean(length),
              SE_length=sd(length))
  ### ADD MISSING SEGMENTS
  sampC<-subset(sim_dat$samp_dat, occasion==1)
  samp_segs<-aggregate(f~b_segment+year+gear,sim_dat$samp_dat, sum)
  fC<-aggregate(f~b_segment+year+gear,sampC, sum)
  names(fC)[which(names(fC)=="f")]<-"f1"
  samp_segs<-merge(samp_segs,fC, by=c("b_segment", "year", "gear"))
  samp_segs<-subset(samp_segs,f!=0)
  tmpC<-merge(tmpC,samp_segs[,c("b_segment", "year", "gear", "f1")],all.y=TRUE)
  names(tmpC)[which(names(tmpC)=="f1")]<-"effort"
  if(length(which(is.na(tmpC$lhat)))!=0){tmpC[which(is.na(tmpC$lhat)),]$lhat<-0}
  ### ADD ESTIMATOR TYPE
  tmpC$estimator<-"CPUE"
  tmpC$occasions<-1
  
  ## M0 & Mt ANALYSES
  tmp<-ddply(ch, .(b_segment, year, gear), summarize,
             lhat=mean(length),
             SE_length=sd(length))
  ### ADD MISSING SEGMENTS
  tmp<-merge(tmp,samp_segs[,c("b_segment", "year", "gear", "f")],all.y=TRUE)
  names(tmp)[which(names(tmp)=="f")]<-"effort"
  if(length(which(is.na(tmp$lhat)))!=0){tmp[which(is.na(tmp$lhat)),]$lhat<-0}
  tmp$occasions<-sim_dat$inputs$occasions
  ### EXPAND FOR TWO ESTIMATORS
  tmp<-rbind(tmp,tmp)
  tmp$estimator<-c(rep("M0",nrow(tmp)/2),rep("Mt",nrow(tmp)/2))
  
  ## COMBINE AND ADD BIAS AND PRECISION
  tmp<-rbind(tmpC,tmp)
  tmp$perform<-1
  ### ADD ACTUAL MEAN LENGTHS
  tmp<-merge(tmp, true_l, by=c("b_segment","year"), all.x=TRUE)
  ### ADD BIAS
  tmp$bias<-tmp$lhat-tmp$mean_length
  ### ADD PRECISION
  tmp$precision<-tmp$SE_length/tmp$lhat
  ### ADD DEPLOYMENTS
  tmp$deployments<-sim_dat$inputs$deployments
  ### FORMULATE OUTPUT
  tmp<-tmp[,c("b_segment","year","gear", "mean_length","lhat","bias","precision",
              "perform","estimator", "effort", "deployments", "occasions")]
  names(tmp)[1]<-"segment"
  return(tmp)
}








 
#######################################################################
# MISC UNUSED FUNCTIONS FOR FUTURE REFERENCE
#######################################################################

# A FUNCTION TO SIMULATE DATA
sim_ch<-function(inputs,index)
	{
	# SET UP CAPTURE HISTORY MATRIX
	ch<- matrix(NA,inputs$N[index],inputs$nocc[index])
	track<- matrix(0,inputs$N[index],1)
	for(k in 1:inputs$nocc[index])
		{
		ch[,k]<-rbinom(inputs$N[index],1,inputs$p[index])	
		}
	# RANOMDLY ASSIGN SOME TRACKING FISH INTO POPULATION
	if(inputs$ntrack[index]<=inputs$N[index])
		{
		track[sample(inputs$N[index],inputs$ntrack[index],replace=FALSE)]<-1
		}
	if(inputs$ntrack[index]>inputs$N[index])
		{
		track[sample(inputs$N[index],inputs$N[index],replace=FALSE)]<-1
		}		
	ch<-cbind(track,ch)
	indx<- which(apply(ch,1,sum)>0)
	ch<- ch[indx,]		
	track<- track[indx]
	if(is.null(nrow(ch))){tmp<-NULL}else{
		tmp<-list(acoustic=data.frame(ch=apply(ch,1,paste0,collapse=""),
			freq=1,
			track=as.factor(track),
			stringsAsFactors=FALSE))
		# DROP ACOUSTIC FISH AND MAKE NEW CAPTURE HISTORIES
		ch<-ch[,-1]
		if(inputs$nocc[index]>1)
			{
			indx<- which(apply(ch,1,sum)>0)
			}else{indx<-0}
		if(length(indx)>5 ) # NOTE 5 CAPTURE-RECAPTURE MINIMUM
			{
			ch<- ch[indx,]	
			tmp$pit<-data.frame(
				ch=apply(ch,1,paste0,collapse=""),
				freq=1,
				stringsAsFactors=FALSE)
			}	
		
		
		}
	return(tmp)
	}	


# A FUNCTION TO ESTIMATE:
# 1. ABUNDANCE AND 
# 2. CAPTURE PROBABILITY
estimate<- function(dat,inputs,indx)
	{
	# ESTIMATE ABUNDANCE USING ACOUSTIC FISH
	M0_proc<-process.data(data=dat$acoustic,
		groups="track",
		model="Closed")
	M0_dll<-make.design.data(M0_proc)
	# FIX P
	trackIndex1<-  which(M0_dll$p$time==1 & M0_dll$p$track==1)
	trackIndex0<- which(M0_dll$p$time==1 & M0_dll$p$track==0)
	vals<-c(rep(1,length(trackIndex1)),rep(0,length(trackIndex0)))
	p<-list(formula=~1,
		fixed=list(index=c(trackIndex1,trackIndex0),
		value=vals),
		share=TRUE)# set c = p
	# FIX F0
	if(sum(as.numeric(as.character(dat$acoustic$track))) < nrow(dat$acoustic))
		{
		f0<-list(formula=~track,fixed=list(index=which(M0_dll$f0$group==1),value=0))
		}
	if(sum(as.numeric(as.character(dat$acoustic$track))) == nrow(dat$acoustic))
		{
		f0<-list(formula=~1,fixed=list(index=which(M0_dll$f0$group==1),value=0))
		}		
	# FIT MODEL
	fit<-try(mark(data = M0_proc,
		ddl=M0_dll,
		model = "Closed", 
		model.parameters=list(
			p=p,
			f0=f0),
		threads=2,
		brief=TRUE,silent=TRUE),silent=TRUE)
	if(class(fit)[1]=="try-error" | is.null(fit))
		{
		outt<- data.frame(
			type="acoustic",
			ntrack=inputs$ntrack[indx],
			reps=inputs$reps[indx],
			p=inputs$p[indx],
			N=inputs$N[indx],
			nocc=inputs$nocc[indx],
			N_hat=-99,
			N_hat_se=-99,
			p_hat=-99,
			p_hat_se=-99)	
		}
	if(class(fit)[1]=="mark")
		{
		outt<- data.frame(
			type="acoustic",
			ntrack=inputs$ntrack[indx],
			reps=inputs$reps[indx],
			p=inputs$p[indx],
			N=inputs$N[indx],
			nocc=inputs$nocc[indx],
			N_hat=sum(fit$results$derived$`N Population Size`$estimate),
			N_hat_se=fit$results$derived$`N Population Size`$se[1],
			p_hat=fit$results$real$estimate[1],
			p_hat_se=fit$results$real$se[1])
		}
	# END ACOUSTIC

		# BEGIN PIT
	fit<-NULL
	if(is.null(dat$pit)==FALSE)
		{
		M0_proc<-process.data(data=dat$pit,
			model="Closed")
		M0_dll<-make.design.data(M0_proc)
		p<-list(formula=~1,share=TRUE)
	
		fit<-try(mark(data = M0_proc,
			ddl=M0_dll,
			model = "Closed", 
			model.parameters=list(
				p=p),
			threads=2,
			brief=TRUE,silent=TRUE),silent=TRUE)
		}

	if(class(fit)[1]=="try-error" | is.null(fit))
		{
		outt<- rbind(outt,data.frame(
			type="pit",
			ntrack=0,
			reps=inputs$reps[indx],
			p=inputs$p[indx],
			N=inputs$N[indx],
			nocc=inputs$nocc[indx],
			N_hat=-99,
			N_hat_se=-99,
			p_hat=-99,
			p_hat_se=-99))	
		}
	if(class(fit)[1]=="mark")
		{
		outt<- rbind(outt,data.frame(
			type="pit",
			ntrack=0,
			reps=inputs$reps[indx],
			p=inputs$p[indx],
			N=inputs$N[indx],
			nocc=inputs$nocc[indx],
			N_hat=fit$results$derived$`N Population Size`$estimate,
			N_hat_se=fit$results$derived$`N Population Size`$se,
			p_hat=fit$results$real$estimate[1],
			p_hat_se=fit$results$real$se[1]))
		}
	# END PIT


	return(outt)	
	}
	
plot_metrics<-function(metric,NN,xxt="s",main=FALSE,datt=pdat)
	{
	form<- as.formula(paste(metric,"~nocc"))
	ppdat<-subset(datt,N==NN)
	ppdat<- ppdat[order(ppdat$type,ppdat$p),]

	# NTRACK = 0 
	plot(form,ppdat,type='n',xaxt=xxt)
	p_true<- unique(combos$p)
    plotcol<-rev(grey((1:length(p_true))/(length(p_true)+2)))
	for(i in 1:length(p_true))
		{
		points(form,ppdat,type='l', subset=(N==NN & p==p_true[i] & type=="pit"),
			col=plotcol[i])
		}	
    if(main==TRUE){mtext(side=3,"Tags=0")}

	# NTRACK = 2
	plot(form,ppdat,type='n',xaxt=xxt,yaxt='n')
	for(i in 1:length(p_true))
		{
		points(form,ppdat,type='l', subset=(N==NN & p==p_true[i] & type=="acoustic" & ntrack==2),#2,5,10,20,30
			col=plotcol[i])
		}
    if(main==TRUE){mtext(side=3,"Tags=2")}

	# NTRACK = 5
    mn<-ifelse(main==TRUE, "Tags=5","")
	plot(form,ppdat,type='n',xaxt=xxt,yaxt='n')
	for(i in 1:length(p_true))
		{
		points(form,ppdat,type='l', subset=(N==NN & p==p_true[i] & type=="acoustic" & ntrack==5),
			col=plotcol[i])
		}	
    if(main==TRUE){mtext(side=3,"Tags=5")}


    # NTRACK = 10
    mn<-ifelse(main==TRUE, "Tags=10","")
	plot(form,ppdat,type='n',xaxt=xxt,yaxt='n')
	for(i in 1:length(p_true))
		{
		points(form,ppdat,type='l', subset=(N==NN & p==p_true[i] & type=="acoustic" & ntrack==10),
			col=plotcol[i])
		}			
    if(main==TRUE){mtext(side=3,"Tags=10")}


    # NTRACK = 20
    mn<-ifelse(main==TRUE, "Tags=20","")
	plot(form,ppdat,type='n',xaxt=xxt,yaxt='n')
	for(i in 1:length(p_true))
		{
		points(form,ppdat,type='l', subset=(N==NN & p==p_true[i] & type=="acoustic" & ntrack==20),
			col=plotcol[i])
		}				
    if(main==TRUE){mtext(side=3,"Tags=20")}


    # NTRACK = 30
    mn<-ifelse(main==TRUE, "Tags=30","")
	plot(form,ppdat,type='n',xaxt=xxt,yaxt='n')
	for(i in 1:length(p_true))
		{
		points(form,ppdat,type='l', subset=(N==NN & p==p_true[i] & type=="acoustic" & ntrack==30),
			col=plotcol[i])
		}		
    if(main==TRUE){mtext(side=3,"Tags=30")}


	}
	
	 