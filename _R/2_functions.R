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
    datLBgear<-subset(dat, gear==LBgears[x])
    dfit<-fitdistr(datLBgear$effort, "gamma")
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
reference_population<- function(segs=c(1,2,3,4,7,8,9,10,13,14),
    bends=NULL,
    fish_density=NULL,
    nyears=10,
    phi=0.95,
    Linf=rep(1000,10),
    k = rep(0.2,10),
    vbgf_vcv=NULL,
    initial_length=NULL)
    {
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
    ##  year (cols)
    ## Linf: a vector of mean Linf values for each segment
    ## k: a vector of mean k values for each segment
    ## vbgf_vcv: an array of variance and covariances for Linf and
    ##  k. Each matrix is for a segment
    ## initial_length: functions to simulate initial length given an
    ##  empirical distribution of segment specific lengths
    
    # outputs
    ## out: a list of 3 objects:
    ##  $out: a matrix where each row is a bend and 
    ##    each column represents a year; number
    ##  $bendMeta: a dataframe including the information in "bends"
    ##    with expected segment density (from init_dens), bend abundance,
    ##    and segment index columns added
    ##  $Z: a list of 472 matrices; each matrix, Z[[i]], gives individual 
    ##    fish status (0=Dead, 1=Alive) where each row represents a fish
    ##    living in bend i and each column represents a year  

    
    # assumptions
    ## no movement
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
    if(dim(vbgf_vcv)[1]!=2|dim(vbgf_vcv)[2]!=2){return("The variance covariance \n
        matrix needs to be 2x2 square")}
    if(dim(vbgf_vcv)[3]!=10){return("There needs to be 10 2x2 variance covariance \n
        matrices for each segment")}
    if(length(Linf)!=10){return("Linf needs to be a vector of 10 values for each segment")}
    if(length(k)!=10){return("k needs to be a vector of 10 values for each segment")}
    ## END: ERROR HANDLING   
    
    
    
    
    
    # GET BEND INFORMATION
    tmp<- subset(bends, b_segment %in% segs)
    tmp<- tmp[order(tmp$b_segment, tmp$bend_num),]## CRITICAL
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
    tmp<-tmp[order(tmp$b_segment,tmp$bend_num),]
    tmp$id<-c(1:nrow(tmp))# link for later...
    ## ASSIGN A BEND TO EACH INVIDUAL 
    ### EXPAND BENDS FOR EACH FISH
    individual_meta<- as.data.frame(lapply(tmp,function(x) rep(x,tmp$N_ini)))
    ### ASSIGN GROWTH PARAMETERS TO EACH INDIVIDUAL
    individual_meta$k<-0.02 # PULL VALUES FROM MV NORMAL
    #individual_meta$k<-k[individual_meta$phi_indx] 
    individual_meta$Linf<- 1200 # PULL VALUES FROM MV NORMAL
    #individual_meta$Linf<-Linf[individual_meta$phi_indx]
 
    ### Z: INDIVIDUAL SURVIVAL MATRIX WHERE EACH ROW IS A SINGLE FISH 
    ### IN THE GIVEN BEND AND EACH COLUMN IS A YEAR (0=Dead, 1=Alive)  
    Z<-matrix(0,nrow=nrow(individual_meta),ncol=nyears)  
    Z[,1]<-1
    ### l: length for individuals
    l<-matrix(0,nrow=nrow(individual_meta),ncol=nyears)
    
    
    for(i in unique(individual_meta$b_segment))
        {
        ## MAKE A QUICK FUNCTION OF THE INVERSE CUM DISTRIBUTION
        l_ini<-approxfun(
            initial_length[initial_length$segment==i,]$qntls,
            initial_length[initial_length$segment==i,]$vals,
            rule=2)
        indx<- which(individual_meta$b_segment==i)
        l[indx,1]<-l_ini(runif(length(indx)))
        }
    ## FIX ANY LENGTHS > THAN LINF
    l[,1]<- ifelse(l[,1]>= individual_meta$Linf,individual_meta$Linf*0.95,l[,1])    
   
    ## POPULATION DYNAMICS
    for(i in 2:nyears)
        {
        Z[,i]<- rbinom(nrow(Z),
            size=1,
            prob=phi[individual_meta$phi_indx,i-1]*Z[,i-1])
        ## FABENS MODEL FOR GROWTH (VBGF)
        l[,i]<-l[,i-1] + (individual_meta$Linf-l[,i-1])*(1-exp(-individual_meta$k*1))*Z[,i-1]# 0 growth if dead   
        }
 

 
    # MATRIX OF BEND LEVEL ABUNDANCES TO RETURN
    out<-aggregate(Z[,1],
                   by=list(individual_meta$b_segment,individual_meta$bend_num),
                   sum)
    names(out)[3]<-"yr_1"
    for(i in 2:nyears)
        {
        app<-aggregate(Z[,i],
                       by=list(individual_meta$b_segment,individual_meta$bend_num),
                       sum)
        names(app)[3]<-paste("yr",i,sep="_")
        out<-merge(out,app,by=c("Group.1","Group.2"),all=TRUE)
        }
    names(out)[1:3]<-c("b_segment","bend_num", "N_ini")
    if(length(out[is.na(out)])!=0){return(print("ERROR IN FISH COUNT"))} #ERROR HANDLING FOR DOUBLE CHECKING...SHOULD BE ABLE TO REMOVE
    out<-merge(out,tmp[,c("b_segment","bend_num", "N_ini")],
                 by=c("b_segment", "bend_num", "N_ini"), all=TRUE)
    if(nrow(out)!=nrow(tmp)){return(print("ERROR IN BEND ABUNDANCE MERGE"))} #ERROR HANDLING FOR DOUBLE CHECKING...SHOULD BE ABLE TO REMOVE
    out[is.na(out)]<-0
    out<-out[order(out$b_segment, out$bend_num),]
    tmp<- tmp[order(tmp$b_segment,tmp$bend_num),]
    out<-list(out=as.matrix(out[,-c(1:2)]), bendMeta=tmp,
        individual_meta=individual_meta,
        Z=Z,l=l)
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
    tmp<-tmp[order(tmp$b_segment, tmp$bend_num),] #CRITICAL
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
catch_data<-function(sim_pop=NULL,
    samp_type=NULL,
    gears=c("GN14", "GN18", "GN41", "GN81",
         "MF", "OT16", "TLC1", "TLC2", "TN"),
    catchability=NULL,
    B0_sd=NULL,
    deployments=rep(8,9),
    effort=NULL,
    occasions=3,
    individual_meta=NULL)
    {
    # USE SIM_POP TO DEFINE VARIABLES
    tmp<-sim_pop$bendMeta
    tmp<-tmp[order(tmp$b_segment, tmp$bend_num),] #CRITICAL
    b_abund<-sim_pop$out
    # Z_abund<-sim_pop$Z now a big matrix... delete
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
        tmp1<-tmp1[,names(tmp1) %in% c("b_segment","bend_num")]
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

        ### ADD INPUT MEAN CATCHABILITY AND LOG-ODDS SD
        out$q_mean<-catchability[k]
        out$B0_sd<-B0_sd[k]
        #dat_q<-aggregate(q~b_segment,out,mean)
        #names(dat_q)[2]<-"data_q_mean"
        #out<-merge(out,dat_q)
        #dat_q<-aggregate(q~b_segment,out,sd)
        #names(dat_q)[2]<-"data_q_sd"
        #out<-merge(out,dat_q)

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
    # GO PARRELLEL?
    ch<-lapply(1:ncol(sampled),function(yr)
        { 
        ## PULL OUT SAMPLED BENDS
        samp_indx<-which(sampled[,yr]==1)
        bend_ch<-lapply(samp_indx, function(x)
            {
            indx<- which(individual_meta$id==x) ## HERE IS THE LINK TO FIX ONCE MOVEMENT IS DYNAMIC
            Z_abund<-sim_pop$Z[indx,,drop=FALSE]  ## MAKE Z_ABUND
            if(nrow(Z_abund)==0) {occ_ch<-NULL}
            if(nrow(Z_abund)>0)
                {
                ## CREATE TABLE OF SAMPLED BENDS
                tmp1<-tmp[x,names(tmp) %in% c("b_segment","bend_num")]
                ## ADD YEAR SAMPLED
                tmp1$year<-yr
                ## EXPAND FOR INDIVIDUALS AND GEARS
                ### INDEX OF INDIVUALS IN EACH REACH
                tmp1<-tmp1[rep(1, nrow(Z_abund)*length(gears)),]
                tmp1$fish_id<-rep(1:nrow(Z_abund),length(gears))
                tmp1$fish_id<-paste0(x,".",tmp1$fish_id)
                tmp1$gear<-rep(gears, each=nrow(Z_abund))
                ## PULL OUT INDIVIDUAL DATA SURVIVAL
                ZZ<-matrix(rep(Z_abund[,yr],length(gears)),
                    nrow=length(Z_abund[,yr]),ncol=length(gears))
                ## FIND CH FOR EACH OCCASION, GEAR, AND INDIVIDUAL
                occ_ch<-lapply(1:occasions,function(occ,out=tmp1)
                    { 
                    ### EXPAND DATAFRAME TO INCLUDE OCCASION
                    out$occasion<-occ
                    ### FIND OCCASION LEVEL CP FOR GIVEN BEND AND YEAR
                    dat<-subset(b_samp, year==yr & b_segment==tmp$b_segment[x] 
                                & bend_num==tmp$bend_num[x] & occasion==occ)
                    P<-aggregate(p~gear,dat,sum)
                    #P<-aggregate(pnot~gear,dat,prod)
                    #P$p<-1-P$pnot
                    ### CAP CPs AT 1
                    P$p<-ifelse(P$p>1,1,P$p)
                    capture_probability<- ZZ%*%diag(P$p)
                    ### CH
                    ch_reps<-matrix(rbinom(length(ZZ), size=1,
                        prob=c(capture_probability)),
                        nrow=nrow(ZZ),
                        ncol=ncol(ZZ))
                    out$ch<-c(ch_reps)
                    out<-subset(out,ch==1)
                    return(out)
                    })
                occ_ch<-do.call(rbind,occ_ch)
                }
            return(occ_ch)
            })
        bend_ch<-do.call(rbind,bend_ch)
        })
    
    ch<-do.call(rbind,ch)


    
    # PROCESS THE DATA
    tmp1<-aggregate(length.rkm~b_segment, tmp,sum)
    tmp1<-tmp1[rep(seq_len(nrow(tmp1)),ncol(b_abund)),]
    tmp1$year<-rep(1:ncol(b_abund), each=length(unique(tmp$b_segment)))
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
    phi<-matrix(0,nrow=nrow(s_abund),ncol=ncol(s_abund))
    for(i in 1:(nrow(phi)-1))
        {
        phi[i,]<-s_abund[i+1,]/s_abund[i,]
        }
    phi[nrow(phi),]<-NA
    tmp1$phi<-c(phi)
    tmp1$density<-tmp1$abundance/tmp1$length.rkm
    tmp1<-tmp1[,c(1,3:6)]
    b_samp<-b_samp[,which(names(b_samp)!="p")]
    ch<-ch[,which(names(ch)!="ch")]
    return(list(true_vals=tmp1, samp_dat=b_samp, catch_dat=ch))
    }




## 6. GETTING CPUE TREND
### CALCULATING TREND FOR OCCASION 1 DATA ONLY
get.trnd<-function(sim_dat=NULL,
                   gears=c("GN14", "GN18", "GN41", "GN81", "MF", 
                           "OT16", "TLC1", "TLC2", "TN")) 
{
  # GET CATCH AND EFFORT DATA
  ## CATCH
  tmp<-aggregate(fish_id~b_segment+year+gear,
                 sim_dat$catch_dat, length, subset=occasion==1)
  names(tmp)[which(names(tmp)=="fish_id")]<-"catch"
  ## EFFORT
  tmp2<-aggregate(f~b_segment+year+gear,
                 sim_dat$samp_dat, sum, subset=occasion==1)
  tmp<-merge(tmp2, tmp, all.x=TRUE)
  # CALCULATE SEGMENT-LEVEL CPUE AND LN(CPUE)
  ## USE CATCH +1 TO AVOID CPUE=0
  tmp$catch1<-ifelse(tmp$f==0,0,tmp$catch+1)
  tmp$cpue1<-tmp$catch1/tmp$f
  tmp$lncpue1<-log(tmp$cpue1)
  
  # FIT LINEAR MODEL FOR TREND FOR EACH GEAR
  tmp$b_segment<- as.factor(tmp$b_segment)
  out<-lapply(gears,function(g)
    {
      fit<- lm(lncpue1~b_segment+year, tmp, subset=gear==g)
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
      return(tmp2)
      }
    )
  out<-do.call(rbind,out)
  # CALCULATE TREND BIAS AND PRECISION
  ## ACTUAL POPULATION TREND
  sim_dat$true_vals$b_segment<- as.factor(sim_dat$true_vals$b_segment)
  fit<-lm(log(abundance)~b_segment+year,sim_dat$true_vals)
  out$pop_trnd<-unname(coef(fit)['year'])
  ## BIAS
  out$bias<-out$trnd-out$pop_trnd
  ## PRECISION AS THE COEFFICIENT OF VARIATION
  out$cv<-out$se/abs(out$trnd)
  # OUTPUT THE GOODIES
  return(out)
}
 

 
## 6. GETTING CPUE ABUNDANCE
### CALCULATING ABUNDANCE FOR OCCASION 1 DATA ONLY
get.abund<-function(sim_dat=NULL,
                    bends=NULL)
{
  ## ESTIMATE ABUNDANCE USING CATCH DENSITY
  ### GET CATCH FOR EACH SAMPLED BEND
  tmp<-aggregate(fish_id~b_segment+bend_num+year+gear,
                 sim_dat$catch_dat, length, subset=occasion==1)
  names(tmp)[which(names(tmp)=="fish_id")]<-"catch"
  #### ADD SAMPLED BENDS WITH ZERO CATCH
  sampled<-ddply(subset(sim_dat$samp_dat, occasion==1),.(b_segment,bend_num,year,gear),
                 summarize,
                 occ=mean(occasion))
  tmp<-merge(tmp, sampled, by=c("b_segment","bend_num","year","gear"),all.y=TRUE)
  tmp[which(is.na(tmp$catch)),]$catch<-0
  ### ADD BEND LENGTH
  tmp<-merge(tmp,bends[,c(2,3,9)],by=c("b_segment","bend_num"),all.x=TRUE)
  ### ADD BEND CATCH DENSITY
  tmp$catch_dens<-tmp$catch/tmp$length.rkm
  ### GET SEGMENT DENSITY ESTIMATES
  ests<-ddply(tmp,.(b_segment,year,gear),
              summarize,
              samp_size=sum(occ),
              mean_dens=mean(catch_dens),
              sd_dens=sd(catch_dens),
              catch=sum(catch),
              samp_length=sum(length.rkm))
  ests$WM_dens<-ests$catch/ests$samp_length
  ### ADD SEGMENT LENGTHS
  ests<-merge(ests,aggregate(length.rkm~b_segment, data=bends,sum),
              by="b_segment",all.x=TRUE)
  ### ESTIMATE ABUNDANCE
  ests$Nhat_AM<-ests$length.rkm*ests$mean_dens #arithmetic mean
  ests$Nhat_WM<-ests$length.rkm*ests$WM_dens #weighted arithmetic mean
  
  ### ADD TRUE ABUNDANCE
  ests<-merge(ests, sim_dat$true_vals[,1:3], by=c("b_segment","year"), all.x=TRUE)
  
  ### ADD BIAS
  ests$bias_AM<-ests$Nhat_AM-ests$abundance
  ests$bias_WM<-ests$Nhat_WM-ests$abundance
  
  ### ADD PRECISION AS CV
  #### CALCULATE WEIGHTED MEAN SD
  tmp<-merge(tmp, ests[,c(1:3,8,9)])
  tmp$diffsq<-(tmp$catch_dens-tmp$WM_dens)^2
  Wsd<-ddply(tmp,.(b_segment,year,gear),
              summarize,
              Wsd_dens=sqrt(sum((length.rkm/samp_length)*diffsq)))
  ests<-merge(ests,Wsd)
  #### CALCULATE CV
  ests$cv_AM<-ests$sd_dens/abs(ests$Nhat_AM)
  ests$cv_WM<-ests$Wsd_dens/abs(ests$Nhat_WM)
  
  ## OUTPUT ESTIMATES
  ests<-ests[,c(1:4,11:15,17,18)]
  return(ests)
}
 
 




## 7. GETTING M0 ESTIMATES
get.M0t.ests<-function(sim_dat=NULL,
                      bends=NULL)
{
  ## PULL TRUE VALUES
  true<-sim_dat$true_vals[,1:3]
  colnames(true)[c(1,3)]<-c("segment", "N")
  
  ## MASSAGE DATA INTO SHAPE FOR 
  ## CAPTURE HISTORIES
  catch<-sim_dat$catch_dat
  catch$ch<-1 #all fish on list were captured
  gears<-unique(catch$gear) #identify gears that caught fish
  occ<-as.character(unique(catch$occasion))
  ch<-lapply(gears,function(g)
  {
    pp<- dcast(catch, 
               b_segment+bend_num+year+fish_id~occasion,
               value.var="ch", sum, subset=.(gear==g))
    pp$gear<-g
    return(pp)
  })
  ch<-do.call(rbind,ch)
  
  ## PULL SAMPLED BENDS
  samps<-subset(sim_dat$samp_dat, deployment==1 & occasion==1)
  samps<-subset(samps,f!=0) # remove unused gears
  ### ADD BEND RKM
  samps<- merge(samps,bends[,c(2,3,9)], by=c("b_segment","bend_num"))
  
  ## RUN M0 ESTIMATOR
  bend_Np<- lapply(1:nrow(samps),function(x)
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
      warn_codes_M0<-ifelse(tmp$results[1,7]==0,0,
                         ifelse(grepl("bias",tmp$glm.warn$M0, fixed=TRUE),"b",
                                ifelse(grepl("sigma",tmp$glm.warn$M0, fixed=TRUE),"s",
                                       ifelse(grepl("converge",tmp$glm.warn$M0, fixed=TRUE),"c",
                                              ifelse(grepl("0 occurred",tmp$glm.warn$M0, fixed=TRUE),"z",
                                                     tmp$glm.warn$M0)))))
      warn_codes_Mt<-ifelse(tmp$results[2,7]==0,0,
                            ifelse(grepl("bias",tmp$glm.warn$Mt, fixed=TRUE),"b",
                                   ifelse(grepl("sigma",tmp$glm.warn$Mt, fixed=TRUE),"s",
                                          ifelse(grepl("converge",tmp$glm.warn$Mt, fixed=TRUE),"c",
                                                 ifelse(grepl("0 occurred",tmp$glm.warn$Mt, fixed=TRUE),"z",
                                                        tmp$glm.warn$Mt)))))
      tmp<- data.frame(## collect up relevant bits for M0 and Mt
        year=samps$year[x],
        segment=samps$b_segment[x],
        bendId=samps$bend_num[x],
        gear=samps$gear[x],
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
        bendId=samps$bend_num[x],
        gear=samps$gear[x],
        rkm=samps$length.rkm[x],
        samp_size=0,
        Nhat_M0=-99,
        SE_Nhat_M0=-99,
        p_M0=-99,
        fit_M0=-99,
        warn_M0=-99,
        Nhat_Mt=-99,
        SE_Nhat_Mt=-99,
        p_Mt=-99,
        fit_Mt=-99,
        warn_Mt=-99
        )}
    return(tmp)    
  }) # ABOUT 15 MINUTES
  bend_Np<- do.call("rbind", bend_Np)
  bend_Np[bend_Np$fit_M0!=0,]$Nhat_M0<-NA ## make non converged and no fish models NA
  bend_Np[bend_Np$fit_M0!=0,]$SE_Nhat_M0<-NA ## make non converged and no fish models NA
  bend_Np[bend_Np$fit_Mt!=0,]$Nhat_Mt<-NA ## make non converged and no fish models NA
  bend_Np[bend_Np$fit_Mt!=0,]$SE_Nhat_Mt<-NA ## make non converged and no fish models NA
  bend_Np$rkm_M0<-ifelse(is.na(bend_Np$Nhat_M0),NA,bend_Np$rkm)  ## needed to exclude in WM calculation
  bend_Np$rkm_Mt<-ifelse(is.na(bend_Np$Nhat_Mt),NA,bend_Np$rkm)  ## needed to exclude in WM calculation
  
  ## CALCULATE BEND LEVEL DENSITY FROM ESTIMATES
  bend_Np$dens_M0<-bend_Np$Nhat_M0/bend_Np$rkm
  bend_Np$dens_Mt<-bend_Np$Nhat_Mt/bend_Np$rkm
  
  ## SUMMARIZE ESTIMATES TO SEGMENT LEVEL 
  ests<-ddply(bend_Np,.(segment,year,gear),
              summarize,
              mn_M0=mean(dens_M0,na.rm = TRUE),
              n_st_M0=length(which(!is.na(dens_M0))),
              v_tmp_M0=sum((1/rkm)^2*SE_Nhat_M0^2,na.rm=TRUE),
              N_sst_M0=sum(Nhat_M0,na.rm=TRUE),
              d_sst_M0=sum(rkm_M0,na.rm=TRUE),
              v_tmp2_M0=sum(SE_Nhat_M0^2, na.rm=TRUE),
              perform_M0=length(which(fit_M0==0))/length(fit_M0),
              mn_Mt=mean(dens_Mt,na.rm = TRUE),
              n_st_Mt=length(which(!is.na(dens_Mt))),
              v_tmp_Mt=sum((1/rkm)^2*SE_Nhat_Mt^2,na.rm=TRUE),
              N_sst_Mt=sum(Nhat_Mt,na.rm=TRUE),
              d_sst_Mt=sum(rkm_Mt,na.rm=TRUE),
              v_tmp2_Mt=sum(SE_Nhat_Mt^2, na.rm=TRUE),
              perform_Mt=length(which(fit_Mt==0))/length(fit_Mt))
    # To remove segments that had no bends with successful
    # sampling catch data for the given gear: 
    # ests<-subset(ests,n_st!=0) 
  ### CALCULATE SEGMENT LENGTH
  segmentDistance<- aggregate(length.rkm~b_segment,bends,sum)
  colnames(segmentDistance)[1]<-"segment"
  ### MERGE
  ests<- merge(ests,segmentDistance, by="segment")
  ### CALCULATE SAMPLED SEGMENT DENSITY
  ests$dens_sst_M0<-ests$N_sst_M0/ests$d_sst_M0
  ests$dens_sst_Mt<-ests$N_sst_Mt/ests$d_sst_Mt
  ### ESTIMATE SEGMENT LEVEL ABUNDANCE
  ests$Nhat_AM_M0<- ests$length.rkm*ests$mn_M0  
  ests$Nhat_WM_M0<- ifelse(ests$n_st_M0>0,ests$length.rkm*ests$dens_sst_M0,NA)
  ests$Nhat_AM_Mt<- ests$length.rkm*ests$mn_Mt  
  ests$Nhat_WM_Mt<- ifelse(ests$n_st_Mt>0,ests$length.rkm*ests$dens_sst_Mt,NA)
  
  ## ABUNDANCE BIAS AND  PRECISION
  ### MERGE ESTIMATES WITH TRUE VALUES
  ests<-merge(ests,true,by=c("year","segment"))
  ests<- ests[order(ests$segment,ests$year),]
  ## CALCULATE ABUNDANCE BIAS
  ests$abund_bias_AM_M0<-ests$Nhat_AM_M0-ests$N
  ests$abund_bias_WM_M0<-ests$Nhat_WM_M0-ests$N
  ests$abund_bias_AM_Mt<-ests$Nhat_AM_Mt-ests$N
  ests$abund_bias_WM_Mt<-ests$Nhat_WM_Mt-ests$N
  ## CALCULATE ABUNDANCE PRECISION
  ests$abund_var_AM_M0<-(ests$length.rkm/ests$n_st_M0)^2*ests$v_tmp_M0
  ests$abund_var_WM_M0<-ifelse(ests$n_st_M0>0,(ests$length.rkm/ests$d_sst_M0)^2*ests$v_tmp2_M0,NA)
  ests$abund_var_AM_Mt<-(ests$length.rkm/ests$n_st_Mt)^2*ests$v_tmp_Mt
  ests$abund_var_WM_Mt<-ifelse(ests$n_st_Mt>0,(ests$length.rkm/ests$d_sst_Mt)^2*ests$v_tmp2_Mt,NA)
  ests$abund_cv_AM_M0<-sqrt(ests$abund_var_AM_M0)/abs(ests$Nhat_AM_M0)
  ests$abund_cv_WM_M0<-sqrt(ests$abund_var_WM_M0)/abs(ests$Nhat_WM_M0)
  ests$abund_cv_AM_Mt<-sqrt(ests$abund_var_AM_Mt)/abs(ests$Nhat_AM_Mt)
  ests$abund_cv_WM_Mt<-sqrt(ests$abund_var_WM_Mt)/abs(ests$Nhat_WM_Mt)
  ests<-ests[,c("year","segment","gear","N","n_st_M0","Nhat_AM_M0", 
                "abund_bias_AM_M0","abund_cv_AM_M0","Nhat_WM_M0", 
                "abund_bias_WM_M0","abund_cv_WM_M0","perform_M0","n_st_Mt",
                "Nhat_AM_Mt", "abund_bias_AM_Mt","abund_cv_AM_Mt","Nhat_WM_Mt",
                "abund_bias_WM_Mt","abund_cv_WM_Mt","perform_Mt")]
  
  ## TREND BIAS AND PRECISION
  ### FIT LINEAR MODEL FOR TREND FOR EACH GEAR
  out<-lapply(gears,function(g)
  {
    tmp<-subset(ests, gear==g)
    perform_M0<-length(which(tmp$n_st_M0!=0))/length(tmp$n_st_M0)
    perform_Mt<-length(which(tmp$n_st_Mt!=0))/length(tmp$n_st_Mt)
    tmp_M0<-subset(tmp,!is.na(Nhat_AM_M0))
    tmp_Mt<-subset(tmp,!is.na(Nhat_AM_Mt))
    if(perform_M0>0 #enough data
       & length(unique(tmp_M0$segment))>1 # more than one segment
       & length(unique(tmp_M0$year))>=2) # at least two years
    {
      fit_AM_M0<-lm(log(Nhat_AM_M0)~year+as.factor(segment),tmp_M0)
      fit_WM_M0<-lm(log(Nhat_WM_M0)~year+as.factor(segment),tmp_M0)
      tmp2_M0<- data.frame( 
        # THE GOODIES
        ## GEAR
        gear=g,
        ## ARITHMETIC MEAN
        ### TREND ESTIMATE
        trnd_AM_M0=ifelse(is.na(summary(fit_AM_M0)$coefficients['year',2]),NA,
                          coef(fit_AM_M0)['year']),
        ### STANDARD ERROR FOR TREND ESTIMATE
        se_AM_M0=summary(fit_AM_M0)$coefficients['year',2],
        ### PVALUE FOR TREND ESTIMATE
        pval_AM_M0=summary(fit_AM_M0)$coefficients['year',4],
        ## WEIGHTED ARITHMETIC MEAN
        ### TREND ESTIMATE
        trnd_WM_M0=ifelse(is.na(summary(fit_WM_M0)$coefficients['year',2]),NA,
                          coef(fit_WM_M0)['year']),
        ### STANDARD ERROR FOR TREND ESTIMATE
        se_WM_M0=summary(fit_WM_M0)$coefficients['year',2],
        ### PVALUE FOR TREND ESTIMATE
        pval_WM_M0=summary(fit_WM_M0)$coefficients['year',4],
        ## PERFORMANCE (FRACTION OF SEGMENT-YEAR DATA USED)
        perform_M0=perform_M0
      )
    }  
    if(perform_M0==0 #no data
       | length(unique(tmp_M0$segment))<=1 #or only one segment
       | length(unique(tmp_M0$year))<2) #or less than two years 
    {
      tmp2_M0<- data.frame( 
        gear=g,
        trnd_AM_M0=NA,
        se_AM_M0=NA,
        pval_AM_M0=NA,
        trnd_WM_M0=NA,
        se_WM_M0=NA,
        pval_WM_M0=NA,
        perform_M0=0 #NOT ENOUGH DATA TO CALCULATE TREND
      )
    }
    if(perform_Mt>0 #enough data
       & length(unique(tmp_Mt$segment))>1 # more than one segment
       & length(unique(tmp_Mt$year))>=2) # at least two years
    {
      fit_AM_Mt<-lm(log(Nhat_AM_Mt)~year+as.factor(segment),tmp_Mt)
      fit_WM_Mt<-lm(log(Nhat_WM_Mt)~year+as.factor(segment),tmp_Mt)
      tmp2_Mt<- data.frame( 
        # THE GOODIES
        ## GEAR
        gear=g,
        ## ARITHMETIC MEAN
        ### TREND ESTIMATE
        trnd_AM_Mt=ifelse(is.na(summary(fit_AM_Mt)$coefficients['year',2]),NA,
                          coef(fit_AM_Mt)['year']),
        ### STANDARD ERROR FOR TREND ESTIMATE
        se_AM_Mt=summary(fit_AM_Mt)$coefficients['year',2],
        ### PVALUE FOR TREND ESTIMATE
        pval_AM_Mt=summary(fit_AM_Mt)$coefficients['year',4],
        ## WEIGHTED ARITHMETIC MEAN
        ### TREND ESTIMATE
        trnd_WM_Mt=ifelse(is.na(summary(fit_WM_Mt)$coefficients['year',2]),NA,
                          coef(fit_WM_Mt)['year']),
        ### STANDARD ERROR FOR TREND ESTIMATE
        se_WM_Mt=summary(fit_WM_Mt)$coefficients['year',2],
        ### PVALUE FOR TREND ESTIMATE
        pval_WM_Mt=summary(fit_WM_Mt)$coefficients['year',4],
        ## PERFORMANCE (FRACTION OF SEGMENT-YEAR DATA USED)
        perform_Mt=perform_Mt
      )
    }  
    if(perform_Mt==0 #no data
       | length(unique(tmp_Mt$segment))<=1 #or only one segment
       | length(unique(tmp_Mt$year))<2) #or less than two years 
    {
      tmp2_Mt<- data.frame( 
        gear=g,
        trnd_AM_Mt=NA,
        se_AM_Mt=NA,
        pval_AM_Mt=NA,
        trnd_WM_Mt=NA,
        se_WM_Mt=NA,
        pval_WM_Mt=NA,
        perform_Mt=0 #NOT ENOUGH DATA TO CALCULATE TREND
      )
    }
    tmp2<-merge(tmp2_M0,tmp2_Mt)
    return(tmp2)
  })
  out<-do.call(rbind,out)
  
  ### MERGE ACTUAL POPULATION TREND
  fit<-lm(log(N)~year+as.factor(segment),true)
  out$pop_trnd<-unname(coef(fit)['year'])
  
  ### TREND BIAS
  #######################################
  #  DO WE WANT EXP VERSION OF THIS???  #
  #######################################
  out$bias_AM_M0<-out$trnd_AM_M0-out$pop_trnd
  out$bias_WM_M0<-out$trnd_WM_M0-out$pop_trnd
  out$bias_AM_Mt<-out$trnd_AM_Mt-out$pop_trnd
  out$bias_WM_Mt<-out$trnd_WM_Mt-out$pop_trnd
  
  ### TREND PRECISION (as coefficient of variation)
  out$cv_AM_M0<-out$se_AM_M0/abs(out$trnd_AM_M0)
  out$cv_WM_M0<-out$se_WM_M0/abs(out$trnd_WM_M0)
  out$cv_AM_Mt<-out$se_AM_Mt/abs(out$trnd_AM_Mt)
  out$cv_WM_Mt<-out$se_WM_Mt/abs(out$trnd_WM_Mt)
  out<-out[,c("gear", "pop_trnd","trnd_AM_M0","bias_AM_M0", "cv_AM_M0",
              "pval_AM_M0", "trnd_WM_M0", "bias_WM_M0", "cv_WM_M0",
              "pval_WM_M0", "perform_M0","trnd_AM_Mt","bias_AM_Mt",
              "cv_AM_Mt", "pval_AM_Mt", "trnd_WM_Mt", "bias_WM_Mt", "cv_WM_Mt",
              "pval_WM_Mt", "perform_Mt")]
  return(list(M0t_trnd=out,M0t_abund=ests))
}
 
 
 
 
 
 
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
	
	 