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
    phi=0.95)
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
    ## EXAPAND BENDS FOR EACH FISH
    indvidual_meta<- expanded.data<-as.data.frame(lapply(tmp,
                   function(x) rep(x,tmp$N_ini)))
    ## SET UP INVIDUAL POPULATION
    Z<- lapply(1:nrow(tmp),function(x)
        {
        ### y: INDIVIDUAL SURVIVAL MATRIX WHERE EACH ROW IS A SINGLE FISH 
        ### IN THE GIVEN BEND AND EACH COLUMN IS A YEAR (0=Dead, 1=Alive) 
        y<-matrix(0,nrow=tmp$N_ini[x],ncol=nyears) 
        y[,1]<-1
        for(i in 2:nyears)
            {
            y[,i]<- rbinom(tmp$N_ini[x],
                size=1,
                prob=phi[tmp$phi_indx[x],i-1]*y[,i-1])
            }
        return(y)
        })
    # MATRIX OF BEND LEVEL ABUNDANCES TO RETURN
    out<- lapply(1:nrow(tmp),function(x)
        {
        colSums(Z[[x]])
        })
    out<- as.matrix(do.call("rbind",out))
    
    out<-list(out=out, bendMeta=tmp,Z=Z)
    return(out)# return relevant stuff
}



## 4. FUNCTION TO DETERMINE CATCH COUNTS IN A BEND FOR ALL 
## STANDARD, COMMON GEARS
catch_counts<-function(sim_pop=NULL,
                       gears=c("GN14", "GN18", "GN41", "GN81",
                               "MF", "OT16", "TLC1", "TLC2", "TN"),
                        catchability=c(0.00002, 0.00004, 0.00002, 0.00004,
                                       0.00004, 0.0002, 0.00002, 0.00004,
                                       0.0002),
                        q_sd=c(0.08, 0.1, 0.08, 0.1, 0.07, 1.2, 0.08, 0.1, 1.2),
                        deployments=rep(8,9),
                        effort=NULL,
                        occasions=3)
{ 
  # this function calculates the realized effort, capture history, 
  # and number of fish caught by each gear in each bend within a 
  # segment probabilistically given a gamma distribution for effort
  # and the individual survival status of each fish within a bend
  
  # a formula for catchability (q) still needs to be worked in
  # currently q is a vector of catchabilities by gear
  # Is the environment different enough that we need by basin as well?
  
  # inputs
  ## sim_pop: a simulated population using the reference_populations function
  ##  having components:
  ##    $bendMeta: a dataframe of bend info (including the RPMA for each bend)
  ##    $Z: a list of 472 matrices; each matrix, Z[[i]], gives individual 
  ##      fish status (0=Dead, 1=Alive) where each row represents a fish
  ##      living in bend i and each column represents a year    
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
  {return(print("Gears not found in the effort analysis will have \n 
                 no catch and cpue will be NaN."))}
  if(length(catchability)!=length(gears))
  {return(print("Catchability needs to be a vector with length equal \n
                  to the length of the vector of gears (catchabilities \n
                  for each given gear in the same order given)."))}
  if(length(deployments)!=length(gears))
  {return(print("Deployments needs to be a vector with length equal \n
                  to the length of the vector of gears (deployment \n 
                  numbers for each given gear in the same order given)."))}
  # USE SIM_POP TO DEFINE VARIABLES
  tmp<-sim_pop$bendMeta
  Z_abund<-sim_pop$Z

  # BEND-SPECIFIC CATCHABILITY BY GEAR
    ## BEND BY YEAR BY GEAR ARRAY
    ## ASSUMES GEAR CATCHABILITY DOES NOT VARY AMONG BASINS
    B0<- log(catchability/(1-catchability))
    ## COULD MAKE A MATRIX TO VARY 'MEAN' q BY BEND
    #bend_B0_matrix<- matrix(rnorm(n=length(B0),
    #      mean=B0,sd2),ncol=length(catchability),
    #   nrow=length(Z_abund),
    #   byrow=TRUE)
        
    ## BEND AND GEAR SPECIFIC NUMBER OF DEPLOYMENTS
    ## CURRENTLY NO DIFFERENCE IN THIS BY BEND (OR YEAR)
    d<-matrix(deployments,ncol=length(deployments),
        nrow=length(Z_abund),
        byrow=TRUE)
    
    ## DETERMINE THE AMOUNT OF EFFORT
    ## FOR EACH BEND, YEAR, REPLICATE AND GEAR
    f_P<- lapply(1:length(Z_abund),function(x)
        {
        bend_f<-list()## TO HOLD OUTPUT OF EFFORTS
        bend_q<-list()## TO HOLD OUTPUT OF CATCHABILITY
        bend_P<-list()## TO HOLD OUTPUT OF CAPTURE PROBABILITIES
        bend_P_flags<-list()## TO HOLD HIGH CAPTURE PROBABILITY FLAG OUTPUT
            
        ## LOOP OVER GEARS AND GENERATE FOR EACH BEND
        ## (x) THE AMOUNT OF EFFORT GIVEN THE NUMBER OF 
        ## OCCASIONS AND THE OCCASION AND BEND SPECIFIC 
        ## CAPTURE PROBABILITY ACCOUNTING FOR NUMBER OF DEPLOYMENTS
        for(k in 1:length(gears))
            {
            #This potentially eliminates the need for g ...but g also  
            #relates to deployments and catchability, so be careful
            indx<- which(effort$gear==gears[k] & 
                effort$rpma==tmp$rpma[x])
            ## ERROR HANDLING FOR GEARS THAT ARE NOT USED 
            ## RPMAS, THEREFORE NO EFFORT AND f=0
            f_reps<-list()
            q_reps<-list()
            p_reps<-list()
            p_flags<-list()
            for(occ in 1:occasions)
                {
                if(length(indx)==0)
                    {
                    f_reps[[occ]]<- matrix(0,
                        nrow=d[x],
                        ncol=nyears)
                    }
                if(length(indx)>0)
                    {
                    #bend_f[[gears[k]]][[occ]]<-matrix(
                    f_reps[[occ]]<-matrix(
                        rgamma(n=d[x,k]*nyears,
                            shape=effort$gamma_shape[indx], 
                            rate=effort$gamma_rate[indx]),
                        nrow=d[x,k],
                        ncol=nyears)
                    }
                q_reps[[occ]]<-matrix(plogis(B0[k]+rnorm(n=d[x,k]*nyears,mean=0,sd=q_sd[k])),
                  nrow=d[x,k],
                  ncol=nyears)
                np<-1-f_reps[[occ]]*q_reps[[occ]]
                p_reps[[occ]]<-unlist(lapply(1:nyears, function(yy)
                  {
                    return(1-prod(np[,yy]))
                  }))
                #HIGH CAPTURE PROBABILITY FLAG
                p_flags[[occ]]<-ifelse(p_reps[[occ]]>0.4,1,0)
                }# end occ
            p_flags<-colSums(do.call("rbind", p_flags))
            bend_f[[gears[k]]]<-f_reps
            bend_q[[gears[k]]]<-q_reps
            bend_P[[gears[k]]]<-p_reps
            bend_P_flags[[gears[k]]]<-p_flags
            } # end k (gears)
            bend_P_flags<-do.call("rbind",bend_P_flags)
        return(list(f=bend_f, q=bend_q, P=bend_P, P_flag=bend_P_flags))
        })
    pull_flags<-do.call(rbind, sapply(f_P, "[[", "P_flag", simplify=FALSE))
    tmp<-data.frame(
      year=rep(1:nyears, each=length(f_P)*length(gears)), 
      bend_indx=rep(1:length(f_P), each=length(gears), times=nyears),
      gear=rep(gears, times=length(f_P)*nyears), 
      flags=c(pull_flags))
    tmp$b_segment=as.numeric(sim_pop$bendMeta$b_segment[tmp$bend_indx])
    tmp$bend_num=as.numeric(sim_pop$bendMeta$bend_num[tmp$bend_indx])
    tmp<-tmp[,colnames(tmp)!="bend_indx"]
   
    ## CAPTURE HISTORY FOR EACH INVIDUAL, BEND,
    ## GEAR AND OCCASSION 
    ch<- lapply(1:length(Z_abund),function(x)
      {
        bend_ch<-list()## TO HOLD OUTPUT OF CAPTURE HISTORIES
        bend_N<-list()## TO HOLD OUTPUT OF CAPTURE HISTORIES
        ZZ<- Z_abund[[x]]
        ## LOOP OVER GEARS AND GENERATE FOR EACH BEND
        ## (x) THE AMOUNT OF EFFORT GIVEN THE NUMBER OF 
        ## OCCASIONS AND THE OCCASION AND BEND SPECIFIC 
        ## CAPTURE PROBABILITY ACCOUNTING FOR NUMBER OF DEPLOYMENTS
        for(k in 1:length(gears))
            {
            ch_reps<-list()
            for(occ in 1:occasions)
                {
                capture_probability<- ZZ%*%diag(f_P[[x]]$P[[gears[k]]][[occ]])
                ch_reps[[occ]]<-matrix(
                    rbinom(length(ZZ), size=1,
                       prob=c(capture_probability)),
                    nrow=nrow(ZZ),
                    ncol=ncol(ZZ))
                }# end occ
            bend_ch[[gears[k]]]<-ch_reps
            bend_N[[gears[k]]]<-lapply(ch_reps, function(yy)
                {
                colSums(yy)         
                })
            } # end k (gears)
        return(list(ch=bend_ch, N=bend_N))
        })
    
    ### PROCESS UP THE GOODIES
    bend_effort<-bend_catch<-list()
    for(ii in 1:length(gears))
        {
        outt<-lapply(1:length(Z_abund),function(y)
            {
            ch[[y]][['N']] [[gears[ii]]][[1]]
            })
        bend_catch[[gears[ii]]]<-do.call("rbind",outt)
        outt<-lapply(1:length(Z_abund),function(y)
            {
            colSums(f_P[[y]][['f']] [[gears[ii]]][[1]])
            #TOTAL EFFORT OVER ALL DEPLOYMENTS IN A YEAR
            })
        bend_effort[[gears[ii]]]<-do.call("rbind",outt)
        }
    return(list(catch=bend_catch,
        f=bend_effort, 
        ch=lapply(ch, function(x) x$ch),
        f_occ = lapply(f_P, function(x) x$f),
        q_occ = lapply(f_P, function(x) x$q),
        flags=tmp)) 
}


 
## 5. FUNCTION TO DETERMINE WHICH BENDS WITHIN A SEGMENT 
## TO SAMPLE
bend_samples<-function(sim_pop=NULL)
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
  
  # GET BEND INFORMATION
  tmp<-sim_pop$bendMeta
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
  
  # DETERMINE WHICH BENDS IN A SEGMENT TO SAMPLE
  abund<-sim_pop$out
  sampled<-matrix(0,nrow=nrow(abund), ncol=ncol(abund))
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
  
  # EXPAND TMP (BEND METADATA)
  tmp<-tmp[,!names(tmp) %in% c("N_ini")]
  out<- tmp[rep(1:nrow(tmp),nyears),]
  names(out)<-toupper(names(out))
  out$SAMPLED<- c(sampled)
  out$B_ABUNDANCE<- c(abund)
  out$YEAR<- sort(rep(c(1:nyears),nrow(tmp)))

  ## ADD SEGMENT ABUNDANCE
  s_abund<-aggregate(B_ABUNDANCE~B_SEGMENT+YEAR,out,sum)
  names(s_abund)[3]<-"s_abund"
  out<-merge(out,s_abund,by=c("B_SEGMENT","YEAR"),all.x=TRUE)

  ## ADD RPMA ABUNDANCE
  r_abund<-aggregate(B_ABUNDANCE~RPMA+YEAR,out,sum)
  names(r_abund)[3]<-"r_abund"
  out<-merge(out,r_abund,by=c("RPMA","YEAR"),all.x=TRUE)
  
  ## SORT
  out<-out[order(out$YEAR, out$B_SEGMENT, out$BEND_NUM),]

  ## NAMES TO LOWERCASE
  names(out)<- tolower(names(out))
  
  # RETURN LONG FORM DATA AND MATRIX FORM BENDS TO SAMPLE
  return(list(bendLong=out,sampled=sampled))
  }
  
  
## 6. LOOKING AT CATCH FROM THE SAMPLED AND NON-SAMPLED BENDS
## IN "LONG FORM" 
samp_dat<-function(sim_pop=NULL,
    gears=c("GN14", "GN18", "GN41", "GN81", "MF", 
        "OT16", "TLC1", "TLC2", "TN"),
    catchability=c(0.00004, 0.00004, 0.00004, 
            0.00004, 0.00004, 0.0002, 0.00004, 
            0.00004, 0.0002),
    q_sd=NULL,#c(0.08, 0.1, 0.08, 0.1, 0.07, 1.2, 0.08, 0.1, 1.2),
    deployments=rep(8,9),
    effort=NULL,
    occasions=1)
    {
    #DETERMINE WHICH BENDS TO SAMPLE
    sim_samp<-bend_samples(sim_pop=sim_pop)
    
    sampled<-sim_samp$sampled
  
    #ADD CATCH AND EFFORT FOR GEARS
    ## NOT SLOW BUT ABOUT 4 SECONDS TO 
    ## RUN. MIGHT BE A PLACE TO COME BACK 
    ## TO FOR PERFORMANCE ENHANCMENT
    sim_catch<-catch_counts(sim_pop=sim_pop,
        gears=gears,
        catchability=catchability,
        deployments=deployments,
        effort=effort,
        occasions=occasions)

    catch<- sim_catch$catch
    effort2<- sim_catch$f
  
    ## CALCULATE CPUE
    cpue<-list()
    for(k in 1:length(gears))
      {
      cpue[[gears[k]]]<-catch[[gears[k]]]/effort2[[gears[k]]]
      }
    
    ## CONVERT CPUE TO LONG FORMAT
    xxx<- lapply(1:length(gears),function(x)
        {
        bend_data<-sim_samp$bendLong
        bend_data$year<- sort(rep(1:nyears,nrow(bends)))
        bend_data$sampled<-c(sampled)        
        bend_data$effort<-ifelse(c(sampled)==1, c(effort2[[gears[k]]]), NA)       
        bend_data$catch<-ifelse(c(sampled)==1, c(catch[[gears[k]]]), NA)
        bend_data$gear<-gears[x] 
        return(as.data.frame(bend_data))
        })
    cpue_long<- do.call("rbind",xxx)
    cpue_long$cpue<-cpue_long$catch/cpue_long$effort
    cpue_long<-unique(merge(cpue_long,sim_catch$flags))
    cpue_long$flags<-ifelse(cpue_long$sampled==1,cpue_long$flags, NA)
    
    ## STILL NEED TO WORK OUT SENSORING THE CAPTURE HISTORIES
    ## SOMETHING TO MULL OVER, MAYBE GO TO REALLY LONG...
     
    ## DO WE WANT PHI_INDX????
        
    ## BUNDLE UP THE GOODIES TO RETURN
    out<-list(
        # SAMPLED
        sampled=sampled, ## BEND SAMPLING MATRIX (BEND, YEAR)
        # CPUE
        catch=catch, ## GEAR, BEND, YEAR
        effort=effort2,## GEAR, BEND, YEAR
        cpue=cpue,## GEAR, BEND, YEAR
        cpue_long=cpue_long,## LONG FORMAT
        # CAPTURE-RECAPTURE
        ch=sim_catch$ch, ## BEND, GEAR, OCC, IND, YEAR
        f_occ=sim_catch$f_occ, ## BEND, GEAR, OCC, YEAR
        q_occ=sim_catch$q_occ) ## BEND, GEAR, OCC, YEAR
    return(out)
  }


## 7. GETTING TREND
get.trnd<-function(sim_dat=NULL,
                   gears=c("GN14", "GN18", "GN41", "GN81", "MF", 
                           "OT16", "TLC1", "TLC2", "TN")) 
{
  
  # GET AVERAGE SEGMENT CPUE BY YEAR
  tmp<- aggregate(cpue~year+b_segment+gear,sim_dat$cpue_long,mean)
  tmp$b_segment<- as.factor(tmp$b_segment)
  tmp$lncpue<- log(tmp$cpue)
  
  # FIT LINEAR MODEL FOR TREND FOR EACH GEAR
  out<-lapply(gears,function(g)
    {
      fit<- lm(lncpue~b_segment+year, tmp, subset=gear==g)
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
	
	 