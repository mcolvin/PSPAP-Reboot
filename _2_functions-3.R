

#RELATIVE ABUNDANCE (CPUE)
## 3. FUNCTION TO DISTRIBUTE FISH AMONG SEGEMENTS
## AND THEN BENDS
reference_population<- function(segs=c(1,2,3,4,7,8,9,10,13,14),
                                bends=NULL,
                                fish_density=1,
                                nyears=10,
                                phi=0.95)
{
  # this function allocates fish to bends within a segment
  # probabilistically given bend weights and determines the survival
  # of fish within bend over nyears given survival probabilities
  
  # inputs
  ## segment: segment [1,2,3,4,7,8,9,10,13,14]
  ## fish_density: density of fish within segment; fish/rkm
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
  ##    with bend abundance and segment index columns added
  ##  $Z: a list of 472 matrices; each matrix, Z[[i]], gives individual 
  ##    fish status (0=Dead, 1=Alive) where each row represents a fish
  ##    living in bend i and each column represents a year  
  
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
  tmp<- tmp[order(tmp$b_segment, tmp$bend_num),]## CRITICAL
  bends_in_segs<-aggregate(bend_num~b_segment,tmp,length) 
  bends_in_segs$phi_indx<-1:nrow(bends_in_segs)
  tmp<-merge(tmp, bends_in_segs[,-2],by="b_segment",all.x=TRUE)
  
  # INITIAL ABUNDANCES
  ## PULL NUMBER FROM A POISSON AFTER ADJUSTING
  ## DENSITY FOR BEND SIZE
  tmp$N_ini<-rpois(nrow(tmp),
                   lambda=fish_density*tmp$length.rkm)
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





## 4. FUNCTION TO DETERMINE WHICH BENDS WITHIN A SEGMENT 
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
      p_flags<-do.call("rbind", p_flags)
      bend_f[[gears[k]]]<-f_reps
      bend_q[[gears[k]]]<-q_reps
      bend_P[[gears[k]]]<-p_reps
      bend_P_flags[[gears[k]]]<-p_flags
    } # end k (gears)
    bend_P_flags<-colSums(do.call("rbind",bend_P_flags))
    return(list(f=bend_f, q=bend_q, P=bend_P, P_flag=bend_P_flags))
  })
  P_flag<-matrix(0,nrow=length(Z_abund), ncol=nyears)
  for(i in 1:length(Z_abund))
  {
    P_flag[i,]<-f_P[[i]]$P_flag
  }
  warning<-sum(P_flag)
  
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
              highCP_count=warning)) 
  }
