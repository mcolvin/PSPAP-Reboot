# FUNCTIONS IN THIS SCRIPT
##  1. dfitfunLB
##  2. dfitfunuB
# RELATIVE ABUNDANCE (CPUE)
##  3. reference_population
##  4. catch_counts
##  5. bend_samples
##  6. samp_dat
##  7. get.trnd
# CAPTURE RECAPTURE FUNCTIONS
##  8. sim_ch
##  9. estimate
## 10. plot_metrics


# FIT DISTRIBUTIONS TO DATA 
dfitfun<-function(x,dat,basin,gears)
{
  datLBgear<-subset(dat, gear==LBgears[x])
  dfit<-fitdistr(datLBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}


 
dfitfunLB<-function(x)
{
  datLBgear<-subset(datLB, gear==LBgears[x])
  dfit<-fitdistr(datLBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}

dfitfunUB<-function(x)
{
  datUBgear<-subset(datUB, gear==UBgears[x])
  dfit<-fitdistr(datUBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}

# FUNCTION TO DISTRIBUTE FISH AMONG SEGEMENTS
# AND THEN BENDS
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
# FUNCTION TO DETERMINE CATCH COUNTS IN A BEND FOR ALL STANDARD, COMMON GEARS
catch_counts<-function(segs=c(1,2,3,4,7,8,9,10,13,14),
                        bends=NULL,
                        abund=NULL,
                        gears=c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN"),
                        catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.0002, 0.00004, 0.00004, 0.0002),
                        deployments=rep(8,9),
                        effort=NULL,
                        occasions=3)
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

  # BEND-SPECIFIC CATCHABILITY BY GEAR (0<q<1/f)
    ## BEND BY YEAR BY GEAR ARRAY
    ## ASSUMES GEAR CATCHABILITY DOES NOT VARY AMONG BASINS
    bend_q_matrix<- matrix(catchability,ncol=length(catchability),
        nrow=length(abund),
        byrow=TRUE)
        
    ## BEND AND GEAR SPECIFIC NUMBER OF DEPLOYMENTS
    d<-matrix(deployments,ncol=length(deployments),
        nrow=length(abund),
        byrow=TRUE)
    

    
    
    ## DETERMINE THE AMOUNT OF EFFORT
    ## FOR EACH BEND, YEAR, REPLICATE AND GEAR
    f_P<- lapply(1:length(abund),function(x)
        {
        bend_f<-list()## TO HOLD OUTPUT OF EFFORTS
        bend_P<-list()## TO HOLD OUTPUT OF CAPTURE PROBABILITIES
            
        ## LOOP OVER OVERS AND GENERATE FOR EACH BEND
        ## (x) THE AMOUNT OF EFFORT GIVEN THE NUMBER OF 
        ## OCCASIONS AND THE OCCASION AND BEND SPECIFIC 
        ## CAPTURE PROBABILITY ACCOUNTING FOR NUMBER OF DEPLOYMENTS
        for(k in 1:length(gears))
            {
            indx<- which(effort$gear==gears[k] & 
                effort$rpma==bends$rpma[x])
            ## ERROR HANDLING FOR GEARS THAT ARE NOT USED 
            ## RPMAS, THEREFORE NO EFFORT AND f=0
            f_reps<-list()
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
                        rgamma(n=d[x]*nyears,
                            shape=effort$gamma_shape[indx], 
                            rate=effort$gamma_rate[indx]),
                        nrow=d[x],
                        ncol=nyears)
                    }
                }# end occ
            bend_f[[gears[k]]]<-f_reps
            bend_P[[gears[k]]]<-lapply(f_reps, function(yy)
                {
                colSums(yy)*bend_q_matrix[x,k]          
                })
            } # end k (gears)
        return(list(f=bend_f, P=bend_P))
        })
    
   
    ## CAPTURE HISTORY FOR EACH INVIDUAL, BEND,
    ## GEAR AND OCCASSION 
    ch<- lapply(1:length(abund),function(x)
        {
        bend_ch<-list()## TO HOLD OUTPUT OF CAPTURE HISTORIES
        bend_N<-list()## TO HOLD OUTPUT OF CAPTURE HISTORIES
        x<-1
        ZZ<- abund[[x]]
        ## LOOP OVER OVERS AND GENERATE FOR EACH BEND
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
                       prob=c(ZZ)*c(capture_probability)),
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
        outt<-lapply(1:length(abund),function(y)
            {
            ch[[y]][['N']] [[gears[ii]]][[1]]
            })
        bend_catch[[gears[ii]]]<-do.call("rbind",outt)
        outt<-lapply(1:length(abund),function(y)
            {
            colSums(f_P[[y]][['f']] [[gears[ii]]][[1]])
            })
        bend_effort[[gears[ii]]]<-do.call("rbind",outt)
        }
    return(list(catch=bend_catch,
        f=bend_effort, 
        ch=lapply(ch, function(x) x$ch),
        f_occ = lapply(f_P, function(x) x$f))) 
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
  bends_in_segs$flag<-ifelse(bends_in_segs$one_fourth>bends_in_segs$samp_num, 
    "*", " ")
  
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
  ## EXPAND TEMP
  out<- tmp[rep(1:nrow(tmp),nyears),]
  names(out)<-toupper(names(out))
  out$SAMPLED<- c(sampled)
  out$B_ABUNDANCE<- unlist(abund)
  out$YEAR<- sort(rep(c(1:nyears),nrow(tmp)))

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
	
	 