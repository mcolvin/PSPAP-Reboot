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
  
  # RETURN SAMPLES BENDS (MATRIX FORM)
  return(sampled)
}


## 5. FUNCTION TO DETERMINE EFFORT & CATCHABILITY OF EACH DEPLOYMENT AND 
##    RESULTING CAPTURE HISTORIES OF FISH IN SAMPLED BENDS
catch_data<-function(sim_pop=NULL,
                     gears=c("GN14", "GN18", "GN41", "GN81",
                             "MF", "OT16", "TLC1", "TLC2", "TN"),
                     catchability=NULL,
                     B0_sd=NULL,
                     deployments=rep(8,9),
                     effort=NULL,
                     occasions=3)
{
  # USE SIM_POP TO DEFINE VARIABLES
  tmp<-sim_pop$bendMeta
  b_abund<-sim_pop$out
  Z_abund<-sim_pop$Z
  sampled<-bend_samples(sim_pop=sim_pop)
  
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

  ch<-lapply(1:ncol(sampled),function(yr)
  { 
    ## PULL OUT SAMPLED BENDS
    samp_indx<-which(sampled[,yr]==1)
    
    bend_ch<-lapply(samp_indx, function(x)
    {
      if(nrow(Z_abund[[x]])==0) occ_ch<-NULL
      if(nrow(Z_abund[[x]])>0)
      {
        ## CREATE TABLE OF SAMPLED BENDS
        tmp1<-tmp[x,names(tmp) %in% c("b_segment","bend_num")]
        ## ADD YEAR SAMPLED
        tmp1$year<-yr
        ## EXPAND FOR INDIVIDUALS AND GEARS
        tmp1<-tmp1[rep(1, nrow(Z_abund[[x]])*length(gears)),]
        tmp1$fish_id<-rep(1:nrow(Z_abund[[x]]),length(gears))
        tmp1$fish_id<-paste0(x,".",tmp1$fish_id)
        tmp1$gear<-rep(gears, each=nrow(Z_abund[[x]]))
        ## PULL OUT INDIVIDUAL DATA SURVIVAL
        ZZ<-matrix(rep(Z_abund[[x]][,yr],length(gears)),
                 nrow=length(Z_abund[[x]][,yr]),ncol=length(gears))
        ## FIND CH FOR EACH OCCASION, GEAR, AND INDIVIDUAL
        occ_ch<-lapply(1:occasions,function(occ,out=tmp1)
          { 
            ### EXPAND DATAFRAME TO INCLUDE OCCASION
            out$occasion<-occ
            ### FIND OCCASION LEVEL CP FOR GIVEN BEND AND YEAR
            dat<-subset(b_samp, year==yr & b_segment==tmp$b_segment[x] 
                        & bend_num==tmp$bend_num[x] & occasion==occ)
            P<-aggregate(p~gear,dat,sum)
            ### FLAG HIGH CPs
            P$flag<-ifelse(P$p<0.4,0,ifelse(P$p<=1,1,2))
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




## 6. GETTING TREND
get.trnd<-function(sim_dat=NULL,
                   gears=c("GN14", "GN18", "GN41", "GN81", "MF", 
                           "OT16", "TLC1", "TLC2", "TN")) 
{
  # USE CATCH +1 TO AVOID CPUE=0
  sim_dat$cpue_long$catch1<-ifelse(sim_dat$cpue_long$effort==0,0,sim_dat$cpue$catch+1)
  sim_dat$cpue_long$cpue1<-sim_dat$cpue_long$catch1/sim_dat$cpue$effort
  
  # GET AVERAGE SEGMENT CPUE BY YEAR
  #dat_occ1<-subset(sim_dat$cpue_long,occasion==1)
  #tmp<- aggregate(cpue1~year+b_segment+gear,dat_occ1,mean)
  tmp<- aggregate(cpue1~year+b_segment+gear,sim_dat$cpue_long,mean)
  tmp$b_segment<- as.factor(tmp$b_segment)
  tmp$lncpue1<- log(tmp$cpue1)
  
  # FIT LINEAR MODEL FOR TREND FOR EACH GEAR
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
	
	 