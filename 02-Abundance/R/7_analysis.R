	


	

if(nrow(inputs)>0){
	# RUN SIMULATION OF VARING INPUTS
	for(i in 1:nrow(inputs))
		{
		tmp<-sim_ch(inputs=inputs,index=i)
		tmpp<-estimate(dat=tmp,indx=i)
		out<-rbind(out,tmpp)
		cleanup(ask=FALSE)
		print(round(1/nrow(inputs),4))
		}
	# SAVE SIMULATION RESULTS
	saveRDS(out,"./output/simulation-results.RDS")
	}


## SIMULATIONS TO PARAMETERIZE A BDN

### RUN SIMULATION OF VARING INPUTS
if(exists('bdn_inputs')==TRUE)
    {
    for(kk in 1:nrow(bdn_inputs))
        {
        # SIMULATE DATASET
        tmp<-sim_ch(inputs=bdn_inputs,index=kk)
        if(is.null(tmp)==FALSE)
            {
            # ESTIMATE 
            tmpp<-estimate(dat=tmp,
                inputs=bdn_inputs,indx=kk)
            out_bdn<-rbind(out_bdn,tmpp)
            cleanup(ask=FALSE)
            print(round(kk/nrow(bdn_inputs)*100,3))		
            }
        # SAVE SIMULATION RESULTS
        saveRDS(out_bdn,"./output/BDN-results.RDS")
        }
    }



    
    
    # SIMULATION CODE TO ESTIMATE THE NUMBER OF PALLID STURGEON 
# IN MISSOURI RIVER SEGMENTS USING CAPTURE-RECAPTURE

## SIMULATE CAPTURE-RECAPTURE DATA
## ASSUMES M_0 
N_reps<- 50
N_fish<- 5000
N_reaches<- 300
p<-runif(N_reaches)
N_bend<- rmultinom(N_reps,N_fish,p=p/sum(p))
N_occ<- 4 
S_bends<- 10
indx<- matrix(0,nrow=S_bends,ncol=N_reps)

for(i in 1:N_reps)
	{
	indx[,i]<- sample(1:N_reaches, S_bends, replace=FALSE)
	}
	
	
aa<- matrix(runif(S_bends*N_reps, -3.5,-2),S_bends,N_reps)
p_cap<- array(0,c(S_bends,N_occ,N_reps))
for(i in 1:N_reps)
	{
	for(j in 1:S_bends)
		{
		for(k in 1:N_occ)
			{
			p_cap[j,k,i]<- plogis(aa[j,i])
			}
		}
	}	




ch_out<- list()
# capture history of untagged fish
ch<- array(0,c(S_bends,N_occ,N_reps))	
for(i in 1:N_reps)
	{
	ch_rep<- list()
	for(j in 1:S_bends)
		{
		ch<- matrix(0,N_bend[indx[j,i]] ,N_occ)
		for(k in 1:N_occ)
			{
			ch[,k]<- rbinom(N_bend[indx[j,i]],1,p_cap[j,k,i])
			}
		ch<- ch[which(apply(ch,1,sum)>0),] 
		ch_rep[[j]]<-ch
		}
	ch_out[[i]]<- ch_rep
	}	



library(R2jags)
mod<- function()
	{
	for(ind in 1:N_inds)
		{
		for(bend in 1:S_bends)
			{
			z[ind,bend]~dbern(omega[bend]) # LATENT VARIABLE, DATA AUGMENTATION
			for(occ in 1:N_occ)
				{
				logit(p_cap[ind,occ,bend])<- a[bend]
				p_eff[ind,occ,bend]<- z[ind,bend]*p_cap[ind,occ,bend] # CONDITIONAL CAPTURE PROBABILITY
				ch[ind,occ,bend]~dbern(p_eff[ind,occ,bend])			
				}# j
			}#
		} # ind

	# DERIVED PARAMETERS
	for(bend in 1:S_bends)
		{
		Fish_bend[bend]<-sum(z[,bend]) 
		}
	N_hat<- N_bends*(sum(Fish_bend[])/S_bends) # ESTIMATED TOTAL
		
	# PRIORS
	for(bend in 1:S_bends)
		{
		omega[bend]~dunif(0,1)
		a[bend]~dnorm(0,0.37)
		#for(occ in 1:N_occ)
		#	{
		##	a[occ,bend]~dnorm(0,0.37)
		#	}
		}
	}

outp<- data.frame()	
for(kk in 1:N_reps)
	{
	ch<- ch_out[[kk]]
	## INDICES
	S_bends<- length(ch)
	N_occ<- max(unlist(sapply(1:length(ch),function(x){
		if(length(ch[[x]])>N_occ){N<- ncol(ch[[x]])}
		if(length(ch[[x]])==N_occ){N<- 1}
		if(length(ch[[x]])==0){N<- 0}
		return(N)
		})))
	maxFish<- unlist(sapply(1:length(ch),function(x){
		if(length(ch[[x]])>N_occ){N<- nrow(ch[[x]])}
		if(length(ch[[x]])==N_occ){N<- 1}
		if(length(ch[[x]])==0){N<- 0}
		return(N)
		}))
	dat_aug<- ifelse(100-max(maxFish)>50,100,250)
	N_inds<- dat_aug
	ch_inn<- array(0,c(dat_aug,N_occ,S_bends))
	for(i in 1:S_bends)
		{
		indxx<- 1:maxFish[i]
		if(maxFish[i]>0){ch_inn[indxx,,i]<- ch[[i]]	}
		}
		
	dat<- list(S_bends=S_bends,
		N_inds=N_inds,
		N_occ=N_occ,
		ch=ch_inn,
		N_bends=300)

	## vectors of 0s and 1s indicating whether fish is 
	## present or not
	Z<- matrix(0,dat_aug,length(ch))
	Z[1:(max(maxFish)+10),]<- 1

		
	## initial values
	## set for each chain
	inits<- function()
		{
		list(a=runif(length(ch),-4,-1),omega=runif(length(ch)),z=Z)
		}
		
	## WHAT PARAMTERS TO KEEP TRACK OF DURING ESTIMATION
	params<-c("a","N_hat","omega")	

	# THIS WILL ONLY RUN IF YOU HAVE JAGS INSTALLED 
	# AND THE R2jags PACKAGE
	out <- jags(data=dat,
		inits=inits,
		parameters=params,	
		model.file=mod,
		n.chains = 3,	
		n.iter = 15000,	
		n.burnin = 6000, 
		n.thin=2,
		working.directory=getwd())

	out$BUGSoutput$mean$N_hat # ESTIMATED NUMBER OF FISH IN POOL
	xx<-as.data.frame(out$BUGSoutput$summary)
	xx$true<- c(N_fish,unlist(aa[,kk]),NA, N_bend[indx[,kk],kk]/dat_aug )
	xx$r<-kk
	outp<-rbind(outp,xx)
	}
	
