
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
	
	