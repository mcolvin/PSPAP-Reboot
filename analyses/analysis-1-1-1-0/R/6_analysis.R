	


	

if(nrow(inputs)>0){
	#' RUN SIMULATION OF VARING INPUTS

	for(i in 1:nrow(inputs))
		{
		tmp<-sim_ch(inputs=inputs,index=i)
		tmpp<-estimate(dat=tmp,indx=i)
		out<-rbind(out,tmpp)
		cleanup(ask=FALSE)
		print(round(1/nrow(inputs),4))
		}
	#' SAVE SIMULATION RESULTS
	saveRDS(out,"./simulation-results.RDS")
	}


## SIMULATIONS TO PARAMETERIZE A BDN



	#' RUN SIMULATION OF VARING INPUTS
	out<-data.frame()
	for(kk in 1:300000)
		{
		inputs<- expand.grid(
			N=round(runif(1,5,200),0),
			nocc=round(runif(1,1,5),0),
			p=runif(1,0.005,0.3),
			reps=1,
			ntrack=round(runif(1,1,30),0))
		tmp<-sim_ch(inputs=inputs,index=1)
		if(is.null(tmp)==FALSE)
			{
			tmpp<-estimate(dat=tmp,indx=1)
			out<-rbind(out,tmpp)
			cleanup(ask=FALSE)
			print(round(kk/300000*100,3))		
			}

		}
	#' SAVE SIMULATION RESULTS
	saveRDS(out,"./output/BDN-results.RDS")


