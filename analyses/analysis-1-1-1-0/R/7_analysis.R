	


	

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


