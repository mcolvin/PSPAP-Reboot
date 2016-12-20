
#' SET UP SIMULATION COMBINATIONS
ntrack<-c(2,5,10,20,30)
N<- round(seq(25,5000,length.out=10))
nocc<-c(2:5)
p<-seq(0.05,0.5, 0.075)
combos<- expand.grid(N=N,nocc=nocc,p=p)
nreps<- c(1:100)
#out<-list()
#' SIMULATION TO GET AT SEs AND ESTIMATES
inputs<- expand.grid(N=N,nocc=nocc,p=p,reps=nreps,ntrack=ntrack)
inputs$id<- paste(inputs$N,inputs$p,inputs$reps,inputs$ntrack,inputs$nocc, sep='-')
#' LOAD PREVIOUSLY RUN SIMULATION RESULTS	
out<- readRDS("./output/simulation-results.RDS")
done_ids<- paste(out$N,out$p,out$reps,out$ntrack,out$nocc,sep='-')

#' CHECK TO SEE IF ANY SIMULATIONS STILL NEED TO BE DONE
done<-unique(done_ids)
inputs<- subset(inputs,!(id %in% done))


out[out==-99]<-NA

#' SET UP DATAFRAME FOR 

