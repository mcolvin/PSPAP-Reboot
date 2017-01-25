
# SET UP SIMULATION COMBINATIONS
ntrack<-c(2,5,10,20,30)
N<- round(seq(25,5000,length.out=10))
nocc<-c(2:5)
p<-seq(0.05,0.5, 0.075)
combos<- expand.grid(N=N,nocc=nocc,p=p)
nreps<- c(1:100)
if(exists('out')==FALSE){out<-list()}

# SIMULATION TO GET AT SEs AND ESTIMATES
inputs<- expand.grid(N=N,nocc=nocc,
    p=p,reps=nreps,ntrack=ntrack)
inputs$id<- paste(inputs$N,inputs$p,
    inputs$reps,inputs$ntrack,inputs$nocc, 
    sep='-')
done_ids<- paste(out$N,out$p,out$reps,out$ntrack,out$nocc,sep='-')

# CHECK TO SEE IF ANY SIMULATIONS STILL
# NEED TO BE DONE
done<-unique(done_ids)
inputs<- subset(inputs,!(id %in% done))
out[out==-99]<-NA


# SET UP GRID FOR PARAMETERIZING A CPT

if(exists('out_bdn')==FALSE)
    {
    out_bdn<-data.frame()
    }# data.frame to hold output

# SET UP GRID TO PARAMETERIZE A BDN
nreps<-300000 # NUMBER OF STOCHASTIC REPS,RETURNS 2 SIMS PER REP (ACOUSTIC, NO ACOUSTIC)
n_to_do<- nreps-nrow(out_bdn)
bdn_inputs<- data.frame(
    N=round(runif(n_to_do,5,200),0),
    nocc=round(runif(n_to_do,1,5),0),
    p=runif(n_to_do,0.005,0.3),
    reps=c((nreps-n_to_do+1):nreps),  # set this up to track in output 
    ntrack=round(runif(n_to_do,1,30),0))

# CHECK TO SEE HOW MANY REPS ARE DONE
    
        
        
        