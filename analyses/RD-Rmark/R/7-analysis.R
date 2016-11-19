

nprim<- 10
nsec<- floor(runif(nprim,5,10))
phi	<- rep(0.8,nprim-1)

# CAPTURE PROBABILITY
p	<- rep(0.3,nprim)

gamma_prime	<- rep(0.3, nprim-1) # pr(unobservable @ t-1  --> unobservable @ t )
gamma_dblprime<- rep(0.4, nprim-1) # pr(observable @ t-1 --> unobservable  @ t   )

n<- 175 # initial population size
n_unobservable<- 200 # UNOBSERVABLE

out<-sim_rd_ch(n=n, 
    n_unobservable=n_unobservable, 
    nprim=nprim,
    nsec=nsec,
    phi=phi,
    gamma_prime=gamma_prime,
    gamma_dblprime=gamma_dblprime,
    p=p) 


rd<-process.data(data=out$ch, model="Robust", time.intervals=out$occs)
rd_ddl<-make.design.data(rd)

S=list(formula=~1)# SURVIVAL
# SHARE = TRUE TO SET C = P
p=list(formula=~1,share=TRUE)# CAPTURE PROBABILITY
f0<- list(formula=~1) # NUMBER NOT ENCOUNTERED
GammaDoublePrime=list(formula=~1)
GammaPrime=list(formula=~1)
fit<-mark(data = rd, 
	model = "Robust", 
    time.intervals=time.intervals,
	model.parameters=list(
		S=S,
		GammaDoublePrime=GammaDoublePrime,
		GammaPrime=GammaPrime,
		p=p),
	threads=2,
	brief=TRUE)
derived<- fit$results$derived$`N Population Size`
saveRDS(derived,"./output/derived.RDS")

cleanup(ask=FALSE)




