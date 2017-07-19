RMark::cleanup(ask=FALSE)    

    
sim_ch<-function(inputs,...)
    {library(rowr)
    ## SET UP TIME INTERRVALS
    ## FOR PROGRAM MARK
    ends<-cumsum(inputs$nsec) # last sampling occasion
    occs<- rep(0,sum(inputs$nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing

    # SET UP SURVIVAL
    S<-occs
    S[which(S==1)]<-inputs$phi
    S[which(S==0)]<-1
    
    # SET UP MOVEMENT
    ## USES GAMMA DOUBLE PRIME PROBABILITY OF BEING IN OUTSIDE STUDY AREA
    ## probability of temporarily emigrating from the observable sample during
    ## an interval is the same as the probability of staying away
    GammaDblPrime<-occs    
    GammaDblPrime[which(GammaDblPrime==1)]<-inputs$gam_d_prime
    GammaDblPrime[which(GammaDblPrime==0)]<-unlist(lapply(1:inputs$nprim,
        function(x) {rep(inputs$gam_d_prime2[x],inputs$nsec[x]-1)}))

    # SET UP RECRUITMENT
    f<- occs
    f[which(f==1)]<-inputs$f
    f[which(f==0)]<-0
    
    
    
    
    
    
    ## MAKE THE SUPER POPULATION AT T=1
    Z<- matrix(0,inputs$n,sum(inputs$nsec))
    Z_inn<-Z
    Z_out<-Z
    Z[,1]<-1   
    Z_inn[,1]<-rbinom(nrow(Z),1, 1-inputs$gam_d_prime)   
    Z_out[,1]<-1-Z_inn[,1]  
    
    ## SIMULATE POPULATION DYNAMICS
    ### SIMULATES PRIMARY AND SECONDARY OCCASIONS...
    for(i in 2:ncol(Z))
        {
        ## SURVIVAL
        Z[,i]<- rbinom(n=nrow(Z),
            size=1,
            prob=S[i-1]*Z[,i-1])
        ## MOVEMENT: RANDOM
        if(GammaDblPrime[i-1]==0)
            {
            Z_inn[,i]<- Z_inn[,i-1]*Z[,i]
            }
        if(GammaDblPrime[i-1]>0)
            {
            Z_inn[,i]<- rbinom(nrow(Z),1,1-GammaDblPrime[i-1])*Z[,i]
            }
        Z_out[,i]<- (1-Z_inn[,i])*Z[,i]      
        
        ## RECRUITMENT
        ## ASSUME RECRUITS SETTLE OUTSIDE OF 
        ## STUDY AREA
        R<-rpois(1,sum(Z[,i])*f[i-1])
        app<- matrix(0,R,ncol(Z))
        app[,i]<-1
        Z<- rbind(Z,app)
        Z_out<- rbind(Z_out,app)
        app[]<-0
        Z_inn<- rbind(Z_inn,app)
        }

        #colSums(Z)
        #colSums(Z_inn)
        #colSums(Z_out)
       #colSums(Z_inn)/colSums(Z)
        #colSums(Z_out)/colSums(Z)

        
    if(3==4)
        {
        ## SIMULATE MOVEMENT: markovian
        ## NOTE...... RANDOM DOES NOT DEPEND ON THE PREVIOUS
        ## LOCATION, I HAVE BEEN IMPLEMENTING MARKOVIAN BECAUSE
        ## GAMMAS DEPEND ON PREVIOUS LOCATION.
        occ<- rep(1:inputs$nprim,inputs$nsec)
        for(i in 1:inputs$nprim)
            {
            if(i==1)
                {
                Z_inn<-as.matrix(c(rep(1,inputs$n_inn),
                    rep(0,inputs$n-inputs$n_inn)),ncol=1)
                Z_out<- 1-Z_inn             
                }
            if(i>1)
                {
                ## MOVEMENT OUT OF STUDY AREA
                innToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_d_prime[i]*Z[,i]*Z_inn[,ncol(Z_inn)-1])
                innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,ncol(Z_inn)-1])
                ## REMAINING OUT OF STUDY AREA
                outToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_prime[i]*Z[,i]*Z_out[,ncol(Z_inn)])
                outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,ncol(Z_inn)])
                Z_inn<-cbind(Z_inn,innToInn+outToInn)
                Z_out<-cbind(Z_out,outToOut+innToOut)
                }
            for(j in 2:inputs$nsec[i])
                {
                ## MOVEMENT OUT OF STUDY AREA
                innToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_d_prime2[i]*Z[,i]*Z_inn[,ncol(Z_inn)])
                innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,ncol(Z_inn)])
                ## REMAINING OUT OF STUDY AREA
                outToOut<- rbinom(n=nrow(Z),
                    size=1,
                    prob=inputs$gam_prime2[i]*Z[,i]*Z_out[,ncol(Z_inn)])
                outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,ncol(Z_inn)])
                
                Z_inn<-cbind(Z_inn,innToInn+outToInn)
                Z_out<-cbind(Z_out,outToOut+innToOut)          
                }
            }
        }   

    ## SIMULATE CAPTURE HISTORIES
    ch<- matrix(0,nrow(Z),ncol(Z))
    p<- rep(inputs$p,inputs$nsec)
    for(i in 1:sum(inputs$nsec))
        {
        ch[,i]<-rbinom(nrow(Z),1,p[i]*Z_inn[,i])
        }
       
    ## SUBSET OUT FISH THAT ARE NEVER CAPTURED
    ch<- ch[which(apply(ch,1,sum)!=0),]
    ## NEED THIS FOR RMARK
    # prep data for processing
    ch<- data.frame(ch=apply(ch,1,paste0,collapse=""),
        freq=1,stringsAsFactors=FALSE)
    out<-(list(ch=ch,
        occs=occs,
        nprim=inputs$nprim,
        nsec=inputs$nsec,
        Z=Z,
        Z_inn=Z_inn,
        Z_out=Z_out))
    return(out)
    }
    

## ESIMATE ABUNDANCE AND SURVIVAL WITH rrd    
est_rd<-function(inputs,...)
    {
    library(RMark)
    rd<-process.data(data=inputs$ch, 
        model="Robust", 
        time.intervals=inputs$occs)
    S=list(formula=~1)# SURVIVAL
    # SHARE = TRUE TO SET C = P
    p=list(formula=~1,share=TRUE)# CAPTURE PROBABILITY
    f0<- list(formula=~time) # NUMBER NOT ENCOUNTERED
    GammaDoublePrime=list(formula=~1,share=TRUE)
    GammaPrime=list(formula=~1)
    fit<-mark(data = rd, 
        model = "Robust", 
        time.intervals=time.intervals,
        model.parameters=list(
            S=S,
            GammaDoublePrime=GammaDoublePrime,
            # GammaPrime=GammaPrime, # not needed when share=TRUE
            p=p),
        threads=2,
        brief=TRUE)
    outp<-list(
        abundance=data.frame(
            derived=fit$results$derived$`N Population Size`,
            trueN=colSums(inputs$Z_inn)),
        parameters=data.frame(
            parameter=rownames(summary(fit)$beta),
            ests=plogis(summary(fit)$beta$estimate)))
    return(outp)
    fit<-NA; cleanup(ask=FALSE)    
    }


sim_rd_ch<-function(n=100, 
    n_unobservable=200, 
    nprim=5,
    nsec=3,
    phi=0.8,
    gamma_prime=0.3,
    gamma_dblprime=0.3,
    p=0.3,  
    ...)
    {
    
    super_n<- n+n_unobservable # SUPER POPULATION (ONSITE + OFFSITE)
    Z<- matrix(0,super_n,nprim)
    Z_observable<- matrix(0,super_n,nprim)
    Z_unobservable<- matrix(0,super_n,nprim)
    Z[1:super_n,1]<-1
    Z_observable[1:n,1]<-1
    Z_unobservable[(n+1):(n+n_unobservable),1]<-1


    # SURVIVAL,, EMIGRATION, AND MIGRATION
    for(i in 1:super_n) # FOR EACH INDIVIDUAL IN THE SUPER POPULATION
        {
        for(j in 2:(nprim)) # FOR PRIMARY OCCASIONS 2:NPRIM
            {
            # DOES A FISH SURVIVE, IF IT WAS PREVIOUSLY ALIVE?
            Z[i,j]<- rbinom(1,1,phi[j-1])*Z[i,j-1]

            prs<-c(
                # OBSERVABLE[t-1] --> UNOBSERVABLE[t]
                (1-Z_unobservable[i,j-1])*(gamma_dblprime[j-1]),
                
                # OBSERVABLE[t-1] --> OBSERVABLE[t]
                (1-Z_unobservable[i,j-1])*(1-gamma_dblprime[j-1]),
                
                # UNOBSERVABLE[t-1]--> UNOBSERVABLE[t]
                Z_unobservable[i,j-1]*gamma_prime[j-1],
                
                # UNOBSERVABLE[t-1]--> 	OBSERVABLE[t]
                Z_unobservable[i,j-1]*(1-gamma_prime[j-1]))

            state<-sample(1:4,1,prs,replace=FALSE)
            if(state==1 & Z[i,j]==1){Z_unobservable[i,j]<- 1}
            if(state==2 & Z[i,j]==1){Z_observable[i,j]<-1}
            if(state==3 & Z[i,j]==1){Z_unobservable[i,j]<-1}
            if(state==4 & Z[i,j]==1){Z_observable[i,j]<-1}
            }
        }
        
        
        
        
    # SIMULATE CAPTURE HISTORY
    ## MORE THAN 1 CAPTURE OCCASIONS
    ch<- matrix(0,nrow(Z),sum(nsec))
    avail2Capture<-ch
    ## LINK SECONDARY OCCASION TO PRIMARY OCCASION
    primsec<- matrix(cbind(rep(1:nprim,nsec),
        c(1:sum(nsec))), ncol=2)
    ## CAPTURE HISTORY CONDITIONAL ON BEING PRESENT (I.E., NOT EMIGRATED)
    for(i in 1:super_n)
        {
        for(j in 1:sum(nsec))
            {
            primocc<- primsec[j,1]
            avail2Capture[i,j]<-Z_observable[i,primsec[j,1]]
            ch[i,j]<- rbinom(1,1,p[primocc]*avail2Capture[i,j])
            }
        }
        
    ## SUBSET OUT FISH THAT ARE NEVER CAPTURED
    ch<- ch[which(apply(ch,1,sum)!=0),]

    ## NEED THIS FOR RMARK
    ch2<- data.frame(ch=apply(ch,1,paste0,collapse=""),freq=1,stringsAsFactors=FALSE)# prep data for processing

    # NEED TO MAKE A MATRIX OF THAT INDICATES THE FIRST OCCASION
    ends<-cumsum(nsec) # last sampling occasion
    occs<- rep(0,sum(nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing 


    return(list(ch=ch2,occs=occs))        
    }
