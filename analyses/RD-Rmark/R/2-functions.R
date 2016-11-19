



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