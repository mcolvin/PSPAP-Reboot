RMark::cleanup(ask=FALSE)    

    
sim_ch<-function(inputs,...)
    {library(rowr)
    ## MAKE THE SUPER POPULATION
    Z<- matrix(0,inputs$n,inputs$nprim)
    Z_inn<-Z
    Z_out<-Z

    Z[,1]<-1
    Z_inn[1:inputs$n_inn,1]<-1
    Z_out[,1]<- 1*(1-Z_inn[,1])

    # SIMULATE CAPTURE HISTORY
    ## MORE THAN 1 CAPTURE OCCASIONS
    ch<- matrix(0,nrow(Z),sum(inputs$nsec))

    ## LINK SECONDARY OCCASION TO PRIMARY OCCASION
    #primsec<- matrix(
    #    cbind(rep(1:inputs$nprim,inputs$nsec),
    #    c(1:sum(inputs$nsec))), ncol=2)

     
    ## CAPTURE HISTORY FOR T=1    
    ## UPDATE WHO IS IN AND OUT
    ## AFTER DAY TO DAY MOVEMENT
    i<-1       
    ch<-Z_inn2<-Z_out2<- c()
    for(k in 1:inputs$nsec[i])
        {
        ch<- cbind(ch,rbinom(nrow(Z_inn),1,inputs$p[i]*Z_inn[,i]*Z[,i]))    
        ## MOVEMENT OUT OF STUDY AREA
        innToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=inputs$gam_d_prime2[i]*Z[,i]*Z_inn[,i])
        innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i])
        ## REMAINING OUT OF STUDY AREA
        outToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=inputs$gam_prime2[i]*Z[,i]*Z_out[,i])
        outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i])
        Z_inn[,i]<-0 
        Z_inn[which(innToInn==1 | outToInn==1),i]<- 1
        Z_out[,i]<-0 
        Z_out[which(outToOut==1 | innToOut==1),i]<- 1
        Z_inn2<- cbind(Z_inn2,Z_inn[,i])
        Z_out2<- cbind(Z_out2,Z_out[,i])
        }    

     

    ## SIMULATE POPULATION DYNAMICS
    for(i in 2:inputs$nprim)
        {
        ## SURVIVAL
        Z[,i]<- rbinom(n=nrow(Z),
            size=1,
            prob=inputs$phi[i-1]*Z[,i-1])
        
        ## MOVEMENT OUT OF STUDY AREA
        innToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=inputs$gam_d_prime[i-1]*Z[,i]*Z_inn[,i-1])
        innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i-1])
        
        ## REMAINING OUT OF STUDY AREA
        outToOut<- rbinom(n=nrow(Z),
            size=1,
            prob=inputs$gam_prime[i-1]*Z[,i]*Z_out[,i-1])
        outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i-1])
        Z_inn[,i]<-0 
        Z_inn[which(innToInn==1 | outToInn==1),i]<- 1
        Z_out[,i]<-0 
        Z_out[which(outToOut==1 | innToOut==1),i]<- 1

       
        ## SIMULATE DAY TO DAY MOVEMENT AND
        ## CAPTURE HISTORY    
        for(k in 1:inputs$nsec[i])
            {
            ch<- cbind.fill(ch,
                rbinom(nrow(Z_inn),1,inputs$p[i]*Z_inn[,i]*Z[,i]))        
            ## MOVEMENT OUT OF STUDY AREA
            innToOut<- rbinom(n=nrow(Z),
                size=1,
                prob=inputs$gam_d_prime2[i]*Z[,i]*Z_inn[,i])
            innToInn<- (1-innToOut)*(Z[,i]*Z_inn[,i])
            ## REMAINING OUT OF STUDY AREA
            outToOut<- rbinom(n=nrow(Z),
                size=1,
                prob=inputs$gam_prime2[i]*Z[,i]*Z_out[,i])
            outToInn<-  (1-outToOut)*(Z[,i]*Z_out[,i])
            Z_inn[,i]<-0 
            Z_inn[which(innToInn==1 | outToInn==1),i]<- 1
            Z_out[,i]<-0 
            Z_out[which(outToOut==1 | innToOut==1),i]<- 1
            Z_inn2<- cbind.fill(Z_inn2,Z_inn[,i])
            Z_out2<- cbind.fill(Z_out2,Z_out[,i])
            }  
        
        ## RECRUITMENT
        ## ASSUME RECRUITS SETTLE OUTSIDE OF 
        ## STUDY AREA
        R<-rpois(1,sum(Z[,i])*inputs$f[i-1])
        app<- matrix(0,R,inputs$nprim)
        app[,i]<-1
        Z<- rbind(Z,app)
        Z_out<- rbind(Z_out,app)
        app[]<-0
        Z_inn<- rbind(Z_inn,app)
        }


        
        

        
    ## SUBSET OUT FISH THAT ARE NEVER CAPTURED
    ch<- ch[which(apply(ch,1,sum)!=0),]
    ## NEED THIS FOR RMARK
    # prep data for processing
    ch<- data.frame(ch=apply(ch,1,paste0,collapse=""),
        freq=1,stringsAsFactors=FALSE)
        
    ## SET UP TIME INTERRVALS
    ## FOR PROGRAM MARK
    ends<-cumsum(inputs$nsec) # last sampling occasion
    occs<- rep(0,sum(inputs$nsec))
    occs[ends]<-1# last occasion in primary
    occs<- occs[-length(occs)]# drop last 1 for processing 


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



