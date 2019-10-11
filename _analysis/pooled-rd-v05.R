## Pooled robust design
### V01-pooled fixed sites
### V02-random sites each year: done terrible performance
### V03-added movement matrix still no movement-terrible.
### V04-some movement, regardless of movement,
###     if you don't sample 100% of reaches the 
###     the estimator is bunk. 
### V05-adding heterogeneous p
### V06-multiple segments 7,8,9,10,13,14 - no movement among years


library(RMark)
expit<-function(x){log(x/(1-x))}

## number of bends in each segment
ini<- data.frame(
    nbends=c(34,61,81,39,45,56),
    seg=c(7,8,9,10,13,14))
ini$bend_id<-1:nrow(ini)

## number of bends to sample
## as a percentage
ini$nsites<- round(ini$nbends*0.2,0)

## expand dataset
ini<-as.data.frame(lapply(ini,
    function(x) rep(x,ini$nbends)))
## assign some lengths
ini$bend_km<- runif(nrow(ini),1,12)
## survival
phi<- 0.8
## years
nprim<- 10
## number of secondary occasions
nsec<- 4
## fish density
dens<- 5 # 10 fish per km
## assign abundance to each bend
ini$N<-rpois(nrow(ini),dens*ini$bend_km)
inds<-as.data.frame(lapply(ini,
    function(x) rep(x,ini$N)))

    
    
    
## HETEROGENOUS CAPTURE PROBABILITIES
p<- matrix(plogis(expit(0.1)+rnorm(nrow(ini)*nprim,0,0.3)),nsites,nprim)


## CRUDE MOVEMENT
#mov<-matrix(0,nbends,nbends)
#diag(mov)<-1
#mov[upper.tri(mov)]<-1 #0 NO MOVEMENT, 1 UNIFORM
#mov[lower.tri(mov)]<-1 #0 NO MOVEMENT, 1 UNIFORM

## ASSIGN A BEND TO EACH FISH
loc<-matrix(0,sum(N),nprim) 
loc[,1]<-rep(1:sum(nbends),N)

## LIVE/DEAD MATRIX
Z<- matrix(0,nrow=nrow(ini),ncol=nprim)
Z[,1]<-1
for(i in 2:nprim)
    {
    for(k in 1:sum(N))
        {
        #loc[k,i]<- sample(c(1:nbends),1,
        #    c(mov[loc[k,i-1],]),
        #    replace=FALSE)
        Z[k,i]<-rbinom(1,1,Z[k,i-1]*phi)
        }
    }
 
## ARRAY TO HOLD CATCH 
catch<- matrix(0,c(nrow(Z),nsec,nprim))
sample_bends<-matrix(0,nsites,nprim)
for(i in 1:nprim)
    {
    sample_bends[,i]<-sample(c(1:nbends),nsites,replace=FALSE)
    for(k in 1:length(sample_bends[,i]))
        {        
        indx<- which(loc[,i]  %in% sample_bends[k,i]) 
        for(j in 1:nsec)
            {
            catch[indx,j,i]<-rbinom(length(indx),1,p[k,i]*Z[indx,i]) 
            }
        }
    }

## process capture histories FOR MARK
ch<- catch[,,1]
for(i in 2:nprim)
    {
    ch<- cbind(ch,catch[,,i])
    }
 
ch<- ch[which(rowSums(ch)>0),]  
ch<-data.frame(ch=apply(ch,1,paste0,collapse=""),
    freq=1,
    stringsAsFactors=FALSE)

## proccess vector of occasion
## for rmark
occ<- rep(0,nsec*nprim)
occ[cumsum(rep(4,nprim))]<-1
occ<- occ[-length(occ)] # funky formatting for rmark
    
rd<-process.data(data=ch, 
    model="Robust", 
    time.intervals=occ)
     
Sform=list(formula=~1)# SURVIVAL
# SHARE = TRUE TO SET C = P
# CAPTURE PROBABILITY VARIES OVER TIME
pform=list(formula=~time,share=TRUE)
## PROBABILITY OF IN SAMPLED BENDS OR NOT
## PROBABLY SOME INTERPRETATION NEEDS TO BE
## DONE HERE SINCE WE ARE AGGREGATING
GammaDoublePrimeform=list(formula=~time,share=TRUE)
GammaPrimeform=list(formula=~time)

# FIT MODEL USING MARK
fit<-mark(data = rd, 
    model = "Robust", 
    time.intervals=time.intervals,
    model.parameters=list(
        S=Sform,
        GammaDoublePrime=GammaDoublePrimeform,
        # GammaPrime=GammaPrime, # not needed when share=TRUE
        p=pform),
    threads=2,
    brief=TRUE)    
summary(fit)
fit$results$derived$`N Population Size`
## TRUE ABUNDANCE-SEGMENT LEVEL
NN<-colSums(Z)   
## GET LENGTHS OF SAMPLED BENDS
lens<-colSums(matrix(bend_km[sample_bends],
    nsites,
    nprim))
## ESTIMATE THE DENSITY OF FISH PER KM
dens<- fit$results$derived$`N Population Size`$estimate/lens     
## ESTIMATE ABUNDANCE AND COMPARE TO TRY
plot(dens*sum(bend_km)~colSums(Z));abline(0,1)
    
cleanup(ask=FALSE)    
    
