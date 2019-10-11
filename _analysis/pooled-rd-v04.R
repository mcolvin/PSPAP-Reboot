## Pooled robust design
### V01-pooled fixed sites
### V02-random sites each year: done terrible performance
### V03-added movement matrix still no movement-terrible.
### V04-some movement, regardless of movement,
###     if you don't sample 100% of reaches the 
###     the estimator is bunk. 

library(RMark)
nbends<- 81
bend_km<- runif(nbends,1,12)
phi<- 0.8
p<- 0.3
nprim<- 10 # years
nsec<- 4
dens<- 5 # 10 fish per km
nsites<-10
N<-rpois(nbends,dens*bend_km)
N

## NO MOVEMENT
mov<-matrix(0,nbends,nbends)
diag(mov)<-1
mov[upper.tri(mov)]<-0
mov[lower.tri(mov)]<-0
mov[1,]/sum(mov[1,])

loc<-matrix(0,sum(N),nprim) 
loc[,1]<-rep(1:nbends,N)
Z<- matrix(0,nrow=sum(N),ncol=nprim)
Z[,1]<-1
for(i in 2:nprim)
    {
    for(k in 1:sum(N))
        {
        loc[k,i]<- sample(c(1:nbends),1,
            c(mov[loc[k,i-1],]),
            replace=FALSE)
        Z[k,i]<-rbinom(1,1,Z[k,i-1]*phi)
        }
    }

    
catch<- array(0,c(nrow(Z),nsec,nprim))
sample_bends<-matrix(0,nsites,nprim)
for(i in 1:nprim)
    {
    sample_bends[,i]<-sample(c(1:nbends),nsites,replace=FALSE)
    indx<- which(loc[,i]  %in% sample_bends[,i])    
    for(j in 1:nsec)
        {
        catch[indx,j,i]<-rbinom(length(indx),1,p*Z[indx,i]) 
        }
    }

## process capture histories
ch<- catch[,,1]
for(i in 2:nprim)
    {
    ch<- cbind(ch,catch[,,i])
    }
 
ch<- ch[which(rowSums(ch)>0),]  
ch<-data.frame(ch=apply(ch,1,paste0,collapse=""),
    freq=1,stringsAsFactors=FALSE)

    
## proccess vector of occasion
## for rmark
occ<- rep(0,nsec*nprim)
occ[cumsum(rep(4,nprim))]<-1
occ<- occ[-length(occ)] # funky formatting for rmark
    
rd<-process.data(data=ch, 
    model="Robust", 
    time.intervals=occ)
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
summary(fit)
fit$results$derived$`N Population Size`


   
NN<-colSums(Z)   
lens<-colSums(matrix(bend_km[sample_bends],
    nsites,
    nprim))
dens<- fit$results$derived$`N Population Size`$estimate/lens     
plot(dens*sum(bend_km),colSums(Z));abline(0,1)
    
    
    