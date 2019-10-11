## Pooled robust design
### V01-pooled fixed sites
nbends<- 81
bend_km<- runif(nbends,1,12)

phi<- 0.8
p<- 0.3

nprim<- 10 # years
nsec<- 4

dens<- 5 # 10 fish per km

N<-rpois(nbends,dens*bend_km)

N

## NO MOVEMENT

loc<- rep(1:nbends,N)

Z<- matrix(0,nrow=sum(N),ncol=nprim)

Z[,1]<-1

for(i in 2:nprim)
    {
    Z[,i]<-rbinom(nrow(Z),1,Z[,i-1]*phi)
    }

catch<- array(0,c(nrow(Z),nsec,nprim))
for(i in 1:nprim)
    {
    for(j in 1:nsec)
        {
        catch[,j,i]<-rbinom(nrow(Z),1,p*Z[,i]) 
        }
    }

sample_bends<- sample(c(1:nbends),10,replace=FALSE)
    
indx<- which(loc  %in% sample_bends)    



library(RMark)

## proccess vector of occasion
## for rmark
occ<- rep(0,nsec*nprim)
occ[cumsum(rep(4,nprim))]<-1
occ<- occ[-length(occ)] # funky formatting for rmark


## process capture histories
ch<- catch[,,1]
for(i in 2:nprim)
    {
    ch<- cbind(ch,catch[,,i])
    }
ch<- ch[indx,]  
ch<- ch[which(rowSums(ch)>0),]  
ch<-data.frame(ch=apply(ch,1,paste0,collapse=""),
    freq=1,stringsAsFactors=FALSE)

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


colSums(Z[loc%in%sample_bends,])    

dim(ch)    
colSums(Z)   
  
dens<- fit$results$derived$`N Population Size`$estimate/sum(bend_km[sample_bends])      
plot(dens*sum(bend_km),colSums(Z)    );abline(0,1)
    
    
    