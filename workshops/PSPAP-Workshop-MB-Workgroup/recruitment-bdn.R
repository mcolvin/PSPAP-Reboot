# upper
nbends<- c(1,40,91,24)



# lower
nbends<- c(34,61,81,39,22,45,56)
NN<- 1000000
recruitmentLevel<- round(runif(NN,1,1000),0)
prop<- sample(c(0.05,0.10,0.15,0.20,0.25),NN,replace=TRUE)
p<- runif(NN, 0, 0.1)
nocc<-sample(c(2:50),NN,replace=TRUE)
bend<- rep(1,sum(nbends))
bendAbund<-lapply(1:NN,function(x)
    {
    abund<-rmultinom(1,recruitmentLevel[x],
        rep(1/sum(nbends),sum(nbends)))
    abund[abund>1]<-1
    detected<- matrix(0,sum(nbends),nocc[x])
    for(i in 1:nocc[x])
        {
        detected[,i]<- rbinom(sum(nbends),1,p[x]*abund)
        }
    sampleSize<- round(sum(nbends)*prop[x],0)
    indx<- sample(1:sum(nbends),
        sampleSize,replace=FALSE)
    samp<-rowSums(detected[indx,])
    reliable<- length(which(samp>2))/sampleSize
    
    
    
    return(
        data.frame(detected=max(detected[indx,]),performance=reliable))
    })
yy<-data.table::rbindlist(bendAbund) 
out<-data.frame(prop=prop, 
    nocc=nocc, 
    p=p,
    recruitmentLevel=recruitmentLevel,
    detected=yy$detected,
    reliability=yy$performance)
out$sampleSize<- round(sum(nbends)*out$prop,0)
write.csv(out,"C:\\Users\\mcolvin\\Desktop\\recruit-out.csv")



out<-read.csv("C:\\Users\\mcolvin\\Desktop\\recruit-out.csv")
out<-read.csv("recruit-out.csv")



## BASIN
basin<-c("Upper","Lower")

## bin p    
brks<- c(0,0.02,0.04,0.06,0.08,0.1)
labs<- paste(brks[-length(brks)],brks[-1],sep='-')    
out$p_bin<-cut(out$p,brks,labs,include.lowest = TRUE)
   
## bin nocc
brks<- c(2, 3, 4, 5, 10, 20, 30, 40, 50)
labs<- paste(brks[-length(brks)],brks[-1],sep='-')    
out$nocc_bin<-cut(out$nocc,brks,labs,include.lowest = TRUE)
 

## bin recruitmentLevel
brks<- c(1, 30, 60, 100, 500, 1000)
labs<- paste(brks[-length(brks)],brks[-1],sep='-')    
out$recruitmentLevel_bin<-cut(out$recruitmentLevel,brks,labs,include.lowest = TRUE) 

out$tmp<-1

cpt<-reshape2::dcast(out,recruitmentLevel_bin+p_bin+prop+nocc_bin~detected,
    value.var="tmp",sum,drop=FALSE, fill=0)
## CONVERT COUNTS TO PROBABILITIES    
cpt[,c(5,6)]<-  cpt[,c(5,6)]/rowSums(cpt[,c(5,6)])
cpt[,5]<-ifelse(is.na(cpt[,5]),0.5,cpt[,5]) # fill missing values
cpt[,6]<-ifelse(is.na(cpt[,6]),0.5,cpt[,6]) # fill missing values
cpt<-rbind(cpt,cpt)
write.csv(cpt,"recrut-cpt.csv")


## bin recruitmentLevel
brks<- c(0,0.5,0.8,1)
labs<- paste(brks[-length(brks)],brks[-1],sep='-')    
out$reliability_bin<-cut(out$reliability,brks,labs,include.lowest = TRUE) 

cpt<-reshape2::dcast(out,recruitmentLevel_bin+p_bin+prop+nocc_bin~reliability_bin,
    value.var="tmp",sum,drop=FALSE, fill=0)
## CONVERT COUNTS TO PROBABILITIES    
cpt[,c(5,6,7)]<-  cpt[,c(5,6,7)]/rowSums(cpt[,c(5,6,7)])
cpt[,5]<-ifelse(is.na(cpt[,5]),0.5,cpt[,5]) # fill missing values
cpt[,6]<-ifelse(is.na(cpt[,6]),0.5,cpt[,6]) # fill missing values
cpt[,7]<-ifelse(is.na(cpt[,7]),0.5,cpt[,7]) # fill missing values
cpt<-rbind(cpt,cpt)
write.csv(cpt,"recrut-cpt-performance.csv")



out[is.na(out$recruitmentLevel_bin),]

