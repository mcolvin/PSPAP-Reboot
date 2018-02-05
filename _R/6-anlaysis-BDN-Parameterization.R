
#######################################################################
#
#  RECRUITMENT LEVEL NODE
#  PARENT(S):
#       1. BASIN
#
#######################################################################
library(plyr)
library(reshape2)
library(xlsx)
makeLabs<-function(x){paste(x[-length(x)],x[-1],sep="-")}

#######################################################################
#
# DECISION NODES 
#
#######################################################################

## DESIGN
design<- c("Stratified Random design","Fixed randomly selected sites")

## PROPORTION OF BENDS SAMPLED IN BASIN
percentBends<-c(0.05,0.1,0.15,0.2,0.25)

## NUMBER OF TRAWLS WITHIN BEND
ntrawlsBrks<-c(2:5,10,20,30,40,50)
ntrawlsLabs<- makeLabs(ntrawlsBrks)

#######################################################################
#
#  NATURE NODES 
#
#######################################################################

## 1. BASIN NODE
basin<-c("Lower","Upper")



## 2. RECRUITMENT LEVEL
recruitmentLevelBrks<-c(1,30,60,100,500,1000,5000)
recruitmentLevelLabs<- makeLabs(recruitmentLevelBrks)
recruitmentLevel<- round(runif(NN,1,1000),0)


## 3. INTERCEPTION LOCATION
intLocation<-c(1,1/3,2/3,0)
intLocationLabs<-c("Anywhere","Lower third","Lower 2/3","Outside of basin")



## 4. DETECTION PROBABILITY
pDetectBrks<-c(0,0.02,0.04,0.06,0.08,1)
pDetectLabs<- makeLabs(pDetectBrks)

## 5. NUMBER OF BENDS SAMPLED
segs<-data.frame(
    basin=c(2,2,2,2,4,4,4,4,4,4),
    segment=c(1,2,3,4,
        7,8,9,10,13,14),
    nbends=c(1,40,91,24,
        34,61,81,39,45,56))
prop_sampled<-c(0.05,0.1,0.15,0.2,0.25)

out<-matrix(0,10,length(prop_sampled))
for(i in 1:length(prop_sampled))
    {
    out[,i]<- round(segs$nbends*prop_sampled[i],0)
    }

colSums(out[which(segs$basin==2),])    
colSums(out[which(segs$basin==4),])  
# write.csv(...,"_output/bdn_nodes/total_bends_sampled.csv"


#######################################################################
#
#  SIMULATE RESULTS TO PARAMETERIZE prob_detect NODE
#
#######################################################################
combos<-expand.grid(design=design, 
    percentBends=percentBends,
    ntrawlsLabs=ntrawlsLabs, 
    basin=basin,
    intLocation=intLocation, 
    recruitmentLevelLabs=recruitmentLevelLabs,
    pDetectLabs=pDetectLabs)
combos$id<-c(1:nrow(combos))

## SET UP VECTORS OF BENDS AND BEND LENGTHS
### UPPer
upper<- subset(bends,basin=="upper")
upper<- upper[order(upper$b_segment,upper$bend_num),]
upper$cumRkm<-cumsum(upper$length.rkm)
### LOWER
lower<- subset(bends,basin=="lower")
lower<- lower[order(lower$b_segment,lower$bend_num),]
lower$cumRkm<-cumsum(lower$length.rkm)
library(parallel)
cl<- makeCluster(3)
library(pbapply)
clusterExport(cl, c("combos","lower","upper"))
outt<- pblapply(1:nrow(combos),function(x){
    ## SET UP WHERE AGE-1 FISH ARE INTERECEPTED AND HANGING OUT
    if(combos$basin[x]=="Upper")
        {
        pp<- sum(upper$length.rkm)-sum(upper$length.rkm)*combos$intLocation[x]
        pp<-ifelse(upper$cumRkm>=pp,1,0)
        }
    if(combos$basin[x]=="Lower")
        {
        pp<- (sum(lower$length.rkm)+0.1)-sum(lower$length.rkm)*combos$intLocation[x]
        pp<-ifelse(lower$cumRkm>=pp,1,0)
        }
    rr<- as.numeric(unlist(strsplit(as.character(combos$recruitmentLevel[x]),"-")))
    #abund<-rmultinom(1,
     #   round(runif(1,rr[1],rr[2])),
    #    pp/length(pp))
    reps<-150
    abunds<-round(runif(reps,rr[1],rr[2]))
    abund<-sapply(1:length(abunds), function(i) 
        {
        if(sum(pp)>0){ww<-rmultinom(1, abunds[i],  pp/length(pp))}
        if(sum(pp)==0){ww<-rep(0,length(pp))}
        return(ww)
        })
    abund[abund>1]<-1
    

    trawls<- as.numeric(unlist(strsplit(as.character(combos$ntrawlsLabs[x]),"-")))
    trawls<-floor(runif(ncol(abund),trawls[1],trawls[2]-0.001))
    pdetect<- as.numeric(unlist(strsplit(as.character(combos$pDetectLabs[x]),"-")))    
    pdetect<- runif(ncol(abund),pdetect[1],pdetect[2]-0.0001)
    sampleSize<- round(nrow(abund)*combos$percentBends[x],0)

       
    detected<-lapply(1:ncol(abund),function(i)
        {
        indx<- sample(1:nrow(abund),
            sampleSize,replace=FALSE)
        xx<-matrix(rbinom(nrow(abund)*trawls[i],1,pdetect[i]*abund),ncol=trawls[i],byrow=FALSE)
        samp<-rowSums(xx[indx,])
        return(data.frame(detect=max(samp), 
            reliable=length(which(samp>2))/sampleSize))
        })
    #print(x)
    detected<-do.call("rbind",detected)
    return(data.frame(id=x,
        notdetected = reps- colSums(detected)[1], 
        detected= colSums(detected)[1],
        performance=colSums(detected)[2]/reps))
    },cl=cl)

outcomes<- do.call("rbind",outt)
stopCluster(cl = cl)



outcomes<-cbind(combos,outcomes)



as.numeric(unlist(strsplit(pDetectLabs[1],"-")))


NN<- 1000000
pDetect<- round(runif(NN,1,1000),0)



#######################################################################
#
# PROCESS AND OUTCOME SIMULATIONS
#
#######################################################################


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

       
        


## DESIGN
design<-c("Stratified Random design","Fixed randomly selected sites")
## PERCENT OF BENDS SAMPLED
percent_bends<-c(0.05,0.1,0.15,0.2,0.25)
## NUMBER OF TRAWLS WITHIN UNIT
ntrawls<-c(2:50)



nreps<- 500000
upper<- rpois(nreps,exp(runif(nreps,2.3,3.92)))   # ln(MEAN RECRUITS)|RECRUITMENT
lower<- rpois(nreps,exp(runif(nreps,2.3,4.61)))   # ln(MEAN RECRUITS)|RECRUITMENT
tmp<-data.frame(basin=c(rep("upper",nreps),rep("lower",nreps)),
    recruitment=c(upper,lower))
brks<- c(0,seq(1,150,length.out=4))
labs<-c("None","Little", "Moderate","Lots")
tmp$recruitment_b<- cut(tmp$recruitment,brks,labels=labs,include.lowest=TRUE)
tmp$tmp<-1

cpt<- dcast(tmp,basin~recruitment_b,value.var="tmp",sum)
cpt[,-1]<- cpt[,-1]/rowSums(cpt[,-1])

write.xlsx(cpt,
    file="C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/PSPAP-Reboot/_output/bdn-cpt.xlsx",
    sheetName="RECRUITMENT LEVEL",
    col.names=TRUE, 
    row.names=FALSE)
 
 
    


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
    return(data.frame(detected=max(detected[indx,]),performance=reliable))
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

    
