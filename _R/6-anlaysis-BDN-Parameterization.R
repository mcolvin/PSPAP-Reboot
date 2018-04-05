
#######################################################################
#
#  RECRUITMENT DETECTION SUBMODEL 
#
#######################################################################
wd<-getwd()
wd<-unlist(strsplit(wd,"/"))
setwd(paste0(wd[-length(wd)],collapse="\\"))

library(parallel)
library(pbapply)
library(plyr)
library(reshape2)
library(xlsx)

# FUNCTION TO MAKE LABELS FOR CUT()
makeLabs<-function(x){paste(x[-length(x)],x[-1],sep="-")}



#######################################################################
# READ IN BEND DATA FOR RPMA 2 (UPPER) AND 4 (LOWER)
# USED FOR MOVEMENT ANALYSIS
#######################################################################
bends<- read.csv("./_dat/bend-data.csv")
# make data.frame column names lower case
names(bends)<- tolower(names(bends))
bends<- subset(bends,b_segment %in% c(1,2,3,4,7,8,9,10,13,14))
bends<-bends[-157,]



# FIX ID COLUMN DUE TO REMOVED BEND
bends<-bends[order(bends$lower_river_mile),]
bends$id<- 1:nrow(bends)

# MAKE A PAIRWISE DISTANCE MATRICES FOR EACH RPMA
## FIND LOWER RKM FOR EACH BEND
bends$lower_rkm<-0
for(i in 2:length(bends$lower_rkm))
    {
    bends$lower_rkm[i]<-bends$lower_rkm[i-1]+bends$length.rkm[i-1]
    }
## FIND BEND CENTER
bends$center<- bends$lower_rkm + bends$length.rkm/2

## ADD WITHIN RPMA BEND ID, INCREASING ORDER MOVING UPSTREAM
bends$b_id<- 1
bends$b_id[which(bends$rpma==2)]<- 1:length(which(bends$rpma==2))
bends$b_id[which(bends$rpma==4)]<- 1:length(which(bends$rpma==4))


#######################################################################
#
# DECISION NODES 
#
#######################################################################

## DESIGN
design<- c("Random","Fixed")

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
##recruitmentLevel<- round(runif(NN,1,1000),0)


## 3. INTERCEPTION LOCATION
intLocation<-c(1,1/3,2/3,0)
intLocationLabs<-c("Anywhere","Lower third","Lower 2/3","Outside of basin")



## 4. DETECTION PROBABILITY
pDetectBrks<-c(0,0.02,0.04,0.06,0.08,0.1)
pDetectLabs<- makeLabs(pDetectBrks)

## 5. NUMBER OF BENDS SAMPLED BY OF FIELD OFFICES, SEGMENTS, AND NUMBER 
##    SAMPLED WITHIN EACH SEGMENT PER PSPAP PROGRAM 
##    NOT REALLY A DECISION NODE RIGHT NOW, BUT WILL BE IN FUTURE
segs<-data.frame(basin=c("Upper","Upper","Upper","Upper","Lower","Lower","Lower","Lower",
        "Lower","Lower","Lower"),
    segment=c(1,2:4,7,8,9,9,10,13,14), 
    nBends=c(0,12, 21, 12, 12, 15, 10,10, 10, 11, 14),## NE and MO does segment 9 equally
    fieldOffice=c("MT","MT","MT","MR","SD","NE",
        "NE","MO","MO","CF","CF"))

        
        
        
        
#######################################################################
#
#  SIMULATE RESULTS TO PARAMETERIZE prob_detect NODE
#
#######################################################################

## SET UP COMBINATIONS OF INPUTS
combos<-expand.grid(design=design, 
    percentBends=22,
    ntrawlsLabs=ntrawlsLabs, 
    basin=basin,
    intLocation=intLocation, 
    recruitmentLevelLabs=recruitmentLevelLabs,
    pDetectLabs=pDetectLabs)
combos$id<-c(1:nrow(combos))


## SET UP VECTORS OF BENDS AND BEND LENGTHS
## TO SAMPLE IN SIMULATION
### UPPER
upper<- subset(bends,basin=="upper")
upper<- upper[order(upper$b_segment,upper$bend_num),]
upper$cumRkm<-cumsum(upper$length.rkm)
### LOWER
lower<- subset(bends,basin=="lower")
lower<- lower[order(lower$b_segment,lower$bend_num),]
lower$cumRkm<-cumsum(lower$length.rkm)
  # CAUTION! THIS CUMULATIVE SUM MIGHT NOT BE WHAT YOU WANTED,
  #   LOWER BASIN BEND NUMBERS ARE DIFFERENT W/IN SEGMENT (SOME COUNT
  #   UP, OTHERS COUNT DOWN)
  # lower2<-lower[order(lower$lower_rkm),]
  # all(lower$id==lower2$id)


## ~ 12 minutes to run on 3 cores
cl<- makeCluster(3)
library(pbapply)
clusterExport(cl, c("combos","bends","segs"))
outt<- pblapply(1:nrow(combos),function(x)
    {
    ## SET UP REPS TO RUN FOR EACH COMBINATION OF INPUTS
    reps<-250
    
    
    bl<- bends[bends$basin==tolower(combos$basin[x]),]$length.rkm
    bl[cumsum(bl)>sum(bl)*combos$intLocation[x]]<-0
    if(sum(bl)>0){lwp<- bl/sum(bl)}else{lwp<-rep(0,length(bl))}
    
    ## SAMPLE WITHIN RECRUITMENT LEVEL INTERVALS
    rr<- as.numeric(unlist(strsplit(as.character(combos$recruitmentLevel[x]),"-")))
    ## RANDOMLY SELECT A NUMBER OF RECRUITS WITHIN THE BOUNDS
    abunds<-round(runif(reps,rr[1],rr[2]))
    ## ALLOCATE RECRUITS TO A BEND FOR EACH REPLICATE    
    abund<-sapply(1:length(abunds), function(i) 
        {
        if(sum(lwp)>0){ww<-rmultinom(1, abunds[i],  lwp)}
        if(sum(lwp)==0){ww<-rep(0,length(lwp))}
        return(ww)
        })
    ### CONVERT THE BEND TO BEING OCCUPIED OR NOT 
    abund[abund>1]<-1  
    
    ## SET UP THE NUMBER OF TRAWLS TO RUN
    trawls<- as.numeric(unlist(strsplit(as.character(combos$ntrawlsLabs[x]),"-")))
    trawls<-floor(runif(ncol(abund),trawls[1],trawls[2]-0.001))
    
    ## SET UP THE DETECTION PROBABILITY
    pdetect<- as.numeric(unlist(strsplit(as.character(combos$pDetectLabs[x]),"-")))    
    pdetect<- runif(ncol(abund),pdetect[1],pdetect[2]-0.0001)    
    ## SIMULATE MANY REPLICATE SAMPLES AND WHETHER AN AN
    ## AGE-1 PS IS DETECTED IN AN OCCUPIED BEND
    detected<-lapply(1:reps,function(i)
        {
        b_segments<-data.frame(b_segment=bends[bends$basin==tolower(combos$basin[x]),]$b_segment,
            tmp=0,
            sampled=0,
            FO=NA)
        samp<-segs[segs$basin==combos$basin[x],]
        for(ii in 1:nrow(samp))
            {
            indx<-which(b_segments$b_segment==samp$segment[ii] & b_segments$sampled != 1)
            b_segments[indx,]$tmp<- runif(nrow(b_segments[indx,]),0,1)
            b_segments[indx,]$sampled<- ifelse(order(b_segments[indx,]$tmp)<=samp$nBends[ii],1,0)
              # THIS RANDOMLY REORDERS THE BENDS THEN FINDS THE INDEX LOCATIONS OF THE FIRST XX
              # BENDS (NUMBER TO BE SAMPLED) AND USES THESE INDICES FOR THE BENDS TO BE SAMPLED
            if(samp$nBends[ii]>0){b_segments[b_segments$b_segment==samp$segment[ii] & 
                b_segments$sampled == 1 & is.na(b_segments$FO),]$FO<- as.character(samp[ii,]$fieldOffice)}
            }       
        xx<-matrix(rbinom(nrow(abund)*trawls[i],1,rep(pdetect[i]*abund[,i], trawls[i])),
            nrow(abund),trawls[i])        
        samp<-rowSums(xx[which(b_segments$sampled==1),])
        samp[samp>0]<-1
        return(data.frame(detect=max(samp)))
          #RETURNS BASIN LEVEL DETECTION
        })   
    
    detected<-do.call("rbind",detected)
    return(data.frame(id=x,
        notdetected = reps - sum(detected), 
        detected= sum(detected)))
    },cl=cl)
    
    
stopCluster(cl = cl)

outcomes<- data.table::rbindlist(outt)

boo<- subset(outcomes, notdetected<0)

## COMBINE COMBOS AND OUTCOMES
outcomes<-cbind(combos,outcomes)
outcomes$nreps<- outcomes$notdetected+outcomes$detected
outcomes$notdetected_prob<-outcomes$notdetected/outcomes$nreps
outcomes$detected_prob<-outcomes$detected/outcomes$nreps



## ENSURE FACTORS ARE ORDERED FOR NETICA
outcomes$design<- factor(as.character(outcomes$design),
    levels=c("Fixed","Random"),
    ordered=TRUE)
outcomes$ntrawlsLabs<- factor(as.character(outcomes$ntrawlsLabs),
    levels=c("2-3","3-4","4-5","5-10","10-20","20-30","30-40","40-50"),
    labels=c("2","3","4","5-9","10-19","20-29","30-39","40-49"),
      #Since "floor" was used
    ordered=TRUE)
outcomes$basin<- factor(as.character(outcomes$basin),
    levels=c("Lower","Upper"),
    ordered=TRUE)
outcomes$intLocation<- factor(outcomes$intLocation,
    levels=c(1.0000000,2/3,1/3,0),
    labels=c("Anywhere","Lower 2/3rds","Lower 1/3rd","Outside basin"),
    ordered=TRUE)
outcomes$recruitmentLevelLabs<- factor(as.character(outcomes$recruitmentLevelLabs),
    levels=c("1-30","30-60","60-100","100-500","500-1000","1000-5000"),
    ordered=TRUE)
outcomes$pDetectLabs<- factor(as.character(outcomes$pDetectLabs),
    levels=c("0-0.02","0.02-0.04","0.04-0.06","0.06-0.08","0.08-0.1"),
    ordered=TRUE)

    
    
    
    

## ORDER FOR NETICA TO COPY AND PASTE
outcomes<-outcomes[order(outcomes$design,
    outcomes$ntrawlsLabs,
    outcomes$basin,outcomes$intLocation,
    outcomes$recruitmentLevelLabs, 
    outcomes$pDetectLabs),]



write.csv(outcomes,"_output/age1-detection-cpt.csv")


outcomes<- read.csv("_output/age1-detection-cpt.csv")
outcomes$x<- 2
outcomes$x[outcomes$ntrawlsLabs==3]<-3
outcomes$x[outcomes$ntrawlsLabs==4]<-4
outcomes$x[outcomes$ntrawlsLabs=="5-10"]<-7.5
outcomes$x[outcomes$ntrawlsLabs=="11-20"]<-15.5
outcomes$x[outcomes$ntrawlsLabs=="21-30"]<-25.5
outcomes$x[outcomes$ntrawlsLabs=="31-40"]<-35.5
outcomes$x[outcomes$ntrawlsLabs=="41-50"]<-45.5


par(mfrow=c(3,2),mar=c(1,2,0,0),oma=c(4,2,1,1))
pp<- subset(outcomes,
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0-0.02" & design=="Random" & basin=="Upper")
plot(detected_prob~x,pp,type='n',
    ylim=c(0,1),
    xlab="",
    ylab="",las=1,xaxt='n',tck=0.02)
axis(1, at=axTicks(1),labels=FALSE,tck=0.02)
    axis(3, at=axTicks(1),labels=FALSE,tck=0.02);axis(4, at=axTicks(2),labels=FALSE,tck=0.02)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 1/3rd",pch=18)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 2/3rds",pch=17)    
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Anywhere",pch=16)   
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Outside basin")       
legend(29,0.5, 
    legend=c("Lower 1/3rd of basin","Lower 2/3rds of basin",
        "Anywhere in basin", "Outside of basin"),
    pch=c(18,17,16,1),lty=1,bty="n",
    title="Interception location")
panLab("  a) Upper; p=0.00 to 0.02")

## [1,2]  
pp<- subset(outcomes,
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0-0.02" & design=="Random" & basin=="Lower")

plot(detected_prob~x,pp,type='n',
    ylim=c(0,1),
    xlab="",
    ylab="",las=1,yaxt='n',xaxt='n',tck=0.02)
axis(1, at=axTicks(1),labels=FALSE,tck=0.02)
    axis(3, at=axTicks(1),labels=FALSE,tck=0.02);axis(4, at=axTicks(2),labels=FALSE,tck=0.02)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 1/3rd",pch=18)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 2/3rds",pch=17)    
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Anywhere",pch=16)   
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Outside basin")       
panLab("  b) Lower; p=0.00 to 0.02") 

## [2,1]
pp<- subset(outcomes,
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0.02-0.04" & design=="Random" & basin=="Upper")
plot(detected_prob~x,pp,type='n',
    ylim=c(0,1),
    xlab="",
    ylab="",las=1,xaxt='n',tck=0.02)
axis(1, at=axTicks(1),labels=FALSE,tck=0.02)
    axis(3, at=axTicks(1),labels=FALSE,tck=0.02);axis(4, at=axTicks(2),labels=FALSE,tck=0.02)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 1/3rd",pch=18)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 2/3rds",pch=17)    
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Anywhere",pch=16)   
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Outside basin")       
panLab("  c) Upper; p=0.02 to 0.04")      
    
## [2,2]   
pp<- subset(outcomes,
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0.02-0.04" & design=="Random" & basin=="Lower")

plot(detected_prob~x,pp,type='n',
    ylim=c(0,1),
    xlab="",
    ylab="",las=1,yaxt='n',xaxt='n')
axis(1, at=axTicks(1),labels=FALSE,tck=0.02)
axis(2, at=axTicks(2),labels=FALSE,tck=0.02)
    axis(3, at=axTicks(1),labels=FALSE,tck=0.02);axis(4, at=axTicks(2),labels=FALSE,tck=0.02)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 1/3rd",pch=18)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 2/3rds",pch=17)    
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Anywhere",pch=16)   
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Outside basin")       
panLab("  d) Lower; p=0.02 to 0.04")     
    
  
 ## [3,1]
pp<- subset(outcomes,
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0.04-0.06" & design=="Random" & basin=="Upper")
plot(detected_prob~x,pp,type='n',
    ylim=c(0,1),
    xlab="",
    ylab="",las=1,xaxt='n',tck=0.02);    axis(3, at=axTicks(1),labels=FALSE,tck=0.02);axis(4, at=axTicks(2),labels=FALSE,tck=0.02)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 1/3rd",pch=18)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 2/3rds",pch=17)    
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Anywhere",pch=16)   
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Outside basin")       
panLab("  e) Upper; p=0.04 to 0.06")      
axis(1, at=c(2,3,4,7.5, 15.5,25.5,35.5,45.5),
    labels=as.character(unique(pp$ntrawlsLabs)),las=2)   

 
## [3,2]   
pp<- subset(outcomes,
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0.04-0.06" & design=="Random" & basin=="Lower")

plot(detected_prob~x,pp,type='n',
    ylim=c(0,1),
    xlab="",
    ylab="",las=1,yaxt='n',xaxt='n',tck=0.02);axis(2, at=axTicks(2),labels=FALSE,tck=0.02);
    axis(3, at=axTicks(1),labels=FALSE,tck=0.02);axis(4, at=axTicks(2),labels=FALSE,tck=0.02)
    
mtext(side=2,  "Probablity of detecting a recruit",line=0.75, outer=TRUE)  

points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 1/3rd",pch=18)
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Lower 2/3rds",pch=17)    
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Anywhere",pch=16)   
points(detected_prob~x,pp,type='b',
    subset=intLocation=="Outside basin")       
panLab("  f) Lower; p=0.04 to 0.06")     
axis(1, at=c(2,3,4,7.5, 15.5,25.5,35.5,45.5),
    labels=as.character(unique(pp$ntrawlsLabs)),las=2)            
mtext(side=1,  "Number of trawls",line=3, outer=TRUE)         
       
savePlot("C:\\Users\\mcolvin\\Documents\\projects\\Pallid Sturgeon\\Analysis\\PSPAP-Reboot\\images\\fig8.pdf",
type='pdf')



     
       
       
par(mfrow=c(3,1),mar=c(1,1,1,1),oma=c(3,3,1,1))
plot(detected_prob~ntrawlsLabs,outcomes,
    subset=intLocation=="Lower 1/3rd" & 
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0-0.02" & design=="Random",        
        las=1,xaxt="n",ylim=c(0,1)) ;panLab("A")       
plot(detected_prob~ntrawlsLabs,outcomes,
    subset=intLocation=="Lower 1/3rd" & 
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0.02-0.04",las=1,xaxt="n",ylim=c(0,1));panLab("B")
 plot(detected_prob~ntrawlsLabs,outcomes,
    subset=intLocation=="Lower 1/3rd" & 
        recruitmentLevelLabs=="1-30" & 
        pDetectLabs=="0.04-0.06",las=1,xaxt="n",ylim=c(0,1)) 
axis(1,at=c(1:8), labels=c(2,3,4, "5-9","10-19","20-29","30-39","40-49"))  ;panLab("C")     
mtext(side=1, "Number of within bend trawls",outer=TRUE,line=1.5)
mtext(side=2, "Probability of detecting a recruit",outer=TRUE,line=1.5)
       
        

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
write.csv(out,"_output\\recruit-out.csv")
out<-read.csv("_output\\recruit-out.csv")


#write.csv(out,"C:\\Users\\mcolvin\\Desktop\\recruit-out.csv")
#out<-read.csv("C:\\Users\\mcolvin\\Desktop\\recruit-out.csv")





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

    
