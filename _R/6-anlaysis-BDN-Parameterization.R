
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


nreps<- 500000
upper<- rpois(nreps,exp(runif(nreps,2.3,3.92)))   # ln(MEAN RECRUITS)|RECRUITMENT
lower<- rpois(nreps,exp(runif(nreps,2.3,4.61)))   # ln(MEAN RECRUITS)|RECRUITMENT
tmp<-data.frame(basin=c(rep("upper",nreps),rep("lower",nreps)),
    recruitment=c(upper,lower))
max(tmp$recruitment)
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
 
bins<- c(0,1,    
    
    
