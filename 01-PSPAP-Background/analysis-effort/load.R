library(RODBC)
library(digest)
library(plyr)
library(reshape2)
#hash <- digest(txt, algo="md5", serialize=F)
#hash

com3<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Pallid Sturgeon/Analysis/Data/pallids.accdb")
dat<-sqlFetch(com3, "Gear-Specific-Effort")



dat<-readRDS("./output/dat.RDS")
hash_imp<-digest(dat,algo="md5", serialize=TRUE)
hash<-readRDS("./output/hash.RDS")
