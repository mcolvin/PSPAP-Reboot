

# LOOK AT EFFORT BY GEAR
par(mfrow=c(1,2),mar=c(4,4,3,2))
figsLB<-function(x)
{
  datLBgear<-subset(datLB, gear==LBgears[x])
  #Make Sure There's Only One Gear ID
  if(length(levels(as.factor(datLBgear$gear_id)))>1) print("Warning: Multiple Gear IDs for this Gear")
  #Make Plots    
  hist(datLBgear$effort,  xlab="Effort (in minutes)", main=paste("LB ", LBgears[x], " Effort"))
  boxplot(datLBgear$effort, ylab="Effort (in minutes)", main=paste("LB ", LBgears[x], " Effort"))
  #barplot(datLBgear$effort,  ylab="Effort (in minutes)", main=paste("LB ", LBgears[x], " Effort"))
}
#figsLB(18)

figsUB<-function(x)
{
  datUBgear<-subset(datUB, gear==UBgears[x])
  #Make Sure There's Only One Gear ID
  if(length(levels(as.factor(datUBgear$gear_id)))>1) print("Warning: Multiple Gear IDs for this Gear")
  #Make Plots    
  hist(datUBgear$effort, xlab="Effort (in minutes)", main=paste("UB ", UBgears[x], " Effort"))
  boxplot(datUBgear$effort, ylab="Effort (in minutes)", main=paste("UB ", UBgears[x], " Effort"))
  #barplot(datUBgear$effort,  ylab="Effort (in minutes)", main=paste("UB ", UBgears[x], " Effort"))
}
#figsUB(8)


# FIT DISTRIBUTIONS TO DATA  
dfitfunLB<-function(x)
{
  datLBgear<-subset(datLB, gear==LBgears[x])
  dfit<-fitdistr(datLBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}

dfitfunUB<-function(x)
{
  datUBgear<-subset(datUB, gear==UBgears[x])
  dfit<-fitdistr(datUBgear$effort, "gamma")
  #Define Shape and Rate Based on Distribution Fitting
  s<-as.numeric(unlist(dfit)[1])
  r<-as.numeric(unlist(dfit)[2])
  return(c(s,r))
}


#LOOK AT DISTRIBUTION FIT
fitfigsLB<-function(x)
{
  s<-dfitfunLB(x)[1]
  r<-dfitfunLB(x)[2]
  datLBgear<-subset(datLB, gear==LBgears[x])
  #Make QQ and Density Plots
  par(mfrow=c(1,2),mar=c(4,4,3,2))
  theo<-rgamma(n=length(datLBgear)*4, shape=s, rate=r)
  qqplot(theo,datLBgear$effort, xlab="Theoretical", ylab=paste(LBgears[x],"Data"), main="QQ-Plot")
  abline(0,1)
  plot(density(datLBgear$effort), xlab="Effort", ylab="Data", main="PDF")
  px<-seq(min(datLBgear$effort), max(datLBgear$effort),floor((max(datLBgear$effort)-min(datLBgear$effort))/25))
  py<-dgamma(px, shape=s, rate=r)
  points(px,py, xlab="Effort", ylab="Normal Function", main="PDF")
}
#fitfigsLB(18)

fitfigsUB<-function(x)
{
  s<-dfitfunUB(x)[1]
  r<-dfitfunUB(x)[2]
  datUBgear<-subset(datUB, gear==UBgears[x])
  #Make QQ and Density Plots
  par(mfrow=c(1,2),mar=c(4,4,3,2))
  theo<-rgamma(n=length(datUBgear)*4, shape=s, rate=r)
  qqplot(theo,datUBgear$effort, xlab="Theoretical", ylab="Data", main="QQ-Plot")
  abline(0,1)
  plot(density(datUBgear$effort), xlab="Effort", ylab=paste(LBgears[x],"Data"), main="PDF")
  px<-seq(min(datUBgear$effort), max(datUBgear$effort),floor((max(datUBgear$effort)-min(datUBgear$effort))/25))
  py<-dgamma(px, shape=s, rate=r)
  points(px,py, xlab="Effort", ylab="Normal Function", main="PDF")
}
#fitfigsUB(1)



