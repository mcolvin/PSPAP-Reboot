
head(dat)

###########################
# FIT GAMMA DISTRIBUTIONS #
###########################

# FIND STANDARD LOWER & UPPER BASIN GEARS
datLB<-subset(dat, standard_gear=="yes" & basin=="LB")
dim(datLB)
LBgears<-unlist(lapply(unlist(levels(datLB$gear)), function(x) 
{
  lg<-subset(datLB, gear==x)
  if(nrow(lg)!=0) return(x)
}
))

datUB<-subset(dat, standard_gear=="yes" & basin=="UB")
dim(datUB)
UBgears<-unlist(lapply(unlist(levels(datUB$gear)), function(x) 
{
  lg<-subset(datUB, gear==x)
  if(nrow(lg)!=0) return(x)
}
))


# MAKE A LIST OF DISTRIBUTION PARAMETERS
valuesLB<-unlist(lapply(c(1:12,15,16,18), dfitfunLB))
shapesLB<-c(valuesLB[(2*c(1:length(LBgears))-1)])
ratesLB<-c(valuesLB[(2*c(1:length(LBgears)))])

valuesUB<-unlist(lapply(1:length(UBgears), dfitfunUB))
shapesUB<-c(valuesUB[(2*c(1:length(UBgears))-1)])
ratesUB<-c(valuesUB[(2*c(1:length(UBgears)))])

shapes<-c(shapesLB[1:12],NA,NA,shapesLB[13:14],NA,shapesLB[15],shapesUB)
rates<-c(ratesLB[1:12],NA,NA,ratesLB[13:14],NA,ratesLB[15],ratesUB)