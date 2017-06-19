#####################
## SIMULATING DATA ##
#####################

## GENERATE THE REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=init_dens, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

## SIMULATE THE SAMPLING EFFORT AND CATCH DATA FOR SEVERAL REPLICATES
nrep<-10
q_sd<-c(0.08, 0.1, 0.08, 0.1, 0.07, 1.2, 0.08, 0.1, 1.2)
store<-replicate(nrep,samp_dat(sim_pop=sim_pop,
                      q_sd=q_sd,
                      effort=effort))
### SAVE THE SIMULATED DATA
save(store, file=paste("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/sim_samp_sd", q_sd[1], sep=""))


## FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
### INITIALIZE
get_trnd<-list() #TO HOLD CPUE TREND
highCP_flags<-numeric() # TO HOLD NUMBER  OF HIGH CPs

### GET TREND FOR ALL REPLICATES
for(i in 1:nrep)
{
  get_trnd[[i]]<-get.trnd(store[,i])
  get_trnd[[i]]<-do.call(rbind, get_trnd[[i]])
  highCP_flags<-c(highCP_flags,length(which(store[,i]$cpue_long$flags==1)))
}

## GET.TRND FAILS WHEN WE HAVE CPUES=ZERO, so LNCPUES=-Inf
## HOW DO WE WANT TO DEAL WITH THIS???


### PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
get_trnd<-do.call(rbind, get_trnd)
get_trnd$sig95<-ifelse(get_trnd$pval<0.05,1,0)
get_trnd$sig90<-ifelse(get_trnd$pval<0.1,1,0)
head(get_trnd)
dim(get_trnd)


### MAKE A TABLE OF RESULTS
ddply(get_trnd, .(gear), summarize,
      mean_trnd=mean(trnd),
      mean_se=mean(se),
      max_se=max(se),
      mean_pval=mean(pval),
      max_pval=max(pval),
      power95=sum(sig95)/nrep,
      power90=sum(sig90)/nrep)







###########################
##  INDIVIDUAL FUNCTIONS ##
###########################

## Population Simulations

segs<- c(1,2,3,4,7,8,9,11,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=10, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS




yyy<-catch_counts(sim_pop=sim_pop,
                  gears=c("GN14", "GN18", "GN41", "GN81", 
                          "MF", "OT16", "TLC1", "TLC2", "TN"),
                  catchability=c(0.00002, 0.00004, 0.00002, 0.00004,
                                 0.00004, 0.0002, 0.00002, 0.00004,
                                 0.0002),
                  q_sd=c(0.08, 0.1, 0.08, 0.1, 0.07, 1.2, 0.08, 0.1, 1.2),
                  deployments=rep(8,9),
                  effort=effort,
                  occasions=3)

# returns list of values    

yyyy<- bend_samples(sim_pop=sim_pop)   

yyyyy<-samp_dat(sim_pop=sim_pop,
                gears=c("GN14", "GN18", "GN41", "GN81", "MF", 
                        "OT16", "TLC1", "TLC2", "TN"),
                catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 
                               0.00004, 0.0002, 0.00004, 0.00004, 0.0002),
                deployments=rep(8,9),
                effort=effort,
                occasions=3)   
names(yyyyy)  

# yyyyy$cpue_long this can feed the beast below
head(yyyyy$cpue_long)

trnd_dat<-get.trnd(sim_dat=sim_dat,
                   gears=c("GN14", "GN18", "GN41", "GN81",
                           "MF", "OT16", "TLC1", "TLC2", "TN"))

trnd_dat<-do.call("rbind",trnd_dat)


########################################
## PREVIOUS (OLD FUNCTION) REPLICATES ##
########################################

#### MAKE 100 REPLICATES
##### DEFINE FIXED REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,11,10,13,14)
beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)
sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=10, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

##### RUN RANDOM SAMPLING EFFORTS AND PULL OUT TREND
nrep<- #number of replicates
sim_dat<-samp_dat(sim_pop=sim_pop,
                  gears=c("GN14", "GN18", "GN41", "GN81", "MF", 
                          "OT16", "TLC1", "TLC2", "TN"),
                  catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 
                                 0.00004, 0.0002, 0.00004, 0.00004, 0.0002),
                  deployments=rep(8,9),
                  effort=effort,
                  occasions=1) 


result <- t(replicate(nrep, get.trnd(segs=segs,
                                     bends=bends,# BENDS DATAFRAME
                                     abund=sim_pop$out,
                                     gears=c("GN14","GN18","GN41", "GN81", "MF", "OT16","TLC1", "TLC2","TN"),
                                     catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.002, 0.00004, 0.00004, 0.002), #BY GEAR,
                                     deployments=rep(8,9), #BY GEAR
                                     effort=effort)
))
result<-do.call("rbind", result)
result$sig<-ifelse(result$pval<0.05,1,0)
head(result)
dim(result)


##### MAKE A TABLE OF RESULTS
ddply(result, .(gear), summarize,
      mean_trnd=mean(trnd),
      mean_se=mean(se),
      max_se=max(se),
      mean_pval=mean(pval),
      max_pval=max(pval),
      power=sum(sig)/nrep)






### RUN SAME TREND ANALYSIS WITH VARIABLE SIM_POP
#### DEFINE FIXED VALUES
segs<- c(1,2,3,4,7,8,9,11,10,13,14)

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

#### RUN RANDOM SAMPLING EFFORTS ON RANDOM SIM_POP
nrep<-100 #number of replicates
#### TRUE POPULATION TO SAMPLE

result <- t(replicate(nrep, 
                      {
                        sim_pop<-reference_population(
                          segs=segs,
                          bends=bends,# BENDS DATAFRAME
                          fish_density=10, # FISH DENSITY PER RKM
                          phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS
                        get.trnd(segs=segs,
                                 bends=bends,# BENDS DATAFRAME
                                 abund=sim_pop$out,
                                 gears=c("GN14","GN18","GN41", "GN81", "MF", "OT16","TLC1", "TLC2","TN"),
                                 catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.002, 0.00004, 0.00004, 0.002), #BY GEAR,
                                 deployments=rep(8,9), #BY GEAR
                                 effort=effort)
                      }))
result<-do.call("rbind", result)
result$sig<-ifelse(result$pval<0.05,1,0)

ddply(result, .(gear), summarize,
      mean_trnd=mean(trnd),
      mean_se=mean(se),
      max_se=max(se),
      mean_pval=mean(pval),
      max_pval=max(pval),
      power=sum(sig)/nrep)

### bias

hist(result$trnd)
abline(v=-0.05,lty=2,col='red',lwd=5)
mean(result$trnd- -0.05)






###########################################
###########################################






# CPUE ANALYSIS FOR TREND
tmp<- aggregate(cpue~year+segment,sim_dat,mean)
tmp$segment<- as.factor(tmp$segment)
tmp$lncpue<- log(tmp$cpue)

## PLOT CPUE OVER TIME FOR EACH SEGMENT
xyplot(cpue~year, tmp, group=segment,type='b')
xyplot(lncpue~year, tmp, group=segment,type='b')

plot(log(r_abundance)~year,sim_dat,type='n')
points(log(r_abundance)~year,sim_dat,subset=rpma==2)
points(log(r_abundance)~year,sim_dat,subset=rpma==4)

## TRUE TREND
fit<- lm(log(r_abundance)~year+rpma,sim_dat)
summary(fit)

## TREND ANALYSIS
### FIT LINEAR MODEL FOR TREND
fit<- lm(lncpue~segment+year, tmp)
### THE GOODIES
#### TREND ESTIMATE
trnd<- coef(fit)['year']
#### PVALUE FOR TREND ESTIMATE
pval<-summary(fit)$coefficients['year',4]


### USE THE GET.TRND FUNCTION
segs<- c(1,2,3,4,7,8,9,11,10,13,14)
nyears<- 10
beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=10, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS
tmp<-bend_samples(segs=c(1,2,3,4,7,8,9,10,13,14),
                  bends=bends,
                  abund=sim_pop$out)


trnd_dat<-get.trnd(segs=segs,
                   bends=bends,# BENDS DATAFRAME
                   abund=sim_pop$out,
                   gears=c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN"),
                   catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.002, 0.00004, 0.00004, 0.002), #BY GEAR,
                   deployments=rep(8,9), #BY GEAR
                   effort=effort)
trnd_dat<-do.call("rbind",trnd_dat)



