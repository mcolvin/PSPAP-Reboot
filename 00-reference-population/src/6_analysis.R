## Population Simulations
segs<- c(1,2,3,4,7,8,9,11,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

fish_density<- 10

sim_pop<-reference_population(segs=segs,
    bends=bends,# BENDS DATAFRAME
    fish_density=10, # FISH DENSITY PER RKM
    phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

## Catch Simulations
sim_catch<-catch_counts(segs=segs,
                          bends=bends,
                          n=sim_pop$out,
                          catchability=c(0.0004, 0.0002, 0, 0.0004, 0.002, 0.1, 0.002, 0, 0.1),
                          deployments=rep(8,9),
                          effort=effort)

  head(sim_catch)
  sim_catch[1:6,1,1]
  dim(sim_catch)
    #NOTE: sim_catch[i,j,k] will give the catch for bend i, in year j, when using gear k.
    #See function description for list of gears k.
  
  
#Sampling Simulation
  sim_samp<-bend_samples(segs=segs,bends=bends,n=sim_pop$out)
  nrow(sim_pop$out)*ncol(sim_pop$out)==nrow(sim_samp)
  head(sim_samp)
  str(sim_samp)
  
  
#SAMPLING OUTPUT 
  segs<- c(1,2,3,4,7,8,9,11,10,13,14)
  nyears<- 10
  
  beta0<- 2.9444
  phi<-matrix(plogis(beta0),length(segs),nyears-1)
  
  sim_pop<-reference_population(segs=segs,
                                bends=bends,# BENDS DATAFRAME
                                fish_density=10, # FISH DENSITY PER RKM
                                phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS
  
  sim_dat<-samp_dat(segs=segs,
            bends=bends,# BENDS DATAFRAME
            n=sim_pop$out,
            catchability=c(0.00004, 0.00004, 0.00004, 0.00004, 0.00004, 0.0002, 0.00004, 0.00004, 0.0002),
            deployments=rep(8,9),
            effort=effort)
  head(sim_dat)
  

# CPUE ANALYSIS FOR TREND

tmp<- aggregate(cpue~year+segment,sim_dat,mean)
tmp$segment<- as.factor(tmp$segment)
tmp$lncpue<- log(tmp$cpue)


## PLOT CPUE OVER TIME FOR EACH SEGMENT
xyplot(cpue~year, tmp, group=segment,type='b')
xyplot(lncpue~year, tmp, group=segment,type='b')

plot(log(r_abundance)~year,sim_dat)
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

get.trnd(segs=segs,
         bends=bends,# BENDS DATAFRAME
         n=sim_pop$out,
         effort=effort)

#### MAKE 100 REPLICATES
##### DEFINE FIXED REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,11,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=10, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

##### RUN RANDOM SAMPLING EFFORTS AND PULL OUT TREND
n<-100 #number of replicates
result <- t(replicate(n, get.trnd(segs=segs,
                                  bends=bends,# BENDS DATAFRAME
                                  n=sim_pop$out,
                                  effort=effort)
                    ))
result<-as.data.frame(result)
result$sig<-ifelse(result$pval<0.05,1,0)
head(result)
mean(result$year)
power<-sum(result$sig)/n
power

