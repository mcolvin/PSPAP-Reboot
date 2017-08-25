# DATA SIMULATED IN THIS SCRIPT
## 1. REFERENCE POPULATIONS:
###  a. abundance
###  b. survival
###  c. length/growth
###  d. movement
## 2. CATCH DATA
###  a. true segment level data
###  b. sampling data
###  c. capture histories
## 3. FUNCTION & DATA SAVE TESTS

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")


## 1. GENERATE REFERENCE POPULATIONS
inputs<-list()
# GENERAL INFO
inputs$segs<- c(1,2,3,4,7,8,9,10,13,14)
inputs$nyears<- 10
inputs$bends<-bends  # BENDS DATAFRAME
inputs$fish_density<-init_dens  # FISH DENSITY PER RKM

# SURVIVAL
beta0<- 2.9444
inputs$phi<-matrix(plogis(beta0),length(inputs$segs),inputs$nyears-1)  
    # MATRIX OF YEAR AND SEGEMENT SPECIFIC SURVIVALS

# GROWTH
## LOWER BASIN
inputs$lower$ln_Linf_mu<-6.982160
inputs$lower$ln_k_mu<- -2.382711
inputs$lower$vcv<- matrix(c(0.0894,-0.1327,-0.1327,0.3179),nrow=2,ncol=2, byrow=TRUE)
## UPPER BASIN
inputs$upper$ln_Linf_mu<-7.136028770
inputs$upper$ln_k_mu<- -3.003764445
inputs$upper$vcv<- matrix(c(0.2768,-0.364,-0.364,0.6342),nrow=2,ncol=2, byrow=TRUE)
## INITIAL LENGTH
#initial_length<-data.frame()
#for(ss in inputs$segs)
#  {
#    x<-runif(1000,30,1200)
#    qntls<-seq(0,1,0.025)
#    vals<-data.frame(segment=ss,qntls=qntls,vals=quantile(x,qntls))
#    initial_length<- rbind(initial_length,vals)
#  }
inputs$initial_length<-initial_length

# MOVEMENT
inputs$mv_beta0<-c(0,0)
inputs$mv_beta1<-c(1,1)
inputs$dis<-sp$dis
#inputs$direct<-sp$direct

# RUN IN PARALLEL
ptm<-proc.time()
nreps<-200
library(parallel)
## USE ALL CORES
numCores<-detectCores()
## INITIATE CLUSTER
cl<-makeCluster(numCores)
## MAKE PREVIOUS ITEMS AND FUNCTIONS AVAILABLE
clusterExport(cl, c("inputs", "nreps","pcname"))
clusterEvalQ(cl, source("_R/2_functions.R"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(plyr))
parLapply(cl,1:nreps, function(i)
{
  sim_pop<-reference_population(inputs = inputs)
  # SAVE POPULATION
  if(pcname=="WF-FGNL842")
    {saveRDS(sim_pop,file=paste0("E:/_output/1-populations/sim_pop_",i,".rds"))}
  if(pcname!="WF-FGNL842")
    {saveRDS(sim_pop,file=paste0("_output/sim_pop_",i,".rds"))}
  
})
stopCluster(cl)
proc.time()-ptm
# CRUNCH FOR 200 (ABOUT 4.5 HOURS)
# user    system    elapsed 
# 2.53    0.80      15979.43
        

## 2. GENERATE CATCH DATA (FOR RANDOM DRAWS OF CATCHABILITY AND B0_SD)
# INPUTS
inputs<-list()
inputs$samp_type<-"r"
inputs$gears<-c("GN14", "GN18", "GN41", "GN81","MF", "OT16", "TLC1", "TLC2", "TN")
inputs$deployments<-rep(8,9)
inputs$occasions<-4
inputs$effort<-effort

# RUN W/O PARALLEL (CATCH_DAT ALREADY MAKES USE OF PARALLEL)
ptm<-proc.time()
nreps<-2
lapply(1:200, function(i)
  {
  if(pcname=="WF-FGNL842")
    {sim_pop<-readRDS(file=paste0("E:/_output/1-populations/sim_pop_",i,".rds"))}
  if(pcname!="WF-FGNL842")
    {sim_pop<-readRDS(file=paste0("_output/sim_pop_",i,".rds"))}
  lapply(1:nreps,function(j)
    {
      ## MEAN CATCHABILITY
      #q_mean<-runif(9,0.000000, 0.001) # Favors larger values
      #q_mean<-10^(-runif(9,3,6)) # More even spacing of magnitudes
      q_mean<-c(runif(5,0.000000, 0.00005), runif(1,0.0001, 0.004),
                runif(2,0.000000, 0.00005),runif(1,0.00009, 0.004))
      # Accounts for differences in "OT16" and "TN" efforts.
      inputs$catchability<-q_mean
      ## B0_SD  
      inputs$B0_sd<-runif(9,0,1.5)
      ## SAMPLING & CATCH DATA
      dat<-catch_data(sim_pop=sim_pop,inputs=inputs)
      if(pcname=="WF-FGNL842")
        {saveRDS(dat,
          file=paste0("E:/_output/2-catch/catch_dat_", inputs$samp_type,"_",i,"-",j,".rds"))}
      if(pcname!="WF-FGNL842")
        {saveRDS(dat,
          file=paste0("_output/catch_dat_", inputs$samp_type,"_",i,"-",j,".rds"))}
      })
  })
proc.time()-ptm
# CRUNCH for 2x200 (ABOUT 12 HOURS)
# user      system    elapsed 
# 4298.17   355.77    43635.42 (r)
# 4280.51   346.73    43428.20 (f) 





#####################################
## 3. FUNCTION AND DATA SAVE TESTS ##
#####################################

segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=init_dens, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

yyr<- bend_samples(sim_pop=sim_pop,"r")
yyf<- bend_samples(sim_pop=sim_pop,"f")

ptm<-proc.time()
dat_trial<-catch_data(sim_pop=sim_pop,
                      samp_type = "f",
                      catchability=rep(0.000005,9),
                      B0_sd=rep(0.5,9),
                      effort=effort)

## TESTING SAVE FORMATS
saveRDS(dat_trial,
        file=paste0("_output/catch_dat_rep",gsub(":", "_", Sys.time()),".rds"))
save(dat_trial,
     file=paste0("_output/catch_dat_rep",gsub(":", "_", Sys.time()),".RData"))
proc.time()-ptm
# user      system    elapsed 
# 68.59     0.28      68.94 
## BOTH ~6.8MB

ptm<-proc.time()
save(dat_trial,
     file=paste0("_output/catch_dat_rep",gsub(":", "_", Sys.time()),".gzip"),
     compress="gzip")
proc.time()-ptm
# user    system    elapsed 
# 1.69    0.04      1.72 
## STILL ~6.8MB

ptm<-proc.time()
save(dat_trial,
     file=paste0("_output/catch_dat_rep",gsub(":", "_", Sys.time()),".bzip2"),
     compress="bzip2")
proc.time()-ptm
# user    system    elapsed 
# 10.11   0.01      10.20 
## REDUCED SLIGHTLY ~6.2MB


ptm<-proc.time()
save(dat_trial,
     file=paste0("_output/catch_dat_rep",gsub(":", "_", Sys.time()),".xz"),
     compress="xz")
proc.time()-ptm
# user    system    elapsed 
# 15.78   0.14      16.01
## RECUCED TO ~5MB

## LOOK AT TEST OUTPUT
sim_dat<-readRDS("E:/_output/catch_dat_2017-08-04 17_46_33_samp_type_r_catchability_random_rep_2017-08-04 17_55_54.rds")
sim_dat$true_vals
head(sim_dat$samp_dat)
head(sim_dat$catch_dat)


### TESTING NEW REF_POP FUNCTION
ptm<-proc.time()
dims<-NULL
check<-replicate(1000,    
                 {
                   sim_pop<-reference_population(segs=segs,
                                                 bends=bends,# BENDS DATAFRAME
                                                 fish_density=init_dens, # FISH DENSITY PER RKM
                                                 phi=phi,
                                                 Linf=rep(1000,10),
                                                 k = rep(0.2,10),
                                                 vbgf_vcv=vbgf_vcv,
                                                 initial_length=initial_length) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS
                   dims<-c(dims,nrow(sim_pop$out))
                   return(dims)
                 })
proc.time()-ptm
#user       system    elapsed 
#1147.76    0.59      1157.84 
all(check==471)
#[1] TRUE