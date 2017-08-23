# SIMULATING DATA 
## CURRENTLY FOR A SINGLE REFERENCE POPULATION
## BUT CAN MODIFY TO MAKE REFERENCE POPULATION REPLICATES TOO

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

## GENERATE THE REFERENCE POPULATION
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


sim_pop<-reference_population(inputs)

# SAVE POPULATION
sim_pop_ref<-gsub(":", "_", Sys.time())
saveRDS(sim_pop,
        file=paste0("_output/sim_pop_version_",sim_pop_ref,".rds"))


        
        

## MAKE CATCH DATA REPLICATES FOR RANDOM DRAWS OF CATCHABILITY AND B0_SD
### INPUTS
inputs<-list()
inputs$samp_type<-"r"
inputs$gears<-c("GN14", "GN18", "GN41", "GN81","MF", "OT16", "TLC1", "TLC2", "TN")
inputs$deployments<-rep(8,9)
inputs$occasions<-4
inputs$effort<-effort


### RUN
ptm<-proc.time()
nreps<-3

lapply(1:nreps, function(i)
    {
    ## MEAN CATCHABILITY
    #q_mean<-runif(9,0.000000, 0.001) # Favors larger values
    #q_mean<-10^(-runif(9,3,6)) # More even spacing of magnitudes
    q_mean<-c(runif(5,0.000000, 0.00005), runif(1,0.00005, 0.001),
              runif(2,0.000000, 0.00005),runif(1,0.00005, 0.001))
      # Accounts for differences in "OT16" and "TN" efforts.
    inputs$catchability<-q_mean
    ## B0_SD  
    inputs$B0_sd<-runif(9,0,1.5)
    ## SAMPLING & CATCH DATA
    dat<-catch_data(sim_pop=sim_pop,inputs)
    saveRDS(dat,
        file=paste0("_output/catch_dat_", sim_pop_ref, "_samp_type_",inputs$samp_type,"_catchability_random_rep_",gsub(":", "_", Sys.time()),".rds"))  
    })
proc.time()-ptm

sim_dat<-readRDS(file=paste0("_output/catch_dat_2017-08-22 16_38_36_samp_type_r_catchability_random_rep_2017-08-22 16_50_52.rds"))



###################################
##  FUNCTION AND DATA SAVE TESTS ##
###################################

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