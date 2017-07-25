# SIMULATING DATA 
## CURRENTLY FOR A SINGLE REFERENCE POPULATION
## BUT CAN MODIFY TO MAKE REFERENCE POPULATION REPLICATES TOO
source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")
source("_R/4_figures.R")
source("_R/5_tables.R")
## GENERATE THE REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=init_dens, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS
sim_pop_ref<-gsub(":", "_", Sys.time())
saveRDS(sim_pop,
        file=paste0("_output/sim_pop_version_",sim_pop_ref,".rds"))


## MAKE CATCH DATA REPLICATES FOR RANDOM DRAWS OF CATCHABILITY AND B0_SD
ptm<-proc.time()
nreps=500

replicate(nreps,
          {
            ## MEAN CATCHABILITY
            #q_mean<-runif(9,0.000000, 0.001) # Favors larger values
            #q_mean<-10^(-runif(9,3,6)) # More even spacing of magnitudes
            q_mean<-c(runif(5,0.000000, 0.00005), runif(1,0.00005, 0.001),
                      runif(2,0.000000, 0.00005),runif(1,0.00005, 0.001))
              # Accounts for differences in "OT16" and "TN" efforts.
            ## B0_SD  
            B0_sd<-runif(9,0,1.5)
            ## SAMPLING & CATCH DATA
            dat<-catch_data(sim_pop=sim_pop,
                            catchability=q_mean,
                            B0_sd=B0_sd,
                            effort=effort,
                            occasions=3)
            saveRDS(dat,
                    file=paste0("_output/catch_dat_", sim_pop_ref, "_catchability_random_rep_",gsub(":", "_", Sys.time()),".rds"))  
          })
proc.time()-ptm





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

yy<- bend_samples(sim_pop=sim_pop)

ptm<-proc.time()
dat_trial<-catch_data(sim_pop=sim_pop,
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
sim_dat<-readRDS("_output/catch_dat_catchability_random_rep2017-07-07 11_43_38.rds")
sim_dat$true_vals
head(sim_dat$samp_dat)
head(sim_dat$catch_dat)