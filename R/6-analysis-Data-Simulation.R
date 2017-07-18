# SIMULATING DATA
source("R/1_global.R")
source("R/2_functions.R")
source("R/3_load-and-clean.R")
source("R/4_figures.R")
source("R/5_tables.R")
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
        file=paste0("output/sim_pop_version_",sim_pop_ref,".rds"))


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
                    file=paste0("output/catch_dat_", sim_pop_ref, "_catchability_random_rep_",gsub(":", "_", Sys.time()),".rds"))  
          })
proc.time()-ptm