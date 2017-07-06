


source("R/1_global.R")
source("R/2_functions.R")
source("R/3_load-and-clean.R")
source("R/4_figures.R")
source("R/5_tables.R")



# GENERATE THE REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=init_dens, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

saveRDS(sim_pop,
        file=paste0("output/sim_pop_version",gsub(":", "_", Sys.time()),".rds"))

        # SIMULATE EFFORT & CATCH DATA FOR A FIXED B0_SD GRID
## MEAN CATCHABILITY

proc.time()-ptm

dat<-samp_dat(sim_pop=sim_pop,
    catchability=rep(0.00001,9),
    B0_sd=runif(9, 0,1.5),
    effort=effort,
    occasions=3)
saveRDS(dat,file="output/test.Rds") 



proc.time()-ptm
#user       system    elapsed 
#15795.57   68.90     15988.57


