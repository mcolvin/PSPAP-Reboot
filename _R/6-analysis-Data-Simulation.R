# SIMULATING DATA 
## CURRENTLY FOR A SINGLE REFERENCE POPULATION
## BUT CAN MODIFY TO MAKE REFERENCE POPULATION REPLICATES TOO

source("_R/1_global.R")
source("_R/2_functions.R")
source("_R/3_load-and-clean.R")

## GENERATE THE REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)
Linf<-rep(1000,length(segs))
k<-rep(0.02,length(segs))
vbgf_vcv<- array(0,dim=c(2,2,length(segs)))
initial_length<-data.frame()
for(ss in segs)
    {
    x<-runif(1000,30,1200)
    qntls<-seq(0,1,0.025)
    vals<-data.frame(segment=ss,qntls=qntls,vals=quantile(x,qntls))
    initial_length<- rbind(initial_length,vals)
    }



sim_pop<-reference_population(segs=segs,
    bends=bends,# BENDS DATAFRAME
    fish_density=init_dens, # FISH DENSITY PER RKM
    phi=phi,
    Linf=rep(1000,10),
    k = rep(0.2,10),
    vbgf_vcv=vbgf_vcv,
    initial_length=initial_length) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS

sim_pop_ref<-gsub(":", "_", Sys.time())
saveRDS(sim_pop,
        file=paste0("_output/sim_pop_version_",sim_pop_ref,".rds"))


        
        

## MAKE CATCH DATA REPLICATES FOR RANDOM DRAWS OF CATCHABILITY AND B0_SD
### PICK SAMLING TYPE
#samp_type="r"
samp_type="f"

### RUN
ptm<-proc.time()
nreps=1

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
        samp_type=samp_type,
        gears=c("GN14", "GN18", "GN41", "GN81",
            "MF", "OT16", "TLC1", "TLC2", "TN"),
        catchability=q_mean,
        B0_sd=B0_sd,
        effort=effort,
        deployments=rep(8,9),
        occasions=3,
        individual_meta=sim_pop$individual_meta)
    saveRDS(dat,
        file=paste0("_output/catch_dat_", sim_pop_ref, "_samp_type_",samp_type,"_catchability_random_rep_",gsub(":", "_", Sys.time()),".rds"))  
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
sim_dat<-readRDS("_output/catch_dat_catchability_random_rep2017-07-07 11_43_38.rds")
sim_dat$true_vals
head(sim_dat$samp_dat)
head(sim_dat$catch_dat)