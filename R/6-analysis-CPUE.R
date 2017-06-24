#####################
## SIMULATING DATA ##
#####################

# GENERATE THE REFERENCE POPULATION
segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=init_dens, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS



# CONSTRUCT MATRIX OF SDs
gears<-c("GN14", "GN18", "GN41", "GN81",
         "MF", "OT16", "TLC1", "TLC2", "TN")

sd_max<-c(1:9)
Nstep<-50
step<-sd_max/Nstep

B0_sd_matrix<-matrix(0,nrow=Nstep+1,ncol=length(gears))
for(i in 1:Nstep)
{
  B0_sd_matrix[i+1,]<-i*step
}

# CONSTRUCT MEAN CATCHABILITY
q_mean<-rep(0.00001,length(gears))

# MAKE REPLICATES
nreps=2

for(count in 1:nrow(B0_sd_matrix))
  {
    replicate(nreps,
              {
                dat<-samp_dat(sim_pop=sim_pop,
                              gears=gears,
                              catchability=q_mean,
                              q_sd=B0_sd_matrix[count,],
                              deployments=rep(8,9),
                              effort=effort,
                              occasions=3)
                saveRDS(dat,
                        file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/data_catchability",q_mean[1],"_sdrow",count,"_rep",gsub(":", "_", Sys.time()),".rds"))  
              })
  }


## BE SURE WE CAN LOAD AND SORT THESE
dir("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output", pattern="sdrow2_")
check<-readRDS("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/data_catchability1e-05_sdrow2_rep2017-06-23 00_20_01.rds")



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

trnd_dat<-get.trnd(sim_dat=yyyyy,
                   gears=c("GN14", "GN18", "GN41", "GN81",
                           "MF", "OT16", "TLC1", "TLC2", "TN"))

trnd_dat<-do.call("rbind",trnd_dat)
