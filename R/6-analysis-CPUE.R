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



################
#  FIXED GRID  #
################

# MEAN CATCHABILITY
q_mean<-rep(0.00001,9)

# CONSTRUCT MATRIX OF SDs  
gears<-c("GN14", "GN18", "GN41", "GN81",
         "MF", "OT16", "TLC1", "TLC2", "TN")
B0_sd<-seq(0,1.5, by=0.1)

# MAKE REPLICATES
nreps=2

for(count in 1:2)#length(sd))
{
  replicate(nreps,
            {
              dat<-samp_dat(sim_pop=sim_pop,
                            gears=gears,
                            catchability=q_mean,
                            q_sd=rep(B0_sd[count],length(gears)),
                            deployments=rep(8,9),
                            effort=effort,
                            occasions=3)
              saveRDS(dat,
                      file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/data_catchability",q_mean[1],"_sdrow",count,"_rep",gsub(":", "_", Sys.time()),".rds"))  
            })
}


## CPUE ANALYSIS
q<-1e-5
sd<-0.8

## PULL THE DATA
dat_files<-paste0("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid/", dir("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid", pattern=paste0(q,"_B0sd_",sd, "_")))

## FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
### INITIALIZE
get_trnd<-list() #TO HOLD CPUE TREND

### GET TREND FOR ALL REPLICATES
for(i in 1:length(dat_files))
{
  dat<-readRDS(dat_files[i])
  get_trnd[[i]]<-get.trnd(sim_dat=dat)
  get_trnd[[i]]<-do.call(rbind, get_trnd[[i]])
  get_trnd[[i]]<-merge(get_trnd,
                       aggregate(flag~gear,dat$cpue_long, sum),by="gear")
      # Merge flags by gear (sum includes any 500's for capping cp)
} 

trial<-lapply(1:length(dat_files), function(i)
  {
  dat<-readRDS(dat_files[i])
  get_trnd2<-get.trnd(sim_dat=dat)
  get_trnd2<-do.call(rbind, get_trnd2)
  get_trnd2<-merge(get_trnd2,
                       aggregate(flag~gear,dat$cpue_long, sum),by="gear")
  return(get_trnd2)
  })

### PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
get_trnd<-do.call(rbind, get_trnd)
get_trnd$sig<-ifelse(get_trnd$pval<0.05,1,0)
head(get_trnd)
dim(get_trnd)

write.csv(get_trnd, file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/cpue_reps_catchability_",q,"_B0sd_",sd, ".csv"))


##### MAKE A TABLE OF RESULTS
df<-ddply(get_trnd, .(gear), summarize,
      mean_trnd=mean(trnd),
      mean_se=mean(se),
      max_se=max(se),
      mean_pval=mean(pval),
      max_pval=max(pval),
      flags=sum(flag),
      power=sum(sig)/length(dat_files))

write.csv(df, file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/cpue_summary_catchability_",q,"_B0sd_",sd, ".csv"))

boxplot(trnd~gear,data=get_trnd)
boxplot(trnd~gear,data=subset(get_trnd, gear!="OT16" & gear!="TN"))


# PULL DF's
# RBIND
# OR CAN DO ABOVE IN A LOOP
# PLOT POWER VS. B0SD BY GEAR


#################
#  SD MAX GRID  #
#################

# CALCULATING SD_MAX ASSUMING KNOWN MEAN CATCHABILITY AND MEAN EFFORT
## MEAN CATCHABILITY
q_mean<-rep(0.00001,9)
B0=log(q_mean/(1-q_mean))

## MEAN EFFORT
tmp<-effort[which(effort$gear %in% c("GN14", "GN18", "GN41", "GN81", 
                                     "MF", "OT16", "TLC1", "TLC2", "TN")),]
mean_effort<-tmp$gamma_shape/tmp$gamma_rate
### TAKE AVERAGE FOR EACH BASIN
mean_effort_avg<-(mean_effort[1:9]+mean_effort[c(10,2,11,4,12:16)])/2

## LOOK AT 3 SD's of EFFORT TO FIND ROUGH EFFORT UPPER BOUND
three_sig<-3*sqrt(tmp$gamma_shape)/tmp$gamma_rate
UB<-mean_effort+three_sig
UB_avg<-(UB[1:9]+UB[c(10,2,11,4,12:16)])/2

## FIND ROUGH CATCHABILITY UPPER BOUND 
### WHEN SUMMING Q*F's TO GET P, ON AVERAGE WE WANT, FOR 8 DEPLOYMENTS,
### 8*Q*F<0.4 SO Q<0.4/(8*F)
UBq<-.4/(8*UB_avg)
### CHECK THAT 1/UBq-1>0
which(1/UBq-1<0)

## USE B0 and UBq TO CALCULATE ROUGH UPPER BOUND for SD
sd_max<-(log(1/UBq-1)+B0)/(-3)

# CONSTRUCT MATRIX OF SDs
gears<-c("GN14", "GN18", "GN41", "GN81",
         "MF", "OT16", "TLC1", "TLC2", "TN")

Nstep<-15
step<-sd_max/Nstep

B0_sd_matrix<-matrix(0,nrow=Nstep+1,ncol=length(gears))
for(i in 1:Nstep)
{
  B0_sd_matrix[i+1,]<-i*step
}

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
