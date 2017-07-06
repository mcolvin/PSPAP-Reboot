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

saveRDS(sim_pop,
        file=paste0("output/sim_pop_version",gsub(":", "_", Sys.time()),".rds"))


# SIMULATE EFFORT & CATCH DATA FOR A FIXED B0_SD GRID
## MEAN CATCHABILITY
q_mean<-rep(0.00001,9)

## B0_SD GRID  
B0_sd<-seq(0,1.5, by=0.1)

## MAKE REPLICATES
ptm<-proc.time()
nreps=100

for(count in 1:length(B0_sd))
{
  replicate(nreps,
            {
              dat<-samp_dat(sim_pop=sim_pop,
                            catchability=q_mean,
                            B0_sd=rep(B0_sd[count],9),
                            effort=effort,
                            occasions=3)
              saveRDS(dat,
                      file=paste0("E:/DataDump/data_catchability_",q_mean[1],"_B0sd_",B0_sd[count],"_rep",gsub(":", "_", Sys.time()),".rds"))  
            })
}
proc.time()-ptm
#user       system    elapsed 
#15795.57   68.90     15988.57


## CPUE ANALYSIS
### ACTUAL TREND
sim_dat<-readRDS("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid/data_catchability_1e-05_B0sd_0_rep2017-06-26 23_25_06.rds")
tmp<- aggregate(s_abund~year+b_segment,sim_dat$cpue_long,mean)
tmp$b_segment<- as.factor(tmp$b_segment)
fit<- lm(log(s_abund)~b_segment+year,tmp)
trnd<-unname(coef(fit)['year'])

### CPUE TREND
q<-1e-5

ptm<-proc.time()
cpue_trnd<-lapply(B0_sd, function(x)
{
  ### PULL THE DATA
  dat_files<-paste0("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid/", dir("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid", pattern=paste0(q,"_B0sd_",x, "_")))
  
  ### FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
  get_trnd<-lapply(dat_files, function(i)
  {
    dat<-readRDS(i)
    out<-get.trnd(sim_dat=dat)
    out<-do.call(rbind, out)
    if(nrow(subset(dat$cpue_long, flag!=0))==0) {out$flag=0}
    if(nrow(subset(dat$cpue_long, flag!=0))!=0)
       {
          out<-merge(out,
                aggregate(flag~gear,subset(dat$cpue_long, flag!=0), length),
                by="gear",all.x=TRUE)
          out$flag<-ifelse(is.na(out$flag),0,out$flag)
        }
    return(out)
  })

  #### PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
  get_trnd<-do.call(rbind, get_trnd)
  get_trnd$sig<-ifelse(get_trnd$pval<0.05,1,0)

  ### MAKE A SUMMARY TABLE OF RESULTS
  df<-ddply(get_trnd, .(gear), summarize,
      mean_trnd=mean(trnd),
      mean_se=mean(se),
      max_se=max(se),
      mean_pval=mean(pval),
      max_pval=max(pval),
      mean_flags=mean(flag,na.rm=TRUE),
      power=sum(sig))
  df$power<-df$power/length(dat_files)
  df$bias<-df$mean_trnd-trnd
  df$q<-q
  df$B0_sd<-x
  
  ### OUTPUT THE TREND AND SUMMARY TABLE FOR EACH B0_SD
  return(list(trnd_dat=get_trnd, summary=df))
})


## SAVE TREND INFORMATION
saveRDS(cpue_trnd,file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/cpue_trnd_catchability",q,"_B0sd_fixed_grid.rds"))

proc.time()-ptm
#user       system    elapsed 
#1279.74    11.02     1325.69 
#1462.69    9.49      1541.73


## READ IN DATA
cpue_trnd<-readRDS(paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/cpue_trnd_catchability",q,"_B0sd_fixed_grid.rds"))


## LOOK AT POWER PLOTS
summary<-do.call(rbind, sapply(cpue_trnd, "[[", "summary", simplify=FALSE))
head(summary)

gears<-c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN")
par(mfrow=c(3,3))
for(j in 1:length(gears))
{
  plot(power~B0_sd, data=subset(summary, gear==gears[j]),main=paste(gears[j]))
}

for(j in 1:length(gears))
{
  plot(bias~B0_sd, data=subset(summary, gear==gears[j]),main=paste(gears[j]))
}

## LOOK AT FLAGS
summary[which(summary$mean_flags!=0),]




###########################
##  INDIVIDUAL FUNCTIONS ##
###########################

## Population Simulations

segs<- c(1,2,3,4,7,8,9,10,13,14)
nyears<- 10

beta0<- 2.9444
phi<-matrix(plogis(beta0),length(segs),nyears-1)

sim_pop<-reference_population(segs=segs,
                              bends=bends,# BENDS DATAFRAME
                              fish_density=init_dens, # FISH DENSITY PER RKM
                              phi=phi) # MATRIX OF YEAR TO YEAR AND SEGEMENT SPECIFIC SURVIVALS




yyy<-catch_counts(sim_pop=sim_pop,
                  gears=c("GN14", "GN18", "GN41", "GN81", 
                          "MF", "OT16", "TLC1", "TLC2", "TN"),
                  catchability=c(0.00002, 0.00004, 0.00002, 0.00004,
                                 0.00004, 0.0002, 0.00002, 0.00004,
                                 0.0002),
                  B0_sd=c(0.08, 0.1, 0.08, 0.1, 0.07, 1.2, 0.08, 0.1, 1.2),
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
                B0_sd=rep(0,9),
                deployments=rep(8,9),
                effort=effort,
                occasions=3)  
names(yyyyy)  

# yyyyy$cpue_long this can feed the beast below
head(yyyyy$cpue_long)

# CATCH +1
yyyyy$cpue_long$catch1<-ifelse(yyyyy$cpue_long$effort==0,0,yyyyy$cpue$catch+1)
yyyyy$cpue_long$cpue1<-yyyyy$cpue_long$catch1/yyyyy$cpue$effort

# CPUE ANALYSIS FOR TREND
tmp<- aggregate(cpue1~year+b_segment,subset(yyyyy$cpue_long,gear=="GN14"),mean)
tmp$b_segment<- as.factor(tmp$b_segment)
tmp$lncpue1<- log(tmp$cpue1)

## PLOT CPUE OVER TIME FOR EACH SEGMENT
xyplot(cpue1~year, tmp, group=b_segment,type='b')
xyplot(lncpue1~year, tmp, group=b_segment,type='b')

plot(log(r_abund)~year,data=yyyyy$cpue_long,type='n')
points(log(r_abund)~year,yyyyy$cpue_long,subset=rpma==2)
points(log(r_abund)~year,yyyyy$cpue_long,subset=rpma==4)

## TRUE TREND
fit<- lm(log(r_abund)~year+rpma,yyyyy$cpue_long)
summary(fit)

tmp2<- aggregate(s_abund~year+b_segment,yyyyy$cpue_long,mean)
tmp2$b_segment<- as.factor(tmp2$b_segment)
fit2<- lm(log(s_abund)~b_segment+year,tmp2)
summary(fit2)

### FIT LINEAR MODEL FOR TREND
fit1<- lm(lncpue1~b_segment+year, tmp)
summary(fit1)


### USE GET.TRND
trnd_dat<-get.trnd(sim_dat=yyyyy,
                   gears=c("GN14", "GN18", "GN41", "GN81",
                           "MF", "OT16", "TLC1", "TLC2", "TN"))

trnd_dat<-do.call("rbind",trnd_dat)
trnd_dat$sig<-ifelse(trnd_dat$pval<0.05,1,0)
trnd_dat


#### REPLICATES
nreps=100
replicate(nreps,
          {
            dat<-samp_dat(sim_pop=sim_pop,
                          B0_sd=rep(0,9),
                          effort=effort,
                          occasions=3)
            saveRDS(dat,
                    file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/data_default_q_B0sd_0_rep",gsub(":", "_", Sys.time()),".rds"))  
          })

#### PULL THE DATA
  dat_files<-paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/", dir("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output", pattern="default_q"))
  
  ### FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
  get_trnd<-lapply(1:length(dat_files), function(i)
  {
    dat<-readRDS(dat_files[i])
    out<-get.trnd(sim_dat=dat)
    out<-do.call(rbind, out)
    out<-merge(out, aggregate(flag~gear,subset(dat$cpue_long, flag!=0), length),by="gear",all.x=TRUE)
    return(out)
  })
  
  #### PUT IN A PALLATABLE FORM AND ADD SIGNIFICANCE CHECK
  get_trnd<-do.call(rbind, get_trnd)
  get_trnd$sig<-ifelse(get_trnd$pval<0.05,1,0)
  get_trnd[1:20,]
  
  ### MAKE A SUMMARY TABLE OF RESULTS
  df<-ddply(get_trnd, .(gear), summarize,
            mean_trnd=mean(trnd),
            mean_se=mean(se),
            max_se=max(se),
            mean_pval=mean(pval),
            max_pval=max(pval),
            mean_flags=mean(flag, na.rm=TRUE),
            power=sum(sig)/length(dat_files))
  df$mean_flags<-ifelse(df$mean_flags=="NaN",0,df$mean_flags)
  df

