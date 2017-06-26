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


# SIMULATE EFFORT & CATCH DATA FOR A FIXED B0_SD GRID
## MEAN CATCHABILITY
q_mean<-rep(0.00001,9)

## B0_SD GRID  
B0_sd<-seq(0,1.5, by=0.1)

## MAKE REPLICATES
ptm<-proc.time()
nreps=100

for(count in 1:length(sd))
{
  replicate(nreps,
            {
              dat<-samp_dat(sim_pop=sim_pop,
                            catchability=q_mean,
                            B0_sd=rep(B0_sd[count],length(gears)),
                            effort=effort,
                            occasions=3)
              saveRDS(dat,
                      file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/data_catchability",q_mean[1],"_sdrow",count,"_rep",gsub(":", "_", Sys.time()),".rds"))  
            })
}
proc.time()-ptm
#user       system    elapsed 
#15795.57   68.90     15988.57


## CPUE ANALYSIS
q<-1e-5
cpue_trnd<-lapply(B0_sd, function(x)
{
  ### PULL THE DATA
  dat_files<-paste0("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid/", dir("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid", pattern=paste0(q,"_B0sd_",x, "_")))

  ### FIND THE TREND IN CPUE AND CHECK FOR HIGH CAPTURE PROBABILITIES
  get_trnd<-lapply(1:length(dat_files), function(i)
  {
    dat<-readRDS(dat_files[i])
    out<-get.trnd(sim_dat=dat)
    out<-do.call(rbind, out)
    out<-merge(out, aggregate(flag~gear,dat$cpue_long, sum),by="gear")
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
      flags=sum(flag),
      power=sum(sig)/length(dat_files))
  df$q<-rep(q,nrow(df))
  df$B0_sd<-x

  ### OUTPUT THE TREND AND SUMMARY TABLE FOR EACH B0_SD
  return(list(trnd_dat=get_trnd, summary=df))
})


## SAVE TREND INFORMATION
saveRDS(cpue_trnd,file=paste0("C:/Users/sreynolds/Documents/GitHub/PSPAP-Reboot/output/cpue_trnd_catchability",q,"_B0sd_fixed_grid.rds"))

## LOOK AT POWER PLOTS
summary<-do.call(rbind, sapply(cpue_trnd, "[[", "summary", simplify=FALSE))
head(summary)

gears<-c("GN14", "GN18", "GN41", "GN81", "MF", "OT16", "TLC1", "TLC2", "TN")
par(mfrow=c(3,3))
for(j in 1:length(gears))
{
  plot(power~B0_sd, data=subset(summary, gear==gears[j]),main=paste(gears[j]))
}

## LOOK AT FLAGS
which(summary$flags!=0)


# CPUE ANALYSIS FOR TREND
dat_files<-paste0("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid/",
                  dir("C:/Users/sreynolds/Desktop/DataDump/sd_fixed_grid",
                      pattern=paste0(q,"_B0sd_",0.5, "_")))
dat<-readRDS(dat_files[1])
tmp<- aggregate(cpue~year+b_segment,dat$cpue_long,mean)
tmp$b_segment<- as.factor(tmp$b_segment)
tmp$lncpue<- log(tmp$cpue+1)

## PLOT CPUE OVER TIME FOR EACH SEGMENT
xyplot(cpue~year, tmp, group=b_segment,type='b')
xyplot(lncpue~year, tmp, group=b_segment,type='b')

head(dat$cpue_long)

plot(log(r_abund)~year,data=dat$cpue_long,type='n')
points(log(r_abund)~year,dat$cpue_long,subset=rpma==2)
points(log(r_abund)~year,dat$cpue_long,subset=rpma==4)

head(dat$sampled)



## TRUE TREND
fit<- lm(log(r_abund)~year+rpma,dat$cpue_long)
summary(fit)

### FIT LINEAR MODEL FOR TREND
fit2<- lm(lncpue~b_segment+year, tmp)
summary(fit2)


# LOG(CPUE+1) CHECK
N0<-colSums(sim_pop$out)[1]
dat<-readRDS(dat_files[1])
sum(subset(dat$cpue_long,year==1)$catch,na.rm=TRUE)/N0
alpha<-0.2
#alpha<-0.0000004
t<-c(1:10)
y<-log(alpha*N0*(0.95)^t+1)
lm(y~t)


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
                occasions=1)  
names(yyyyy)  

# yyyyy$cpue_long this can feed the beast below
head(yyyyy$cpue_long)

trnd_dat<-get.trnd(sim_dat=yyyyy,
                   gears=c("GN14", "GN18", "GN41", "GN81",
                           "MF", "OT16", "TLC1", "TLC2", "TN"))

trnd_dat<-do.call("rbind",trnd_dat)

# CPUE ANALYSIS FOR TREND
tmp<- aggregate(cpue~year+b_segment,yyyyy$cpue_long,mean)
tmp$b_segment<- as.factor(tmp$b_segment)
tmp$lncpue<- log(tmp$cpue+1)

## PLOT CPUE OVER TIME FOR EACH SEGMENT
xyplot(cpue~year, tmp, group=b_segment,type='b')
xyplot(lncpue~year, tmp, group=b_segment,type='b')

plot(log(r_abund)~year,data=yyyyy$cpue_long,type='n')
points(log(r_abund)~year,yyyyy$cpue_long,subset=rpma==2)
points(log(r_abund)~year,yyyyy$cpue_long,subset=rpma==4)

## TRUE TREND
fit<- lm(log(r_abund)~year+rpma,yyyyy$cpue_long)
summary(fit)

tmp2<- aggregate(s_abund~year+b_segment,yyyyy$cpue_long,mean)
tmp2$b_segment<- as.factor(tmp2$b_segment)
fit1<- lm(log(s_abund+1)~b_segment+year,tmp2)
summary(fit1)

### FIT LINEAR MODEL FOR TREND
fit2<- lm(lncpue~b_segment+year, tmp)
summary(fit2)



get_trnd$sig<-ifelse(get_trnd$pval<0.05,1,0)
ddply(trnd_dat, .(gear), summarize,
      mean_trnd=mean(trnd),
      mean_se=mean(se),
      max_se=max(se),
      mean_pval=mean(pval),
      max_pval=max(pval),
      flags=sum(flag),
      power=sum(sig)/length(dat_files))
