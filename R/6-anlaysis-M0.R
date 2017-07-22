# M0 ANALYSIS:
## 1. M0 ESTIMATES: TREND & ABUNDANCE
### BIAS & PRECISION
## 2. GET.M0.ESTS FUNCTION TEST

source("R/1_global.R")
source("R/2_functions.R")
source("R/3_load-and-clean.R")

rm(effort, effort_data,studyArea,dfitfun,dfitfunLB,dfitfunUB,estimate, plot_metrics,sim_ch)


# ## RUN COMMENTED IF ONLY RESULTS FROM A PARTICULAR REFERENCE
# ## POPULATION ARE DESIRED
# ### READ IN ALL AVAILABLE REFERENCE POPULATIONS
# pop_list<-dir("output", pattern="sim_pop_version_")
# 
# ### SELECT A REFERENCE POPULATION
# item<-2
# pop_ref<-strsplit(pop_list[item],"version_")[[1]][2]
# pop_ref<-strsplit(pop_ref, ".", fixed=TRUE)[[1]][1]
# 
# ### PULL THE CATCH DATA ASSOCIATED WITH THE REFERENCE POPULATION
# dat_files<-dir("output", pattern=paste0("catch_dat_",pop_ref))



## PULL CATCH DATA
dat_files<-dir("output", pattern="catch_dat_")


#####################
## 1. M0 ESTIMATES ##
#####################

# FIND M0 ESTIMATES FOR TREND AND ABUNDANCE
get_ests<-lapply(dat_files, function(i)
{
  # TREND & ABUNDANCE
  dat<-readRDS(paste0("output/",i))
  out<-get.M0.ests(sim_dat=dat,bends=bends)
  return(out)
})

# TREND
## PUT IN A PALLATABLE FORM
get_M0_trnd<-do.call(rbind, sapply(get_ests, "[[", "M0_trnd", simplify=FALSE))
get_M0_trnd$sig_AM<-ifelse(get_M0_trnd$pval_AM<0.05,1,0)
get_M0_trnd$sig_HM<-ifelse(get_M0_trnd$pval_HM<0.05,1,0)

## MAKE SUMMARY TABLE OF RESULTS
df_trnd<-ddply(get_M0_trnd, .(gear), summarize,
               mean_trnd_AM=mean(trnd_AM, na.rm=TRUE),
               mean_pval_AM=mean(pval_AM, na.rm=TRUE),
               mean_bias_AM=mean(bias_AM, na.rm=TRUE),
               mean_cv_AM=mean(cv_AM, na.rm=TRUE),
               power_AM=sum(sig_AM,na.rm=TRUE)/length(which(!is.na(sig_AM))),
               mean_trnd_HM=mean(trnd_HM, na.rm=TRUE),
               mean_pval_HM=mean(pval_HM, na.rm=TRUE),
               mean_bias_HM=mean(bias_HM, na.rm=TRUE),
               mean_cv_HM=mean(cv_HM, na.rm=TRUE),
               power_HM=sum(sig_HM,na.rm=TRUE)/length(which(!is.na(sig_HM))),
               mean_performance=mean(perform))

# STORE AND SAVE TREND INFORMATION
M0_trnd<-list(trnd_dat=get_M0_trnd, summary=df_trnd)
saveRDS(M0_trnd,file=paste0("output/M0_trnd_catchability_random_",
                              gsub(":", "_", Sys.time()),".rds"))

# ### SAVE FOR A PARTICULAR REFERENC POPULATION 
# saveRDS(M0_trnd,file=paste0("output/M0_trnd_",pop_ref,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))



# ABUNDANCE
## PUT IN A PALLATABLE FORM
get_M0_abund<-do.call(rbind, sapply(get_ests, "[[", "M0_abund", simplify=FALSE))

# MAKE SUMMARY TABLE OF RESULTS
df_abund<-ddply(get_M0_abund, .(segment,year,gear), summarize,
                mean_Nhat_AM=mean(Nhat_AM, na.rm=TRUE),
                mean_bias_AM=mean(abund_bias_AM, na.rm=TRUE),
                mean_cv_AM=mean(abund_cv_AM, na.rm=TRUE),
                mean_Nhat_HM=mean(Nhat_HM, na.rm=TRUE),
                mean_bias_HM=mean(abund_bias_HM, na.rm=TRUE),
                mean_cv_HM=mean(abund_cv_HM, na.rm=TRUE),
                mean_perform=mean(perform, na.rm=TRUE))

# STORE AND SAVE ABUNDANCE INFORMATION
M0_abund<-list(abund_dat=get_M0_abund, summary=df_abund)
saveRDS(M0_abund,file=paste0("output/M0_abund_catchability_random_",
                               gsub(":", "_", Sys.time()),".rds"))

# ### SAVE FOR A PARTICULAR REFERENC POPULATION 
# saveRDS(M0_abund,file=paste0("output/M0_abund_",pop_ref,"_catchability_random_",
#                               gsub(":", "_", Sys.time()),".rds"))



###########################
##  2. TEST GET.M0.ESTS  ##
###########################

## PULL ONE REPLICATE OF CATCH DATA
dat_files<-dir("output", pattern="catch_dat_")
sim_dat<-readRDS(paste0("output/",dat_files[2]))

## TEST FUNCTION
ptm<-proc.time()
yy<-get.M0.ests(sim_dat=sim_dat, bends=bends)
proc.time()-ptm
# user      system    elapsed 
# 817.50    36.12     864.53 


